<?php
/**
 * Calculator pages.
 *
 * Same page IDs as the old theme; scripts live in
 * /calculators/. To add a calculator: drop its .js file there and add a line below.
 *
 * The three S&P 500 calculators (returns, DCA, stock/bond) share one data file,
 * sp500_data.js, written by the R script. It is the only file to replace for a
 * monthly update; the calculator code never changes.
 */

function odad_calculators() {
	// page ID => array( script file, needs Chart.js?, data file (optional) )
	return array(
		23470 => array( 'sp500_calculator.js',             false, 'sp500_data.js' ),
		23619 => array( 'sp500_dca_calculator.js',         true,  'sp500_data.js' ),
		23693 => array( 'us_stock_bond_calculator.js',     true,  'sp500_data.js' ),
		23727 => array( 'investment_return_calculator.js', true ),
		24987 => array( 'net_worth_by_age_calculator.js',  true,  'net_worth_data.js' ),
		25023 => array( 'income_by_age_calculator.js',     true,  'income_data.js' ),
		27697 => array( 'rent_vs_buy_calculator.js',       true ),
	);
}

add_action( 'wp_enqueue_scripts', function () {
	if ( ! is_page() ) {
		return;
	}

	$calculators = odad_calculators();
	$page_id     = get_queried_object_id();

	if ( empty( $calculators[ $page_id ] ) ) {
		return;
	}

	$calc        = $calculators[ $page_id ];
	$file        = $calc[0];
	$needs_chart = $calc[1];
	$data_file   = isset( $calc[2] ) ? $calc[2] : '';
	$dir         = get_template_directory() . '/calculators/';
	$uri         = get_template_directory_uri() . '/calculators/';
	$deps        = array();

	// Chart.js 2.9.4, hosted in the theme (no outside CDN, no Moment.js needed).
	if ( $needs_chart ) {
		wp_enqueue_script( 'chartjs', get_template_directory_uri() . '/assets/js/vendor/chart-2.9.4.min.js', array(), '2.9.4', true );
		$deps[] = 'chartjs';
	}

	// Data kept in its own file (written by R), loaded before the calculator code.
	if ( $data_file && file_exists( $dir . $data_file ) ) {
		$data_handle = sanitize_key( str_replace( '.js', '', $data_file ) );
		wp_enqueue_script( $data_handle, $uri . $data_file, array(), filemtime( $dir . $data_file ), true );
		$deps[] = $data_handle;
	}

	// Calculator styles (S&P 500 results table, error messages, bigger date menus on phones).
	$css = get_template_directory() . '/assets/css/calculators.css';
	if ( file_exists( $css ) ) {
		wp_enqueue_style( 'odad-calculators', get_template_directory_uri() . '/assets/css/calculators.css', array( 'odad-custom' ), filemtime( $css ) );
	}

	$handle = sanitize_key( str_replace( '.js', '', $file ) );
	wp_enqueue_script( $handle, $uri . $file, $deps, filemtime( $dir . $file ), true );
} );

/** Adds a "calculator-page" body class in case your CSS wants to target these pages. */
add_filter( 'body_class', function ( $classes ) {
	if ( is_page() && array_key_exists( get_queried_object_id(), odad_calculators() ) ) {
		$classes[] = 'calculator-page';
	}
	return $classes;
} );

/*
 * Calculator shortcodes. Put the one line on each page in place of the old
 * form/results HTML:
 *   S&P 500 calculator        [sp500_calculator]
 *   S&P 500 DCA calculator    [sp500_dca_calculator]
 *   Stock/bond calculator     [stock_bond_calculator]
 *   Net worth by age          [net_worth_calculator]
 *   Income by age             [income_calculator]
 *   Rent vs. buy              [rent_vs_buy_calculator]
 *   Investment return         [investment_return_calculator]
 *
 * Month/year lists, default dates and the "data through" line are filled in
 * from sp500_data.js, so this HTML never changes when the data is updated.
 */
function odad_calc_dates() {
	$field = function ( $id, $label ) {
		return '<div class="date-field"><label for="' . $id . '-month">' . $label . '</label>'
			. '<div class="date-selector">'
			. '<select id="' . $id . '-month" aria-label="' . $label . ' month"></select>'
			. '<select id="' . $id . '-year" aria-label="' . $label . ' year"></select>'
			. '</div></div>';
	};
	return '<div class="date-container">' . $field( 'start', 'Start Month:' ) . $field( 'end', 'End Month:' ) . '</div>'
		. '<p id="calc-error" class="calc-error" role="alert" hidden></p>';
}

function odad_calc_money_input( $id, $label, $value ) {
	return '<label for="' . $id . '">' . $label . '</label>'
		. '<input type="text" id="' . $id . '" inputmode="decimal" value="' . $value . '" oninput="formatInputNumber(this)">';
}

function odad_calc_footer() {
	return '<button type="button" id="calculate-btn">Calculate</button>'
		. '<p id="data-through" class="data-through"></p>';
}

function odad_calc_chart() {
	return '<div class="calc-chart"><hr><div id="chart-container"><canvas id="myChart" width="400" height="200"></canvas></div></div>';
}

add_shortcode( 'sp500_calculator', function () {
	return '<div class="calculator odad-calc sp500-calc">'
		. odad_calc_dates()
		. '<hr>'
		. '<div class="initial-investment">' . odad_calc_money_input( 'initialInvestment', 'Initial Investment:', '10,000' ) . '</div>'
		. odad_calc_footer()
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Nominal Price Return:</strong> <span id="nominal-price-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-price-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-price-dollar"></span></p>'
		. '<p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nominal-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-total-dollar"></span></p>'
		. '<hr>'
		. '<p><strong>Inflation-Adjusted Price Return:</strong> <span id="real-price-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-real-price-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-price-dollar"></span></p>'
		. '<p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-real-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-total-dollar"></span></p>'
		. '<hr><p class="history-rank"><strong>Compared to History:</strong> <span id="history-rank"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. '</div>';
} );

add_shortcode( 'sp500_dca_calculator', function () {
	return '<div class="calculator odad-calc dca-calc">'
		. odad_calc_dates()
		. '<hr>'
		. '<div class="investment-amounts">'
		. odad_calc_money_input( 'initial-investment', 'Initial Investment:', '10,000' )
		. odad_calc_money_input( 'monthly-investment', 'Monthly Investment:', '1,000' )
		. '</div>'
		. '<div class="inflation-checkbox"><label for="adjust-for-inflation">Adjust Monthly Investments for Inflation?</label>'
		. '<input type="checkbox" id="adjust-for-inflation"></div>'
		. odad_calc_footer()
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Total Nominal Contributions (Initial + Monthly): </strong><span id="total-contributions"></span></p>'
		. '<p><strong>Final Nominal Value (with dividends reinvested): </strong><span id="final-value-nominal-dollars"></span></p>'
		. '<p class="indented"><strong>IRR (Nominal): </strong><span id="nom_irr"></span></p>'
		. '<p><strong>Final Inflation-Adjusted Value (with dividends reinvested): </strong><span id="final-value-real-dollars"></span></p>'
		. '<p class="indented"><strong>IRR (Inflation-Adjusted): </strong><span id="real_irr"></span></p>'
		. '<hr><p class="history-rank"><strong>Compared to History:</strong> <span id="history-rank"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. odad_calc_chart()
		. '</div><hr>';
} );

add_shortcode( 'stock_bond_calculator', function () {
	return '<div class="calculator odad-calc stock-bond-calc">'
		. odad_calc_dates()
		. '<hr>'
		. '<div class="investment-amounts">'
		. odad_calc_money_input( 'initial-investment', 'Initial Investment:', '10,000' )
		. '<label for="percentage-in-stocks">Percentage in Stocks:</label>'
		. '<input type="number" id="percentage-in-stocks" min="0" max="100" value="60" inputmode="numeric">'
		. '</div>'
		. odad_calc_footer()
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nom-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="nom-annualized"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nom-total"></span></p>'
		. '<hr>'
		. '<p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="real-annualized"></span><span class="calc-unit">%</span></p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-total"></span></p>'
		. '<hr><p class="history-rank"><strong>Compared to History:</strong> <span id="history-rank"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. odad_calc_chart()
		. '</div><hr>';
} );

add_shortcode( 'net_worth_calculator', function () {
	return '<div class="calculator odad-calc nw-calc">'
		. '<div class="inputs">'
		. '<label for="nw-age">Your Age Group:</label>'
		. '<select id="nw-age" name="nw-age"><option value="">Select age group</option></select>'
		. '<label for="net-worth">Household Net Worth:</label>'
		. '<input type="text" id="net-worth" name="net-worth" placeholder="Enter your household net worth" oninput="formatInputNumber(this)">'
		. '</div>'
		. '<p id="calc-error" class="calc-error" role="alert" hidden></p>'
		. '<button type="button" id="calculate-btn">Calculate Percentile</button>'
		. '<p id="data-through" class="data-through"></p>'
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Your Net Worth Percentile: </strong><span id="nw-percentile"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. odad_calc_chart()
		. '</div><hr>';
} );

add_shortcode( 'income_calculator', function () {
	return '<div class="calculator odad-calc income-calc">'
		. '<div class="inputs">'
		. '<label for="age">Your Age Group:</label>'
		. '<select id="age" name="age"><option value="">Select age group</option></select>'
		. '<label for="income">Household Income:</label>'
		. '<input type="text" id="income" name="income" inputmode="decimal" placeholder="Enter your household income" oninput="formatInputNumber(this)">'
		. '</div>'
		. '<p id="calc-error" class="calc-error" role="alert" hidden></p>'
		. '<button type="button" id="calculate-btn">Calculate Percentile</button>'
		. '<p id="data-through" class="data-through"></p>'
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Your Income Percentile: </strong><span id="income-percentile"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. odad_calc_chart()
		. '</div><hr>';
} );

/*
 * Rent vs. buy: same markup and inline styles as the original page HTML, so it
 * looks the same on desktop. Results update automatically as you type.
 */
add_shortcode( 'rent_vs_buy_calculator', function () {
	$field = function ( $id, $label, $value ) {
		return '<div class="input-group"><label for="' . $id . '">' . $label . '</label>'
			. '<input type="text" id="' . $id . '" value="' . $value . '"></div>';
	};
	$col = '<div class="rvb-col" style="flex: 1 1 auto; min-width: 200px; width: calc(33.333% - 14px);">';

	return '<div class="calculator rvb-calc">'
		. '<div class="rvb-box" style="border: 4px solid #333; border-radius: 4px; padding: 25px 25px 15px 25px; margin-bottom: 0px; background-color: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.1);">'
		. '<div class="calculator-grid-bvr" style="display: flex; flex-wrap: wrap; width: 100%;">'
		. $col . $field( 'monthlyRent', 'Monthly Rent:', '$2,500' ) . $field( 'inflation', 'Future Inflation:', '3.00%' ) . $field( 'portfolioGrowth', 'Portfolio Growth:', '6.00%' ) . '</div>'
		. $col . $field( 'homePrice', 'Home Price:', '$500,000' ) . $field( 'downpayment', 'Downpayment:', '20%' ) . $field( 'interestRate', 'Interest Rate:', '7.00%' ) . '</div>'
		. $col . $field( 'propertyTax', 'Property Tax:', '1.00%' ) . $field( 'maintenance', 'HOA/Maintenance:', '1.00%' ) . $field( 'insurance', 'Insurance:', '0.50%' ) . '</div>'
		. '</div>'
		. '<p id="calc-error" class="calc-error" role="alert" hidden></p>'
		. '</div>'
		. '<div class="results">'
		. '<div class="rvb-results-row" style="display: flex; justify-content: space-between;">'
		. '<div><h3>Monthly Housing Cost</h3>'
		. '<p>Total: <span id="totalMonthlyCost">$0</span></p>'
		. '<p>Mortgage Payment: <span id="monthlyMortgage">$0</span></p>'
		. '<p>Property Tax: <span id="monthlyPropertyTax">$0</span></p>'
		. '<p>Maintenance/HOA: <span id="monthlyMaintenance">$0</span></p>'
		. '<p>Insurance: <span id="monthlyInsurance">$0</span></p></div>'
		. '<div><h3>Final Decision</h3>'
		. '<div class="decision" id="finalDecision" style="text-align: left; margin: 30px 0; font-size: 32px; font-weight: bold; letter-spacing: 1px;">-</div>'
		. '<p>Final Portfolio Value: <span id="portfolioValue">$0</span></p>'
		. '<p>Final Home Value: <span id="finalHomeValue">$0</span></p></div>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. '</div>'
		. '<div class="chart-container" style="border: 1px solid #eee; padding: 20px; border-radius: 4px; background-color: #fff; margin-top: 20px; border-top: 4px solid #333;">'
		. '<h3 style="margin-bottom: 20px; margin-top: 10px;">Renting vs. Buying Over Time</h3>'
		. '<div class="rvb-chart-box" style="position: relative; height: 400px;"><canvas id="valueChart"></canvas></div>'
		. '</div>'
		. '</div>';
} );

add_shortcode( 'investment_return_calculator', function () {
	return '<div class="calculator odad-calc inv-calc">'
		. '<div class="age-amounts">'
		. '<div class="ages">'
		. '<label for="current-age">Current Age:</label>'
		. '<input type="number" id="current-age" name="current-age" value="30" inputmode="numeric">'
		. '<label for="retirement-age">Retirement Age:</label>'
		. '<input type="number" id="retirement-age" name="retirement-age" value="65" inputmode="numeric">'
		. '</div>'
		. '<div class="amounts">'
		. odad_calc_money_input( 'current-amount', 'Current Amount Invested:', '10,000' )
		. odad_calc_money_input( 'monthly-contributions', 'Monthly Contributions:', '1,000' )
		. '</div>'
		. '</div>'
		. '<div class="expected-return">'
		. '<label for="expected-return">Expected Annual Return (%):</label>'
		. '<input type="number" id="expected-return" name="expected-return" step="any" inputmode="decimal">'
		. '</div>'
		. '<p id="calc-error" class="calc-error" role="alert" hidden></p>'
		. '<button type="button" id="calculate-btn">Calculate</button>'
		. '</div>'
		. '<div id="calc-output" class="is-empty">'
		. '<div class="results">'
		. '<p><strong>Total Contributions: </strong><span id="total-contributions"></span></p>'
		. '<p><strong>Estimated Final Amount: </strong><span id="final-amount"></span></p>'
		. '</div>'
		. '<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>'
		. '<span class="sp500-copied" aria-live="polite"></span></p>'
		. odad_calc_chart()
		. '</div><hr>';
} );
