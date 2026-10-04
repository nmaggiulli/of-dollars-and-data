<?php
/**
 * Calculator pages.
 *
 * Same page IDs and library versions as the old theme; scripts live in
 * /calculators/. To add a calculator: drop its .js file there and add a line below.
 *
 * The three S&P 500 calculators (returns, DCA, stock/bond) share one data file,
 * sp500_data.js, written by the R script. It is the only file to replace for a
 * monthly update; the calculator code never changes.
 */

function odad_calculators() {
	// page ID => array( script file, needs Chart.js?, needs Moment.js?, data file (optional) )
	return array(
		23470 => array( 'sp500_calculator.js',             false, false, 'sp500_data.js' ),
		23619 => array( 'sp500_dca_calculator.js',         true,  true,  'sp500_data.js' ),
		23693 => array( 'us_stock_bond_calculator.js',     true,  true,  'sp500_data.js' ),
		23727 => array( 'investment_return_calculator.js', true,  true ),
		24987 => array( 'net_worth_by_age_calculator.js',  true,  true ),
		25023 => array( 'income_by_age_calculator.js',     true,  true ),
		27697 => array( 'rent_vs_buy_calculator.js',       true,  true ),
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

	$calc         = $calculators[ $page_id ];
	$file         = $calc[0];
	$needs_chart  = $calc[1];
	$needs_moment = $calc[2];
	$data_file    = isset( $calc[3] ) ? $calc[3] : '';
	$dir          = get_template_directory() . '/calculators/';
	$uri          = get_template_directory_uri() . '/calculators/';
	$deps         = array();

	if ( $needs_moment ) {
		wp_enqueue_script( 'momentjs', 'https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js', array(), '2.29.1', true );
		$deps[] = 'momentjs';
	}

	if ( $needs_chart ) {
		wp_enqueue_script( 'chartjs', 'https://cdn.jsdelivr.net/npm/chart.js@2.9.4', $needs_moment ? array( 'momentjs' ) : array(), '2.9.4', true );
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
	return '<hr><div id="chart-container"><canvas id="myChart" width="400" height="200"></canvas></div>';
}

add_shortcode( 'sp500_calculator', function () {
	return '<div class="calculator odad-calc sp500-calc">'
		. odad_calc_dates()
		. '<hr>'
		. '<div class="initial-investment">' . odad_calc_money_input( 'initialInvestment', 'Initial Investment:', '10,000' ) . '</div>'
		. odad_calc_footer()
		. '</div>'
		. '<div id="calc-output" hidden>'
		. '<div class="results">'
		. '<p><strong>Nominal Price Return:</strong> <span id="nominal-price-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-price-return"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-price-dollar"></span></p>'
		. '<p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nominal-total-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-total-return"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-total-dollar"></span></p>'
		. '<hr>'
		. '<p><strong>Inflation-Adjusted Price Return:</strong> <span id="real-price-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-real-price-return"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-price-dollar"></span></p>'
		. '<p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="annualized-real-total-return"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-total-dollar"></span></p>'
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
		. '<div id="calc-output" hidden>'
		. '<div class="results">'
		. '<p><strong>Total Nominal Contributions (Initial + Monthly): </strong><span id="total-contributions"></span></p>'
		. '<p><strong>Final Nominal Value (with dividends reinvested): </strong><span id="final-value-nominal-dollars"></span></p>'
		. '<p class="indented"><strong>IRR (Nominal): </strong><span id="nom_irr"></span></p>'
		. '<p><strong>Final Inflation-Adjusted Value (with dividends reinvested): </strong><span id="final-value-real-dollars"></span></p>'
		. '<p class="indented"><strong>IRR (Inflation-Adjusted): </strong><span id="real_irr"></span></p>'
		. '</div>'
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
		. '<div id="calc-output" hidden>'
		. '<div class="results">'
		. '<p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nom-total-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="nom-annualized"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="nom-total"></span></p>'
		. '<hr>'
		. '<p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span>%</p>'
		. '<p class="indented"><strong>Annualized:</strong> <span id="real-annualized"></span>%</p>'
		. '<p class="indented"><strong>Investment Grew To:</strong> <span id="real-total"></span></p>'
		. '</div>'
		. odad_calc_chart()
		. '</div><hr>';
} );
