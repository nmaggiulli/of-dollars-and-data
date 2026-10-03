<?php
/**
 * Calculator pages.
 *
 * Same page IDs, libraries and versions as the old theme. The calculator
 * scripts themselves are unchanged copies, now in /calculators/.
 * To add a calculator: drop its .js file in /calculators/ and add a line below.
 */

function odad_calculators() {
	// page ID => array( script file, needs Chart.js?, needs Moment.js? )
	return array(
		23470 => array( 'sp500_calculator.js',             false, true ),
		23619 => array( 'sp500_dca_calculator.js',         true,  true ),
		23693 => array( 'us_stock_bond_calculator.js',     true,  true ),
		23727 => array( 'investment_return_calculator.js', true,  false ),
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

	list( $file, $needs_chart, $needs_moment ) = $calculators[ $page_id ];

	// The old theme loaded Moment.js on every calculator page, so keep doing that.
	wp_enqueue_script( 'momentjs', 'https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js', array(), '2.29.1', true );
	$deps = array( 'momentjs' );

	if ( $needs_chart ) {
		wp_enqueue_script( 'chartjs', 'https://cdn.jsdelivr.net/npm/chart.js@2.9.4', array( 'momentjs' ), '2.9.4', true );
		$deps = $needs_moment ? array( 'momentjs', 'chartjs' ) : array( 'chartjs' );
	}

	$handle = sanitize_key( str_replace( '.js', '', $file ) );
	wp_enqueue_script(
		$handle,
		get_template_directory_uri() . '/calculators/' . $file,
		$deps,
		filemtime( get_template_directory() . '/calculators/' . $file ),
		true
	);
} );

/** Adds a "calculator-page" body class in case your CSS wants to target these pages. */
add_filter( 'body_class', function ( $classes ) {
	if ( is_page() && array_key_exists( get_queried_object_id(), odad_calculators() ) ) {
		$classes[] = 'calculator-page';
	}
	return $classes;
} );
