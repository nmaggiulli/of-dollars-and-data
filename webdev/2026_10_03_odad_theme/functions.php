<?php
/**
 * Of Dollars And Data theme.
 *
 * A clean rebuild of the 2018 Aytoo/Bones theme that looks identical to it.
 * The templates and page structure are ported from the old theme and it uses the
 * old theme's own stylesheet, so nothing visual changes. What's gone is the
 * baggage: Kirki, the bundled-plugin installer, the 2018 jQuery bundle, the
 * leftover ad-server script and the debug files.
 *
 * Same menu locations, sidebar ID and ACF fields as before, so menus, widgets
 * and settings carry over when you switch.
 */

define( 'ODAD_VERSION', '2.0.0' );

require_once get_template_directory() . '/inc/helpers.php';
require_once get_template_directory() . '/inc/legacy.php';
require_once get_template_directory() . '/inc/feed.php';
require_once get_template_directory() . '/inc/calculators.php';
require_once get_template_directory() . '/inc/analytics.php';

/* ---------------------------------------------------------------
 * Theme setup (same features the old theme turned on)
 * ------------------------------------------------------------- */
add_action( 'after_setup_theme', function () {
	add_theme_support( 'title-tag' );
	add_theme_support( 'automatic-feed-links' );
	add_theme_support( 'post-thumbnails' );
	set_post_thumbnail_size( 125, 125, true );
	add_theme_support( 'post-formats', array( 'gallery', 'link', 'quote', 'video' ) );
	add_theme_support( 'html5', array( 'script', 'style' ) );

	// Same slugs as the old theme, so WordPress reassigns your menus automatically.
	register_nav_menus( array(
		'main-nav'     => 'The Main Menu',
		'footer-links' => 'Footer Links',
	) );
} );

// Same sidebar ID as the old theme, so your widgets move over automatically.
add_action( 'widgets_init', function () {
	register_sidebar( array(
		'id'            => 'sidebar1',
		'name'          => 'Sidebar 1',
		'description'   => 'The first (primary) sidebar.',
		'before_widget' => '<div id="%1$s" class="widget %2$s">',
		'after_widget'  => '</div>',
		'before_title'  => '<h4 class="widgettitle">',
		'after_title'   => '</h4>',
	) );
} );

/* ---------------------------------------------------------------
 * Advanced Custom Fields
 * The field definitions (header image, author box, disclaimer, contact page)
 * live in /acf-json, copied from the old theme. ACF loads them from the
 * active theme, so they have to travel with it.
 * ------------------------------------------------------------- */
add_action( 'acf/init', function () {
	if ( function_exists( 'acf_add_options_page' ) ) {
		acf_add_options_page(); // the "Options" screen in wp-admin, same as before
	}
} );

/* ---------------------------------------------------------------
 * Styles and scripts
 *
 * Load order matches the old theme exactly:
 *   1. legacy.css      – the old theme's compiled stylesheet, unchanged
 *   2. customizer.css  – the colors/fonts the old Customizer (Kirki) printed
 *   3. custom.css      – your Additional CSS
 * ------------------------------------------------------------- */
add_action( 'wp_enqueue_scripts', function () {
	$uri = get_template_directory_uri();
	$dir = get_template_directory();
	$ver = function ( $file ) use ( $dir ) {
		return file_exists( $dir . $file ) ? filemtime( $dir . $file ) : ODAD_VERSION;
	};

	// Libre Baskerville, regular weight only — exactly what the old theme loaded.
	wp_enqueue_style( 'odad-fonts', 'https://fonts.googleapis.com/css2?family=Libre+Baskerville&display=swap', array(), null );

	// Font Awesome 4.0.3 (same version as before, from a maintained CDN). Used for
	// the menu button, share icons, "Read More" arrow and pagination arrows.
	wp_enqueue_style( 'font-awesome', 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.0.3/css/font-awesome.min.css', array(), '4.0.3' );

	wp_enqueue_style( 'odad-legacy', $uri . '/assets/css/legacy.css', array( 'odad-fonts', 'font-awesome' ), $ver( '/assets/css/legacy.css' ) );
	wp_enqueue_style( 'odad-customizer', $uri . '/assets/css/customizer.css', array( 'odad-legacy' ), $ver( '/assets/css/customizer.css' ) );
	wp_enqueue_style( 'odad-custom', $uri . '/assets/css/custom.css', array( 'odad-customizer' ), $ver( '/assets/css/custom.css' ) );

	// Sticky menu, mobile menu toggle and share pop-ups (replaces the 170 KB jQuery bundle).
	wp_enqueue_script( 'odad-theme', $uri . '/assets/js/theme.js', array(), $ver( '/assets/js/theme.js' ), array( 'strategy' => 'defer', 'in_footer' => true ) );
} );

add_action( 'wp_head', function () {
	echo '<link rel="preconnect" href="https://fonts.googleapis.com">' . "\n";
	echo '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>' . "\n";
}, 1 );

/* ---------------------------------------------------------------
 * Head cleanup (carried over from the old theme)
 * ------------------------------------------------------------- */
remove_action( 'wp_head', 'rsd_link' );
remove_action( 'wp_head', 'wlwmanifest_link' );
remove_action( 'wp_head', 'index_rel_link' );
remove_action( 'wp_head', 'parent_post_rel_link', 10 );
remove_action( 'wp_head', 'start_post_rel_link', 10 );
remove_action( 'wp_head', 'adjacent_posts_rel_link_wp_head', 10 );
remove_action( 'wp_head', 'wp_generator' );
add_filter( 'the_generator', '__return_empty_string' );

/* ---------------------------------------------------------------
 * Content filters (carried over so posts render the same)
 * ------------------------------------------------------------- */

// Remove the <p> WordPress wraps around images.
add_filter( 'the_content', function ( $content ) {
	return preg_replace( '/<p>\s*(<a .*>)?\s*(<img .* \/>)\s*(<\/a>)?\s*<\/p>/iU', '\1\2\3', $content );
} );

add_filter( 'excerpt_more', function () {
	return '...';
} );
