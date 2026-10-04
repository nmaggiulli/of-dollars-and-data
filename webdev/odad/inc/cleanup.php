<?php
/**
 * Front-end cleanup: stop loading things readers never use.
 * Nothing here affects wp-admin or the post editor.
 */

/**
 * Editor toolbar scripts. WordPress's "quicktags" editor buttons and Kit's
 * add-on for them were loading on every public page. They're only used in
 * the post editor, so drop them from the public site.
 */
add_action( 'wp_enqueue_scripts', function () {
	if ( is_admin() ) {
		return;
	}
	wp_dequeue_script( 'convertkit-admin-quicktags' );
	wp_dequeue_style( 'convertkit-admin-quicktags' );
	wp_dequeue_script( 'quicktags' );
}, 100 );

/**
 * Kit also prints a hidden pop-up template for that editor button at the bottom
 * of every page. Remove it from public pages.
 */
add_action( 'wp_footer', function () {
	global $wp_filter;
	if ( empty( $wp_filter['wp_footer'] ) ) {
		return;
	}
	foreach ( $wp_filter['wp_footer']->callbacks as $priority => $callbacks ) {
		foreach ( $callbacks as $cb ) {
			$fn = $cb['function'];
			if ( is_array( $fn ) && is_object( $fn[0] ) && is_string( $fn[1] )
				&& stripos( get_class( $fn[0] ), 'convertkit' ) !== false
				&& stripos( $fn[1], 'quicktag' ) !== false ) {
				remove_action( 'wp_footer', $fn, $priority );
			}
		}
	}
}, 0 );

/**
 * WordPress's emoji script converts emoji to images for very old browsers.
 * Every current browser shows emoji natively, so it's dead weight.
 */
remove_action( 'wp_head', 'print_emoji_detection_script', 7 );
remove_action( 'wp_print_styles', 'print_emoji_styles' );
add_action( 'init', function () {
	remove_action( 'wp_enqueue_scripts', 'wp_enqueue_emoji_styles' );
} );
