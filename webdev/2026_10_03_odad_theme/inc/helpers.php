<?php
/**
 * Small helpers used by the templates.
 */

/**
 * Read an ACF field without fatal errors if ACF is ever deactivated.
 * The old theme stored most site settings (header image, author box,
 * disclaimer) in ACF options, so we keep reading from the same place.
 */
function odad_field( $name, $post_id = false ) {
	if ( ! function_exists( 'get_field' ) ) {
		return null;
	}
	return get_field( $name, $post_id );
}

/**
 * ACF "repeater with one row" settings (profile, social_media, favicons)
 * come back as array( 0 => array(...) ). Return the first row or an empty array.
 */
function odad_option_row( $name ) {
	$value = odad_field( $name, 'option' );
	return ( is_array( $value ) && isset( $value[0] ) && is_array( $value[0] ) ) ? $value[0] : array();
}

/**
 * Turn an ACF image value into a URL, whatever format it comes back in
 * (URL string, attachment ID, or image array).
 */
function odad_image_url( $value, $size = 'full' ) {
	if ( is_array( $value ) ) {
		if ( 'full' !== $size && ! empty( $value['sizes'][ $size ] ) ) {
			return $value['sizes'][ $size ];
		}
		return ! empty( $value['url'] ) ? $value['url'] : '';
	}
	if ( is_numeric( $value ) ) {
		$url = wp_get_attachment_image_url( (int) $value, $size );
		return $url ? $url : '';
	}
	return is_string( $value ) ? $value : '';
}

/** Desktop and mobile header artwork (same sources as the old theme). */
function odad_header_images() {
	return array(
		'desktop' => odad_image_url( odad_field( 'header_image', 'option' ) ),
		'mobile'  => apply_filters( 'odad_mobile_header_image', 'https://ofdollarsanddata.com/wp-content/uploads/2025/03/odad_header_mobile.webp' ),
	);
}

/**
 * Properly encoded share URLs for the current post.
 * Titles are decoded first so apostrophes and ampersands don't cut the text off.
 */
function odad_share_links() {
	$social = odad_option_row( 'social_media' );
	$handle = ! empty( $social['name_twitter'] ) ? $social['name_twitter'] : '@dollarsanddata';
	$title  = html_entity_decode( get_the_title(), ENT_QUOTES, 'UTF-8' );
	$url    = get_permalink();

	return array(
		'x'        => 'https://twitter.com/intent/tweet?text=' . rawurlencode( $title . ' by ' . $handle ) . '&url=' . rawurlencode( $url ),
		'facebook' => 'https://www.facebook.com/sharer/sharer.php?u=' . rawurlencode( $url ),
		'linkedin' => 'https://www.linkedin.com/sharing/share-offsite/?url=' . rawurlencode( $url ),
	);
}

/** Fallback favicon from the old ACF setting, only if no WordPress Site Icon is set. */
add_action( 'wp_head', function () {
	if ( has_site_icon() ) {
		return;
	}
	$fav = odad_option_row( 'favicons' );
	if ( ! empty( $fav['favicon_png'] ) ) {
		echo '<link rel="icon" href="' . esc_url( $fav['favicon_png'] ) . '">' . "\n";
	}
} );

/** Preload the header artwork so it paints quickly. */
add_action( 'wp_head', function () {
	$img = odad_header_images();
	if ( $img['mobile'] ) {
		echo '<link rel="preload" href="' . esc_url( $img['mobile'] ) . '" as="image" media="(max-width: 768px)">' . "\n";
	}
	if ( $img['desktop'] ) {
		echo '<link rel="preload" href="' . esc_url( $img['desktop'] ) . '" as="image" media="(min-width: 769px)">' . "\n";
	}
}, 2 );
