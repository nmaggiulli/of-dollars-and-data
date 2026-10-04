<?php
/**
 * RSS feed customizations, carried over unchanged from the old theme so the
 * feed (and anything that reads it, like a newsletter tool) behaves the same.
 */

/** Use the theme's feed-rss2.php for the main RSS feed. */
add_feed( 'rss2', 'odad_custom_feed' );
function odad_custom_feed() {
	load_template( get_template_directory() . '/feed-rss2.php' );
}

/**
 * Strip iframes from feed excerpts/content and trim to 425 characters.
 * (Same as the old a2_rss_filter.)
 */
add_filter( 'the_excerpt_rss', 'odad_rss_filter' );
add_filter( 'the_content_feed', 'odad_rss_filter' );
function odad_rss_filter( $content ) {
	$content = preg_replace( '/(<p><iframe|<iframe)(.*)(<\/iframe><\/p>|<\/iframe>)/s', '', $content );
	if ( strlen( $content ) < 10 ) {
		$excerpts = odad_field( 'default_excerpts', 'options' );
		$content  = ( is_array( $excerpts ) && ! empty( $excerpts[0]['rss_video_posts'] ) ) ? $excerpts[0]['rss_video_posts'] : '';
	}
	$content = substr( strip_tags( $content ), 0, 425 ) . '...';
	return $content;
}

/**
 * Return the full rendered post body for the_excerpt_rss.
 * This runs after the filter above (same priority, added later), so — exactly
 * as on the old theme — feed items carry the full post.
 */
add_filter( 'the_excerpt_rss', function ( $content ) {
	$blocks = parse_blocks( get_the_content() );
	$output = '';
	foreach ( $blocks as $block ) {
		$output .= render_block( $block );
	}
	return $output;
} );
