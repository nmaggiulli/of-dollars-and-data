<?php
/**
 * Template functions with the same names the old Aytoo/Bones templates used.
 *
 * The templates (header.php, index.php, single.php, …) are ported from the old
 * theme so the page structure — and therefore the look — is identical. These are
 * clean, safer re-implementations of the helpers those templates call.
 */

/* ---------------------------------------------------------------
 * Menus
 * ------------------------------------------------------------- */
function bones_main_nav() {
	wp_nav_menu( array(
		'container'      => false,
		'menu_class'     => 'nav top-nav clearfix',
		'theme_location' => 'main-nav',
		'depth'          => 0,
		'fallback_cb'    => 'bones_main_nav_fallback',
	) );
}

function bones_main_nav_fallback() {
	wp_page_menu( array(
		'show_home'  => true,
		'menu_class' => 'nav top-nav clearfix',
	) );
}

function bones_footer_links() {
	wp_nav_menu( array(
		'container'      => '',
		'menu_class'     => 'nav footer-nav clearfix',
		'theme_location' => 'footer-links',
		'depth'          => 0,
		'fallback_cb'    => '__return_false',
	) );
}

/* ---------------------------------------------------------------
 * Pagination (same markup as before; Font Awesome arrows)
 * ------------------------------------------------------------- */
function bones_page_navi() {
	global $wp_query;
	$bignum = 999999999;
	if ( $wp_query->max_num_pages <= 1 ) {
		return;
	}
	echo '<nav class="pagination">';
	echo paginate_links( array(
		'base'      => str_replace( $bignum, '%#%', esc_url( get_pagenum_link( $bignum ) ) ),
		'format'    => '',
		'current'   => max( 1, get_query_var( 'paged' ) ),
		'total'     => $wp_query->max_num_pages,
		'prev_text' => '<i class="fa fa-long-arrow-left"></i>',
		'next_text' => '<i class="fa fa-long-arrow-right"></i>',
		'type'      => 'list',
		'end_size'  => 0,
		'mid_size'  => 2,
	) );
	echo '</nav>';
}

/* ---------------------------------------------------------------
 * Byline author link
 * ------------------------------------------------------------- */
function bones_get_the_author_posts_link() {
	$id = get_the_author_meta( 'ID' );
	if ( ! $id ) {
		return '';
	}
	return sprintf(
		'<a href="%1$s" title="%2$s" rel="author">%3$s</a>',
		esc_url( get_author_posts_url( $id ) ),
		esc_attr( sprintf( 'Posts by %s', get_the_author() ) ),
		esc_html( get_the_author() )
	);
}

/* ---------------------------------------------------------------
 * Search form (same markup as before)
 * ------------------------------------------------------------- */
function bones_wpsearch( $form = '' ) {
	return '<form role="search" method="get" id="searchform" action="' . esc_url( home_url( '/' ) ) . '" >
  <input type="text" value="' . esc_attr( get_search_query() ) . '" name="s" id="s" placeholder="Search" />
  <input type="submit" id="searchsubmit" value="Search" />
  </form>';
}
add_filter( 'get_search_form', 'bones_wpsearch' );

/* ---------------------------------------------------------------
 * Comment layout (only existing comments/pingbacks are shown)
 * ------------------------------------------------------------- */
function bones_comments( $comment, $args, $depth ) {
	$url = get_comment_author_url( $comment );
	if ( strpos( $url, 'twitter' ) ) {
		$type = 'tweeted';
	} elseif ( strpos( $url, 'facebook' ) ) {
		$type = 'posted';
	} else {
		$type = 'commented';
	}
	?>
	<li <?php comment_class(); ?>>
		<article id="comment-<?php comment_ID(); ?>" class="comment-item clearfix">
			<aside class="comment-avatar">
				<a href="<?php echo esc_url( $url ); ?>" target="_blank" rel="noopener"><?php echo get_avatar( $comment, 50 ); ?></a>
			</aside>
			<div class="comment-block">
				<header class="comment-title vcard">
					<a class="comment-author-name" href="<?php echo esc_url( $url ); ?>"><?php echo esc_html( get_comment_author( $comment ) ); ?></a>
					<span class="comment-source"><?php echo esc_html( $type ); ?></span>
					<span class="comment-date">on <?php echo esc_html( get_comment_date( 'M d', $comment ) ); ?></span>
				</header>
				<section class="comment-content clearfix">
					<?php comment_text(); ?>
				</section>
			</div>
		</article>
	<?php
}

/* ---------------------------------------------------------------
 * ACF output helper (won't crash if ACF is ever deactivated)
 * ------------------------------------------------------------- */
function odad_the_field( $name, $post_id = false ) {
	$value = odad_field( $name, $post_id );
	if ( is_string( $value ) ) {
		echo wp_kses_post( $value );
	}
}

/* ---------------------------------------------------------------
 * Share icons on listings (date box + X / Facebook / LinkedIn)
 * Same markup as before; share URLs are now properly encoded.
 * ------------------------------------------------------------- */
function a2_social_aside() {
	$links = odad_share_links();
	?>
    <aside class="post-meta">
      <div class="meta-box post-date">
        <span class="day"><?php echo esc_html( get_the_date( 'd' ) ); ?></span>
        <span class="month"><?php echo esc_html( get_the_date( 'M' ) ); ?></span>
      </div>
      <div class="meta-box post-twitter-share">
        <a href="<?php echo esc_url( $links['x'] ); ?>" target="_blank" rel="noopener" class="popup">
          <i class="fa fa-twitter"></i>
        </a>
      </div>
      <div class="meta-box post-facebook-share">
        <a href="<?php echo esc_url( $links['facebook'] ); ?>" target="_blank" rel="noopener" class="popup">
          <i class="fa fa-facebook"></i>
        </a>
      </div>
      <div class="meta-box post-linkedin-share">
        <a href="<?php echo esc_url( $links['linkedin'] ); ?>" target="_blank" rel="noopener" class="popup">
          <i class="fa fa-linkedin"></i>
        </a>
      </div>
    </aside>
	<?php
}

/* ---------------------------------------------------------------
 * "Now go talk about it." buttons at the end of posts (same markup as before)
 * ------------------------------------------------------------- */
function a2_social_share() {
	$links = odad_share_links();
	?>
  <ul class="rrssb-buttons clearfix">
    <li class="facebook">
      <a href="<?php echo esc_url( $links['facebook'] ); ?>" target="_blank" rel="noopener" class="popup">
        <span class="icon">
          <svg version="1.1" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px" width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
            <path d="M27.825,4.783c0-2.427-2.182-4.608-4.608-4.608H4.783c-2.422,0-4.608,2.182-4.608,4.608v18.434
                c0,2.427,2.181,4.608,4.608,4.608H14V17.379h-3.379v-4.608H14v-1.795c0-3.089,2.335-5.885,5.192-5.885h3.718v4.608h-3.726
                c-0.408,0-0.884,0.492-0.884,1.236v1.836h4.609v4.608h-4.609v10.446h4.916c2.422,0,4.608-2.188,4.608-4.608V4.783z"/>
          </svg>
        </span>
        <span class="text">facebook</span>
      </a>
    </li>
    <li class="twitter">
      <a href="<?php echo esc_url( $links['x'] ); ?>" target="_blank" rel="noopener" class="popup">
        <span class="icon">
          <svg version="1.1" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px" width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
          <path d="M24.253,8.756C24.689,17.08,18.297,24.182,9.97,24.62c-3.122,0.162-6.219-0.646-8.861-2.32
              c2.703,0.179,5.376-0.648,7.508-2.321c-2.072-0.247-3.818-1.661-4.489-3.638c0.801,0.128,1.62,0.076,2.399-0.155
              C4.045,15.72,2.215,13.6,2.115,11.077c0.688,0.275,1.426,0.407,2.168,0.386c-2.135-1.65-2.729-4.621-1.394-6.965
              C5.575,7.816,9.54,9.84,13.803,10.071c-0.842-2.739,0.694-5.64,3.434-6.482c2.018-0.623,4.212,0.044,5.546,1.683
              c1.186-0.213,2.318-0.662,3.329-1.317c-0.385,1.256-1.247,2.312-2.399,2.942c1.048-0.106,2.069-0.394,3.019-0.851
              C26.275,7.229,25.39,8.196,24.253,8.756z"/>
          </svg>
        </span>
        <span class="text">twitter</span>
      </a>
    </li>
    <li class="linkedin">
      <a href="<?php echo esc_url( $links['linkedin'] ); ?>" target="_blank" rel="noopener" class="popup">
        <span class="icon">
          <svg version="1.1" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px" width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
            <path d="M25.424,15.887v8.447h-4.896v-7.882c0-1.979-0.709-3.331-2.48-3.331c-1.354,0-2.158,0.911-2.514,1.803
                c-0.129,0.315-0.162,0.753-0.162,1.194v8.216h-4.899c0,0,0.066-13.349,0-14.731h4.899v2.088c-0.01,0.016-0.023,0.032-0.033,0.048
                h0.033V11.69c0.65-1.002,1.812-2.435,4.414-2.435C23.008,9.254,25.424,11.361,25.424,15.887z M5.348,2.501
                c-1.676,0-2.772,1.092-2.772,2.539c0,1.421,1.066,2.538,2.717,2.546h0.032c1.709,0,2.771-1.132,2.771-2.546
                C8.054,3.593,7.019,2.501,5.343,2.501H5.348z M2.867,24.334h4.897V9.603H2.867V24.334z"/>
          </svg>
        </span>
        <span class="text">linkedin</span>
      </a>
    </li>
  </ul>
	<?php
}
