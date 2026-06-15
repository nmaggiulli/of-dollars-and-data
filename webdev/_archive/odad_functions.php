<?php

require_once( 'config/bones.php' ); // if you remove this, bones will break
require_once( 'config/bundled-plugins/plugin-activation.php' );
require_once( 'config/bundled-plugins/required-plugins.php' );
require_once( 'config/custom-widgets.php' );
require_once( 'config/customization/kirki/kirki.php' );
require_once( 'config/customization/customizer.php' );
// require_once( 'config/customization/custom-header.php' );

// require_once( 'library/admin.php' );
// require_once( 'config/custom-post-type.php' );


// require_once( 'library/translation/translation.php' );

/************* THUMBNAIL SIZE OPTIONS *************/

// Thumbnail sizes
// add_image_size( 'bones-thumb-600', 600, 150, true );


/************* ACTIVE SIDEBARS ********************/

// Sidebars & Widgetizes Areas
function bones_register_sidebars() {
	register_sidebar(array(
		'id' => 'sidebar1',
		'name' => __( 'Sidebar 1', 'bonestheme' ),
		'description' => __( 'The first (primary) sidebar.', 'bonestheme' ),
		'before_widget' => '<div id="%1$s" class="widget %2$s">',
		'after_widget' => '</div>',
		'before_title' => '<h4 class="widgettitle">',
		'after_title' => '</h4>',
	));
}


/************* WP ADMIN ********************/

// Add ACF Options Page
if( function_exists('acf_add_options_page') ) {
  acf_add_options_page();
}

// Hide ACF Settings Menu
// To edit, Go to http://trb.dev/wp-admin/edit.php?post_type=acf-field-group
// add_filter('acf/settings/show_admin', '__return_false');

/************* ROUTING ********************/

/**
 * Adds support for single-category.php templates
 * Routes a single blog post template based on its category
 */
add_filter('single_template', create_function(
  '$the_template',
  'foreach( (array) get_the_category() as $cat ) {
    if ( file_exists(TEMPLATEPATH . "/single-{$cat->slug}.php") )
    return TEMPLATEPATH . "/single-{$cat->slug}.php"; }
  return $the_template;' )
);

/************* DISPLAY ********************/

/**
 * Filter Excerpt and set default excerpt if no there is no content
 * Used for post formats like quote and hot links
 */
// add_filter('get_the_excerpt', 'a2_default_excerpts');
function a2_default_excerpts($output) {
  if(strlen($output) == 0) {
    $format =  get_post_format();
    if(!$format) {$format = in_category('hot-links');}

    $excerpts = get_field('default_excerpts','option')[0];

    switch($format) {
      case 'true':
        $output = $excerpts['hot_links'];
        break;
      case 'quote':
        $output = $excerpts['quote'];
        break;
      case 'gallery':
        $output = $excerpts['gallery'];
        break;
      case 'link':
        $output = $excerpts['link'];
        break;
      case false:
        $output = wp_trim_words(strip_shortcodes(the_content('')), 55);
        break;
    }
  }
  return $output;
}

/**
 * Add parameters to a url
 * @param  [string] $link url to append parematers
 * @param  [array]  $parr associative array of parameters
 * @return [string]       new url string with parameters appended
 */
function a2_append_parameter($link, $parr) {
  if(strpos($link, "?")) {
    return $link . "&" . http_build_query($parr);
  } else {
    return $link . "?" . http_build_query($parr);
  }
}

/**
 * Remove iFrames from RSS
 */
add_filter( "the_excerpt_rss", "a2_rss_filter");
add_filter( "the_content_feed", "a2_rss_filter" );
function a2_rss_filter($content){
   $content = preg_replace( '/(<p><iframe|<iframe)(.*)(<\/iframe><\/p>|<\/iframe>)/s', '', $content );
   if(strlen($content) < 10) {
      $content = get_field('default_excerpts', 'options')[0]['rss_video_posts'];
   }
   $content = substr(strip_tags($content), 0, '425') . '...';
   return $content;
}

/**
 * Social Tabs on Post Archives
 */
function a2_social_aside() {
  global $post, $posts;
  $social = get_field('social_media', 'options')[0];
  $twitter = addslashes( get_the_title() . ' by '. $social['name_twitter'] .' '. get_permalink() );
  $linkString = htmlentities("url=" . urlencode(get_permalink()) . "&title=" . urlencode(get_the_title()));

  ob_start(); ?>
    <aside class="post-meta">
      <div class="meta-box post-date">
        <span class="day"><?php echo get_the_date('d'); ?></span>
        <span class="month"><?php echo get_the_date('M'); ?></span>
      </div>
      <div class="meta-box post-twitter-share">
        <a href="http://twitter.com/intent/tweet?text=<?php echo $twitter; ?>" target="_blank" class="popup">
          <i class="fa fa-twitter"></i>
        </a>
      </div>
      <div class="meta-box post-facebook-share">
        <a href="https://www.facebook.com/sharer/sharer.php?u=<?php the_permalink(); ?>" target="_blank" class="popup">
          <i class="fa fa-facebook"></i>
        </a>
      </div>
      <div class="meta-box post-linkedin-share">
        <a href="http://www.linkedin.com/shareArticle?mini=true&amp;<?php echo $linkString; ?>" target="_blank" class="popup">
          <i class="fa fa-linkedin"></i>
        </a>
      </div>
    </aside>

  <? ob_end_flush();
}

/**
 * Post Footer Social Buttons
 */
function a2_social_share() {
  global $post, $posts;

  $social = get_field('social_media', 'options')[0];
  $twitter = addslashes( get_the_title() . ' by '. $social['name_twitter'] .' '. get_permalink() );
  $linkString = htmlentities("url=" . urlencode(get_permalink()) . "&title=" . urlencode(get_the_title()));

  ob_start(); ?>
  <ul class="rrssb-buttons clearfix">
    <li class="facebook">
      <!-- Replace with your URL. For best results, make sure you page has the proper FB Open Graph tags in header:
      https://developers.facebook.com/docs/opengraph/howtos/maximizing-distribution-media-content/ -->
      <a href="https://www.facebook.com/sharer/sharer.php?u=<?php the_permalink(); ?>" target="_blank" class="popup">
        <span class="icon">
          <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
            <path d="M27.825,4.783c0-2.427-2.182-4.608-4.608-4.608H4.783c-2.422,0-4.608,2.182-4.608,4.608v18.434
                c0,2.427,2.181,4.608,4.608,4.608H14V17.379h-3.379v-4.608H14v-1.795c0-3.089,2.335-5.885,5.192-5.885h3.718v4.608h-3.726
                c-0.408,0-0.884,0.492-0.884,1.236v1.836h4.609v4.608h-4.609v10.446h4.916c2.422,0,4.608-2.188,4.608-4.608V4.783z"/>
          </svg>
        </span>
        <span class="text">facebook</span>
      </a>
    </li>

    <li class="twitter">
      <a href="http://twitter.com/intent/tweet?text=<?php echo $twitter; ?>" target="_blank" class="popup">
        <span class="icon">
          <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
               width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
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
        <!-- Replace href with your meta and URL information -->
        <a href="http://www.linkedin.com/shareArticle?mini=true&amp;<?php echo $linkString; ?>" target="_blank" class="popup">
            <span class="icon">
                <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="28px" height="28px" viewBox="0 0 28 28" enable-background="new 0 0 28 28" xml:space="preserve">
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
  <?
  ob_end_flush();
}


/************* ASSETS ********************/
/**
 * Enqueuing assets based on our current environment
 * Register all plugins
 * Enqueue as required by env, grunt handles concat and min
 */
function enqueue_theme_assets() {

  // Styles
  wp_register_style( 'fontAwesome', '//netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css', '', '1.0.0', 'all' );
  wp_register_style( 'application-css', get_stylesheet_directory_uri().'/library/public/application.min.css', '', '1.0.0', 'all' );

  // Scripts
  wp_register_script( 'appjs', get_stylesheet_directory_uri().'/library/public/scripts.js', '1.0.0', true );
  wp_register_script( 'application-js', get_stylesheet_directory_uri().'/library/public/application.min.js', '1.0.0', true );

  // Enqueue
  wp_enqueue_style('fontAwesome');
  wp_enqueue_style('application-css');
  // wp_enqueue_script('application-js');
  wp_enqueue_script('application-js');

}
// Fire enqueue
add_action( 'wp_enqueue_scripts', 'enqueue_theme_assets', 999 );

function a2_OAS() {
  echo "<script type='text/javascript'>
var oas_tag = {};
oas_tag.url = 'oascentral.investingmediasolutions.com'; //Define OAS URL
oas_tag.sizes = function () { //size is required [width, height]
oas_tag.definePos('Top', [728,90]);
oas_tag.definePos('TopRight', [300,250]);
oas_tag.definePos('Left', [300,250]); };
oas_tag.allowSizeOverride = true;

oas_tag.site_page = 'TheReformedBroker'; //Define OAS Site page
(function() {
oas_tag.version ='1'; oas_tag.loadAd = oas_tag.loadAd || function(){}; var oas = document.createElement('script'), protocol = 'https:' == document.location.protocol?'https://':'http://',
node = document.getElementsByTagName('script')[0];
oas.type = 'text/javascript'; oas.async = true; oas.src = protocol + oas_tag.url + '/om/' + oas_tag.version + '.js'; node.parentNode.insertBefore(oas, node); })(); </script>";
}
add_action( 'wp_head', 'a2_OAS' );


/************ CUSTOM FEED ********/
/**
 * Modify the default feed 
 * This allows to modify the default feed-rss2.php.  
 * */

add_feed('rss2', 'aytoo_my_custom_feed');
function aytoo_my_custom_feed() {  
  load_template( TEMPLATEPATH . '/feed-rss2.php');  
}  


/** Alter the excerpt_rss function 
 * Return the full body text instead of an excerpt. 
 * */
add_filter( 'the_excerpt_rss', function( $content ) {
  $blocks = parse_blocks(get_the_content());
  $output = '';
  foreach( $blocks as $block ) {
    $output .= render_block( $block );
  }
  return $output;
});

function enqueue_custom_scripts() {
	// Enqueue Moment.js library
    wp_enqueue_script('momentjs', 'https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js', array(), '2.29.1', true);

    // Enqueue Chart.js library
    wp_enqueue_script('chartjs', 'https://cdn.jsdelivr.net/npm/chart.js@2.9.4', array('momentjs'), '2.9.4', true);

    wp_enqueue_script('sp500_calculator', get_template_directory_uri() . '/sp500_calculator.js', array(), '1.0.0', true);
	wp_enqueue_script('sp500_dca_calculator', get_template_directory_uri() . '/sp500_dca_calculator.js', array('momentjs', 'chartjs'), '1.0.0', true);
	wp_enqueue_script('us_stock_bond_calculator', get_template_directory_uri() . '/us_stock_bond_calculator.js', array('momentjs', 'chartjs'), '1.0.0', true);
	wp_enqueue_script('investment_return_calculator', get_template_directory_uri() . '/investment_return_calculator.js', array('chartjs'), '1.0.0', true);
		wp_enqueue_script('net_worth_by_age_calculator', get_template_directory_uri() . '/net_worth_by_age_calculator.js', array('momentjs', 'chartjs'), '1.0.0', true);
			wp_enqueue_script('income_by_age_calculator', get_template_directory_uri() . '/income_by_age_calculator.js', array('momentjs', 'chartjs'), '1.0.0', true);
				wp_enqueue_script('rent_vs_buy_calculator', get_template_directory_uri() . '/rent_vs_buy_calculator.js', array('momentjs', 'chartjs'), '1.0.0', true);
}
add_action('wp_enqueue_scripts', 'enqueue_custom_scripts');
?>