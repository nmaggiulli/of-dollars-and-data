<!doctype html>
<html <?php language_attributes(); ?> class="no-js">
	<head>
		<meta charset="utf-8">
<!-- Preload header images -->
<?php if(get_field('header_image', 'option')) : 
    $header = get_field('header_image', 'option');
    $mobile_header = 'https://ofdollarsanddata.com/wp-content/uploads/2025/03/odad_header_mobile.webp';
?>
    <!-- Preload mobile header for mobile users -->
    <link rel="preload" href="<?php echo $mobile_header; ?>" as="image" media="(max-width: 768px)">
    <!-- Preload desktop header for desktop users -->
    <link rel="preload" href="<?php echo $header; ?>" as="image" media="(min-width: 769px)">
<?php endif; ?>
		<!-- Google Chrome Frame for IE -->
		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
		<title><?php wp_title(''); ?></title>
		<!-- mobile meta (hooray!) -->
		<meta name="HandheldFriendly" content="True">
		<meta name="MobileOptimized" content="320">
		<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
		<!-- Font display swap for better performance -->
		<style>
			@font-face {
				font-display: swap !important;
			}
		</style>
		
		<!-- ADDITION: Header banner styles moved to head -->
		<?php if(get_field('header_image', 'option')) : 
		    $header = get_field('header_image', 'option');
		    $mobile_header = 'https://ofdollarsanddata.com/wp-content/uploads/2025/03/odad_header_mobile.webp';
		    
		    // Specify dimensions for better CLS
		    $desktop_width = 1200; // Set your actual width
		    $desktop_height = 133; // Your actual height
		    $mobile_width = 600;  // Your mobile width
		    $mobile_height = 133; // Your mobile height
		    
		    echo '<style>
		        .header-banner {
		            background-image: url('. $mobile_header .');
		            background-repeat: no-repeat;
		            background-size: contain;
		            background-position: center;
		            width: 100%;
		            height: '. $mobile_height .'px;
		            max-width: 100%;
		            will-change: transform; /* Hint for browser optimization */
		        }
		        @media (min-width: 769px) {
		            .header-banner {
		                background-image: url('. $header .');
		                height: '. $desktop_height .'px;
		            }
		        }
		    </style>';
		endif; ?>
		<!-- END ADDITION -->
		
		<!-- icons & favicons -->
		<?php
		$fav_png = get_field('favicons', 'option')[0]['favicon_png'];
		$fav_ico = get_field('favicons', 'option')[0]['favicon_ico'];
		?>
		<link rel="icon" href="<?php echo $fav_png; ?>">
		<!--[if IE]>
			<link rel="shortcut icon" href="<?php echo $fav_ico; ?>">
		<![endif]-->
		<meta name="msapplication-TileColor" content="#f01d4f">
		<meta name="msapplication-TileImage" content="<?php echo get_template_directory_uri(); ?>/library/images/win8-tile-icon.png">
		<link rel="pingback" href="<?php bloginfo('pingback_url'); ?>">
		<!-- wordpress head functions -->
		<?php wp_head(); ?>
		<!-- end of wordpress head -->
	<?php
		if (is_user_logged_in()) {
			echo '<div id="mediavine-settings" data-blacklist-all="1"></div>';
		}
	?>
	</head>
	<body <?php body_class(); ?>>
		<div id="container">
			<header class="header" role="banner">
				<div id="inner-header" class="inner-header clearfix">
				<?php if(get_field('header_image', 'option')) : 
				    $header = get_field('header_image', 'option');
				    $mobile_header = 'https://ofdollarsanddata.com/wp-content/uploads/2025/03/odad_header_mobile.webp';
				    
				    // Specify dimensions for better CLS
				    $desktop_width = 1200; // Set your actual width
				    $desktop_height = 133; // Your actual height
				    $mobile_width = 600;  // Your mobile width
				    $mobile_height = 133; // Your mobile height
				    
				    echo '<a href="'. get_bloginfo("url") .'">';
				    echo '<div class="header-banner" role="img" aria-label="Site Header" style="width:100%;height:'.$mobile_height.'px;"></div>';
				    echo '</a>';
				endif; ?>
					<div id="sticker" class="nav-wrap">
						<p class="mobile-title"><a href="<?php bloginfo('url'); ?>"><?php bloginfo('name'); ?></a></p>
						<i class="fa fa-bars nav-toggle"></i>
						<nav class="header-nav wrap clearfix" role="navigation">
							<?php bones_main_nav(); ?>
						</nav>
					</div>
				</div> <!-- end #inner-header -->
			</header> <!-- end header -->
			<?php if(function_exists('wc_zone')) : ?>
			    <div class="wc_leaderboard">
			        <?php echo wc_zone('leaderboard'); ?>
			    </div>
			
			<?php endif; ?>