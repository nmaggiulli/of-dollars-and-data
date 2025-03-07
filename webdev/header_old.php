<!doctype html>

<!--[if lt IE 7]><html <?php language_attributes(); ?> class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
<!--[if (IE 7)&!(IEMobile)]><html <?php language_attributes(); ?> class="no-js lt-ie9 lt-ie8"><![endif]-->
<!--[if (IE 8)&!(IEMobile)]><html <?php language_attributes(); ?> class="no-js lt-ie9"><![endif]-->
<!--[if gt IE 8]><!--> <html <?php language_attributes(); ?> class="no-js"><!--<![endif]-->

	<head>
		<meta charset="utf-8">
		<!-- Preload header image -->
		<?php if(get_field('header_image', 'option')) : 
			$header = get_field('header_image', 'option'); ?>
			<link rel="preload" href="<?php echo $header; ?>" as="image">
		<?php endif; ?>
		<!-- Google Chrome Frame for IE -->
		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">

		<title><?php wp_title(''); ?></title>

		<!-- mobile meta (hooray!) -->
		<meta name="HandheldFriendly" content="True">
		<meta name="MobileOptimized" content="320">
		<meta name="viewport" content="width=device-width, initial-scale=1.0"/>

		<!-- icons & favicons (for more: http://www.jonathantneal.com/blog/understand-the-favicon/) -->
		<?php
		$fav_png = get_field('favicons', 'option')[0]['favicon_png'];
		$fav_ico = get_field('favicons', 'option')[0]['favicon_ico'];
		?>

		<link rel="icon" href="<?php echo $fav_png; ?>">
		<!--[if IE]>
			<link rel="shortcut icon" href="<?php echo $fav_ico; ?>">
		<![endif]-->
		<!-- or, set /favicon.ico for IE10 win -->
		<meta name="msapplication-TileColor" content="#f01d4f">
		<meta name="msapplication-TileImage" content="<?php echo get_template_directory_uri(); ?>/library/images/win8-tile-icon.png">

		<link rel="pingback" href="<?php bloginfo('pingback_url'); ?>">

		<!-- wordpress head functions -->
		<?php wp_head(); ?>
		<!-- end of wordpress head -->
		<!-- drop Google Analytics Here -->
		<!-- end analytics -->
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

				<?php // Theme Header
				if(get_field('header_image', 'option')) {
					$header = get_field('header_image', 'option');
					    $image_id = attachment_url_to_postid($header);
						$image_data = wp_get_attachment_image_src($image_id, 'full');
						$width = $image_data[1] ? $image_data[1] : '1800'; // Default width if not found
						$height = $image_data[2] ? $image_data[2] : '600'; // Default height if not found
   
					echo '<a href="'. get_bloginfo( "url" ) .'">';
					echo '<div class="header-banner" style="background-image: url('. $header .');"></div>';
					echo '</a>';

				} else {
					echo '<div class="wrap">';
					echo '<h1 class="site-title"><a href="'. get_bloginfo( "url" ) .'">'. get_bloginfo("name") .'</a></h1>';
					echo '</div>';
				}
				?>



					<!-- to use a image just replace the bloginfo('name') with your img src and remove the surrounding <p>
					<p id="logo" class="h1"><a href="<?php echo home_url(); ?>" rel="nofollow"><?php bloginfo('name'); ?></a></p> -->

					<!-- if you'd like to use the site description you can un-comment it below -->
					<?php // bloginfo('description'); ?>


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