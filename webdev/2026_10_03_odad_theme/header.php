<!doctype html>
<html <?php language_attributes(); ?> class="no-js">
	<head>
		<meta charset="utf-8">
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<?php
		// Header banner: same images and styles as the old theme. This prints
		// before the stylesheets on purpose — that's the order the old theme used,
		// and the old stylesheet's banner rules are meant to override these.
		$odad_img = odad_header_images();
		if ( $odad_img['desktop'] || $odad_img['mobile'] ) :
			$odad_mobile  = $odad_img['mobile'] ? $odad_img['mobile'] : $odad_img['desktop'];
			$odad_desktop = $odad_img['desktop'] ? $odad_img['desktop'] : $odad_img['mobile'];
			?>
		<style>
			.header-banner {
				background-image: url(<?php echo esc_url( $odad_mobile ); ?>);
				background-repeat: no-repeat;
				background-size: contain;
				background-position: center;
				width: 100%;
				height: 133px;
				max-width: 100%;
				will-change: transform;
			}
			@media (min-width: 769px) {
				.header-banner {
					background-image: url(<?php echo esc_url( $odad_desktop ); ?>);
					height: 133px;
				}
			}
		</style>
		<?php endif; ?>
		<?php wp_head(); ?>
	</head>
	<body <?php body_class(); ?>>
		<?php wp_body_open(); ?>
		<?php if ( is_user_logged_in() ) : // hide Mediavine ads when you're logged in ?>
			<div id="mediavine-settings" data-blacklist-all="1"></div>
		<?php endif; ?>
		<div id="container">
			<header class="header" role="banner">
				<div id="inner-header" class="inner-header clearfix">
				<?php if ( $odad_img['desktop'] || $odad_img['mobile'] ) : ?>
					<a class="header-banner-link" href="<?php echo esc_url( home_url() ); ?>"><div class="header-banner" role="img" aria-label="<?php echo esc_attr( get_bloginfo( 'name' ) ); ?>" style="width:100%;height:133px;"></div></a>
				<?php endif; ?>
					<div id="sticker" class="nav-wrap">
						<p class="mobile-title"><a href="<?php echo esc_url( home_url() ); ?>"><?php bloginfo( 'name' ); ?></a></p>
						<i class="fa fa-bars nav-toggle" role="button" tabindex="0" aria-label="Menu" aria-expanded="false"></i>
						<nav class="header-nav wrap clearfix" role="navigation">
							<?php bones_main_nav(); ?>
						</nav>
					</div>
				</div> <!-- end #inner-header -->
			</header> <!-- end header -->
			<?php if ( function_exists( 'wc_zone' ) ) : ?>
				<div class="wc_leaderboard">
					<?php echo wc_zone( 'leaderboard' ); ?>
				</div>
			<?php endif; ?>
