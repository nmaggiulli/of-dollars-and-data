<?php
/**
 * Sidebar author box — same markup as the old theme.
 * Content comes from ACF Options > Profile / Social Media.
 */
$author = odad_option_row( 'profile' );
$social = odad_option_row( 'social_media' );
if ( ! $author ) {
	return;
}
$image = ! empty( $author['image'] ) ? odad_image_url( $author['image'], 'large' ) : '';
?>
						<div class="author-box clearfix">

							<div  class="author-image">
								<?php if ( $image ) : ?>
								<img src="<?php echo esc_url( $image ); ?>" alt="<?php echo esc_attr( ! empty( $author['name'] ) ? $author['name'] : 'Nick Maggiulli' ); ?>"/>
								<?php endif; ?>

								<div class="author-social">
									<ul>
										<?php if ( ! empty( $social['name_twitter'] ) && ! empty( $social['link_twitter'] ) ) : ?>
											<li><a href="<?php echo esc_url( $social['link_twitter'] ); ?>" target="_blank" rel="noopener"><?php echo esc_html( $social['name_twitter'] ); ?></a></li>
										<?php endif; ?>
									</ul>
								</div>
							</div>

							<div class="inner-author clearfix">

								<div class="author-title">
									<h2><?php echo esc_html( isset( $author['name'] ) ? $author['name'] : '' ); ?></h2>
								</div>

								<div class="bio">
									<?php echo wp_kses_post( isset( $author['bio'] ) ? $author['bio'] : '' ); ?>
								</div>

							</div>

						</div>
