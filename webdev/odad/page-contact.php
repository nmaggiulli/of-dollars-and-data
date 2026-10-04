<?php get_header(); ?>

			<div id="content">

				<div id="inner-content" class="clearfix">

						<div id="main" class="eightcol first clearfix" role="main">

							<div class="inner-main">

								<?php if (have_posts()) : while (have_posts()) : the_post(); ?>

								<article id="post-<?php the_ID(); ?>" <?php post_class( 'clearfix' ); ?> role="article" itemscope itemtype="http://schema.org/BlogPosting">

									<header class="article-header">

										<h1 class="page-title" itemprop="headline"><?php the_title(); ?></h1>

									</header> <!-- end article header -->

									<section class="entry-content clearfix" itemprop="articleBody">
										<?php the_content(); ?>

										<div class="contact-methods">
											<?php 
											$odad_c = function ( $k ) { $v = odad_field( $k ); return ( is_array( $v ) && isset( $v[0] ) ) ? $v[0] : array(); };
											$email = $odad_c( 'contact_email' );
											$twitter = $odad_c( 'contact_twitter' );
											$facebook = $odad_c( 'contact_facebook' );

											if($email) : ?>
												<div class="contact-method contact-email">
													<i class="fa fa-envelope"></i><a href="mailto:<?php echo esc_attr( $email['email_address'] . '?' . $email['email_subject'] ); ?>"><?php echo $email['email_address']; ?></a>
												</div>
											<?php endif; 

											if($twitter) : ?>
												<div class="contact-method contact-twitter">
													<i class="fa fa-twitter"></i><a href="<?php echo $twitter['link'];?>"><?php echo $twitter['handle'];?></a>
												</div>
											<?php endif; 

											if($facebook) : ?>
												<div class="contact-method contact-facebook">
													<i class="fa fa-facebook"></i><a href="<?php echo $facebook['link'];?>"><?php echo $facebook['title'];?></a>
												</div>
											<?php endif; ?>

										</div>


									</section> <!-- end article section -->

									<footer class="article-footer">
										<?php the_tags( '<span class="tags">' . __( 'Tags:', 'bonestheme' ) . '</span> ', ', ', '' ); ?>

										<div class="disclaimer"><?php odad_the_field('disclaimer_text','option');?></div>
										
										

										

									</footer> <!-- end article footer -->

									<?php // comments_template(); ?>

								</article> <!-- end article -->

								<?php endwhile; else : ?>

										<article id="post-not-found" class="hentry clearfix">
											<header class="article-header">
												<h1><?php _e( 'Oops, Post Not Found!', 'bonestheme' ); ?></h1>
											</header>
											<section class="entry-content">
												<p><?php _e( 'Uh Oh. Something is missing. Try double checking things.', 'bonestheme' ); ?></p>
											</section>
											<footer class="article-footer">
													<p><?php _e( 'This is the error message in the page.php template.', 'bonestheme' ); ?></p>
											</footer>
										</article>

								<?php endif; ?>

							</div>

						</div> <!-- end #main -->

						<?php get_sidebar(); ?>

				</div> <!-- end #inner-content -->

			</div> <!-- end #content -->

<?php get_footer(); ?>
