<?php get_header(); ?>

			<div id="content">

				<div id="inner-content" class=" clearfix">

						<div id="main" class="eightcol first clearfix" role="main">

							<!-- <div class="header-ad archive-banner">
							</div> --> 

							<div class="inner-main">

								<?php if (have_posts()) : while (have_posts()) : the_post(); ?>

								<article id="post-<?php the_ID(); ?>" <?php post_class( 'clearfix' ); ?> role="article">

									<?php a2_social_aside(); ?>

									<header class="article-header">

										<h1 class="h2"><a href="<?php the_permalink() ?>" rel="bookmark" title="<?php the_title_attribute(); ?>"><?php the_title(); ?></a></h1>

									</header> <!-- end article header -->

									<section class="entry-content clearfix">
										<?php the_excerpt(); ?>
									</section> <!-- end article section -->

									<footer class="article-footer">
										<a class="excerpt-read-more" href="<?php the_permalink(); ?>">Read More</a>
									</footer> <!-- end article footer -->

									<?php // comments_template(); // uncomment if you want to use them ?>

								</article> <!-- end article -->

								<?php endwhile; ?>

										<?php if ( function_exists( 'bones_page_navi' ) ) { ?>
												<?php bones_page_navi(); ?>
										<?php } else { ?>
												<nav class="wp-prev-next">
														<ul class="clearfix">
															<li class="prev-link"><?php next_posts_link( __( '&laquo; Older Entries', 'bonestheme' )) ?></li>
															<li class="next-link"><?php previous_posts_link( __( 'Newer Entries &raquo;', 'bonestheme' )) ?></li>
														</ul>
												</nav>
										<?php } ?>

								<?php else : ?>

										<article id="post-not-found" class="hentry clearfix">
												<header class="article-header">
													<h1><?php _e( 'Oops, Post Not Found!', 'bonestheme' ); ?></h1>
											</header>
												<section class="entry-content">
													<p><?php _e( 'Uh Oh. Something is missing. Try double checking your link.', 'bonestheme' ); ?></p>
											</section>
											<footer class="article-footer">
											</footer>
										</article>

								<?php endif; ?>

								</div>

						</div> <!-- end #main -->

						<?php get_sidebar(); ?>

				</div> <!-- end #inner-content -->

			</div> <!-- end #content -->

<?php get_footer(); ?>
