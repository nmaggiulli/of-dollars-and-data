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
									</section> <!-- end article section -->

									<footer class="article-footer">
										<?php the_tags( '<span class="tags">' . __( 'Tags:', 'bonestheme' ) . '</span> ', ', ', '' ); ?>

										<div class="disclaimer"><?php odad_the_field('disclaimer_text','option');?></div>
										
										<div class="book-ad">
											
										</div>

										<div class="article-footerad">
											
										</div>

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
