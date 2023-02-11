<?php get_header(); ?>

			<div id="content">

				<div id="inner-content" class="clearfix">

					

					<div id="main" class="eightcol first clearfix" role="main">

						<div class="inner-main">

							<?php if (have_posts()) : while (have_posts()) : the_post(); ?>

								<article id="post-<?php the_ID(); ?>" <?php post_class('clearfix'); ?> role="article" itemscope itemtype="http://schema.org/BlogPosting">

									<!-- <div class="header-ad">
									</div>  -->

									<header class="article-header">


										<h1 class="entry-title single-title" itemprop="headline"><?php the_title(); ?></h1>

										<p class="byline vcard"><?php
											printf( __( 'Posted <time class="updated" datetime="%1$s" pubdate>%2$s</time> by <span class="author">%3$s</span>', 'bonestheme' ), get_the_time( 'Y-m-j' ), get_the_time( get_option('date_format')), bones_get_the_author_posts_link(), get_the_category_list(', ') );
										?></p>

									</header> <!-- end article header -->

									<section class="entry-content clearfix" itemprop="articleBody">
										<?php if(!get_post_format()) {
											the_content();
										} else {
											get_template_part( 'partials/format', get_post_format() );
										} ?>
									</section> <!-- end article section -->

									<footer class="article-footer">
										

										<?php the_tags( '<p class="tags"><span class="tags-title">' . __( 'Tags:', 'bonestheme' ) . '</span> ', ', ', '</p>' ); ?>

										<div class="disclaimer"><?php the_field('disclaimer_text','option');?></div>

										<div class="sponsored-content">
										</div>

									</footer> <!-- end article footer -->
									
									<div class="article-social">

										<div class="sharing">
											<h2>Now go talk about it.</h2>
											<?php a2_social_share(); ?>
										</div>

										<div class="reactions clearfix">
											<?php comments_template(); ?>
										</div>

									</div>

									<?php if(function_exists('wc_zone')) : ?>
									    <div class="wc_footer">
									        <?php echo wc_zone('footer'); ?>
									    </div>
									<?php endif; ?>

									<div class="article-related">
										<?php wp_related_posts()?>
									</div>
									
									<?php if(function_exists('wc_zone')) : ?>
									    <div class="wc_related">
									        <?php echo wc_zone('related'); ?>
									    </div>
									<?php endif; ?>

								</article> <!-- end article -->

							<?php endwhile; ?>

							<?php else : ?>

								<article id="post-not-found" class="hentry clearfix">
										<header class="article-header">
											<h1><?php _e( 'Oops, Post Not Found!', 'bonestheme' ); ?></h1>
										</header>
										<section class="entry-content">
											<p><?php _e( 'Uh Oh. Something is missing. Try double checking things.', 'bonestheme' ); ?></p>
										</section>
										<footer class="article-footer">
												<p><?php _e( 'This is the error message in the single.php template.', 'bonestheme' ); ?></p>
										</footer>
								</article>

							<?php endif; ?>

						</div>

					</div> <!-- end #main -->

					<?php get_sidebar(); ?>

				</div> <!-- end #inner-content -->

			</div> <!-- end #content -->

<?php get_footer(); ?>
