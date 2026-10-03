				<div id="sidebar" class="sidebar fourcol last clearfix" role="complementary">
					
						<?php get_template_part('partials/author-box'); ?>
					

					<div class="inner-sidebar">

					<?php if ( is_active_sidebar( 'sidebar1' ) ) : ?>

						<?php dynamic_sidebar( 'sidebar1' ); ?>

					<?php endif; ?>

					</div>
				</div>

