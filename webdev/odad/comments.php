<?php
/*
The comments page for Bones
*/

// Do not delete these lines
	if ( ! empty($_SERVER['SCRIPT_FILENAME']) && 'comments.php' == basename( $_SERVER['SCRIPT_FILENAME'] ) )
		die ('Please do not load this page directly. Thanks!');

	if ( post_password_required() ) { ?>
		<div class="alert alert-help">
			<p class="nocomments"><?php _e('This post is password protected. Enter the password to view comments.', 'bonestheme' ); ?></p>
		</div>
	<?php
		return;
	}
?>

<!-- You can start editing here. -->

<?php if ( have_comments() ) : ?>

	<div id="comments" class="comments-title">
		<h4>What's been said:</h4>
		<span class="how-to-comment">Discussions found on the web</span>
	</div>

	<ol class="commentlist">
		<?php wp_list_comments( 'callback=bones_comments' ); ?>
	</ol>

	<nav id="comment-nav">
		<ul class="clearfix">
			<li class="previous-comments"><?php previous_comments_link('<span><i class="fa fa-chevron-left"></i> Older</span>') ?></li>
			<li class="next-comments"><?php next_comments_link('<span>Newer <i class="fa fa-chevron-right"></i> </span>') ?></li>
		</ul>
	</nav>

	<?php else : // this is displayed if there are no comments so far ?>

	<?php if ( comments_open() ) : ?>
			<!-- If comments are open, but there are no comments. -->

	<?php else : // comments are closed ?>

	<!-- If comments are closed. -->
	<!--p class="nocomments"><?php _e( 'Comments are closed.', 'bonestheme' ); ?></p-->

	<?php endif; ?>

<?php endif; ?>