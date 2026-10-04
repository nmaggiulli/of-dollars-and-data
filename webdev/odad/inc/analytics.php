<?php
/**
 * Google Analytics (GA4), loaded 3 seconds after the page finishes loading so it
 * doesn't slow the page down. Moved here from WPCode's Header & Footer screen.
 *
 * The small gtag() setup runs right away in the <head>, so anything the page
 * reports before Analytics has loaded (e.g. a calculator opened from a shared
 * link) waits in line and is sent as soon as it loads. Only the download of
 * Google's script is delayed.
 *
 * To change the Analytics property, edit the ID below.
 */

define( 'ODAD_GA_ID', 'G-CQWDD1SKD8' );

add_action( 'wp_head', function () {
	if ( ! ODAD_GA_ID ) {
		return;
	}
	$id = esc_js( ODAD_GA_ID );
	?>
<script>
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', '<?php echo $id; ?>');
</script>
	<?php
}, 2 );

add_action( 'wp_footer', function () {
	if ( ! ODAD_GA_ID ) {
		return;
	}
	$id = esc_js( ODAD_GA_ID );
	?>
<!-- Delayed Google Analytics -->
<script>
window.addEventListener('load', function() {
  setTimeout(function() {
    var analyticsScript = document.createElement('script');
    analyticsScript.async = true;
    analyticsScript.src = 'https://www.googletagmanager.com/gtag/js?id=<?php echo $id; ?>';
    document.head.appendChild(analyticsScript);
  }, 3000); // This delays loading by 3 seconds
});
</script>
	<?php
}, 100 );
