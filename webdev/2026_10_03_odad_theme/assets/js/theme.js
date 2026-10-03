/*
 * Of Dollars And Data theme scripts.
 * Plain-JavaScript replacement for the old 170 KB jQuery bundle. It reproduces
 * the three things the old bundle actually did on this site, using the same
 * element IDs and classes the stylesheet expects.
 */
(function () {
	'use strict';

	/* ---------------------------------------------------------------
	 * 1. Sticky menu bar (replaces jquery.sticky)
	 *    Wraps #sticker in #sticker-sticky-wrapper and adds .is-sticky
	 *    once you scroll past it, exactly like before.
	 * ------------------------------------------------------------- */
	var sticker = document.getElementById('sticker');
	var wrapper = null;

	if (sticker) {
		wrapper = document.createElement('div');
		wrapper.id = 'sticker-sticky-wrapper';
		wrapper.className = 'sticky-wrapper';
		sticker.parentNode.insertBefore(wrapper, sticker);
		wrapper.appendChild(sticker);

		var setHeight = function () {
			if (!wrapper.classList.contains('is-sticky')) {
				wrapper.style.height = sticker.offsetHeight + 'px';
			}
		};
		setHeight();

		var ticking = false;
		var update = function () {
			ticking = false;
			var top = wrapper.getBoundingClientRect().top;
			if (top <= 0) {
				if (!wrapper.classList.contains('is-sticky')) {
					sticker.style.position = 'fixed';
					sticker.style.top = '0px';
					wrapper.classList.add('is-sticky');
				}
			} else if (wrapper.classList.contains('is-sticky')) {
				sticker.style.position = '';
				sticker.style.top = '';
				wrapper.classList.remove('is-sticky');
			}
		};
		var onScroll = function () {
			if (!ticking) {
				ticking = true;
				window.requestAnimationFrame(update);
			}
		};

		window.addEventListener('scroll', onScroll, { passive: true });
		window.addEventListener('resize', function () { setHeight(); onScroll(); });
		window.addEventListener('load', function () { setHeight(); update(); });
		update();
	}

	/* ---------------------------------------------------------------
	 * 2. Mobile menu toggle (same classes as before)
	 * ------------------------------------------------------------- */
	var toggle = document.querySelector('.nav-toggle');
	var nav = document.querySelector('.header-nav .nav');

	if (toggle && nav) {
		var flip = function () {
			nav.classList.toggle('active');
			if (wrapper) wrapper.classList.toggle('active');
			toggle.classList.toggle('active');
			toggle.setAttribute('aria-expanded', nav.classList.contains('active') ? 'true' : 'false');
		};
		toggle.addEventListener('click', flip);
		toggle.addEventListener('keydown', function (e) {
			if (e.key === 'Enter' || e.key === ' ') {
				e.preventDefault();
				flip();
			}
		});
	}

	/* ---------------------------------------------------------------
	 * 3. Share buttons at the end of posts open in a small centered
	 *    pop-up window (what the old rrssb script did).
	 * ------------------------------------------------------------- */
	document.addEventListener('click', function (e) {
		var link = e.target.closest ? e.target.closest('.rrssb-buttons a.popup') : null;
		if (!link) return;
		var w = 580, h = 470;
		var left = (window.screenX || window.screenLeft || 0) + (window.outerWidth - w) / 2;
		var top = (window.screenY || window.screenTop || 0) + (window.outerHeight - h) / 2;
		var win = window.open(link.href, 'share', 'scrollbars=yes,width=' + w + ',height=' + h + ',top=' + top + ',left=' + left);
		if (win) {
			e.preventDefault();
			win.focus();
		}
	});
})();
