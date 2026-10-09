/*
 * S&P 500 Historical Return Calculator — Of Dollars And Data
 *
 * The numbers live in sp500_data.js (written by the R script each month).
 * This file never needs to change for a data update: the year lists, default
 * dates and the "data through" label are all built from the data.
 *
 * Shareable links: after Calculate, the address bar updates to e.g.
 *   /sp500-calculator/?start=2009-03&end=2010-03&amount=10000
 * Opening a link like that fills in the form and shows those results.
 */
(function () {
	'use strict';

	var MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July',
		'August', 'September', 'October', 'November', 'December'];

	var D = window.SP500_DATA || (typeof SP500_DATA !== 'undefined' ? SP500_DATA : null);
	if (!D) { return; }

	var startY = +D.start.slice(0, 4), startM = +D.start.slice(5, 7);
	var endY = +D.end.slice(0, 4), endM = +D.end.slice(5, 7);
	var N = D.price.length;

	/* ---------- "Compared to History" ---------- */
	// Ranks this result against every period of the same length since the data starts.
	// valueFor(s, e) returns the number to compare (higher = better) for months s..e.
	function lengthLabel(months) {
		var y = Math.floor(months / 12), m = months % 12, parts = [];
		if (y) { parts.push(y + '-year'); }
		if (m) { parts.push(m + '-month'); }
		return parts.join(', ');
	}
	function showHistoryRank(s, e, valueFor, basis) {
		var el = $('history-rank');
		if (!el) { return; }
		var len = e - s, mine = valueFor(s, e), worse = 0, total = 0;
		for (var a = 0; a + len < N; a++) {
			if (a === s) { continue; }
			total++;
			if (valueFor(a, a + len) < mine) { worse++; }
		}
		var row = el.parentNode;
		if (total < 10) {
			row.hidden = true;
			if (row.previousSibling && row.previousSibling.tagName === 'HR') { row.previousSibling.hidden = true; }
			return;
		}
		row.hidden = false;
		if (row.previousSibling && row.previousSibling.tagName === 'HR') { row.previousSibling.hidden = false; }
		var pct = Math.round(worse / total * 100);
		var lead = worse === total ? 'The best of all ' : worse === 0 ? 'The worst of all ' :
			'Better than ' + Math.min(99, Math.max(1, pct)) + '% of all ';
		el.innerText = lead + lengthLabel(len) + ' periods since ' + startY + ' (' + basis + ').';
	}

	/* ---------- Google Analytics events (see inc/analytics.php) ---------- */
	function track(name, params) {
		window.dataLayer = window.dataLayer || [];
		(function () { window.dataLayer.push(arguments); })('event', name, params);
	}

	function $(id) { return document.getElementById(id); }

	/* ---------- number helpers (same formatting as before) ---------- */
	function getNumericValue(v) { return parseFloat(String(v).replace(/,/g, '')) || 0; }
	function formatNumber(num) {
		return num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
	}
	function formatDollar(pct, initial) { return '$' + formatNumber((pct / 100 + 1) * initial); }
	function pad(m) { return (m < 10 ? '0' : '') + m; }
	function monthName(m) { return MONTHS[m - 1]; }

	// Kept as a global so the existing oninput="formatInputNumber(this)" keeps working.
	window.formatInputNumber = function (input) {
		var value = input.value.replace(/[^0-9.]/g, '');
		if (value) { input.value = new Intl.NumberFormat('en-US').format(value); }
	};

	function indexFor(y, m) { return (y - startY) * 12 + (m - startM); }

	/* ---------- build the form from the data ---------- */
	function fillSelect(sel, items, selected) {
		if (!sel) { return; }
		sel.innerHTML = '';
		items.forEach(function (it) {
			var o = document.createElement('option');
			o.value = it.value; o.textContent = it.label;
			if (String(it.value) === String(selected)) { o.selected = true; }
			sel.appendChild(o);
		});
	}

	function buildForm() {
		var months = MONTHS.map(function (name, i) { return { value: pad(i + 1), label: name }; });
		var years = [];
		for (var y = startY; y <= endY; y++) { years.push({ value: String(y), label: String(y) }); }

		// Defaults: December of last year → latest month available (same as before).
		fillSelect($('start-month'), months, '12');
		fillSelect($('start-year'), years, String(endY - 1));
		fillSelect($('end-month'), months, pad(endM));
		fillSelect($('end-year'), years, String(endY));

		var through = $('data-through');
		if (through) {
			through.textContent = 'Data from ' + monthName(startM) + ' ' + startY +
				' through ' + monthName(endM) + ' ' + endY + '.';
		}
	}

	/* ---------- inline error messages ---------- */
	function errorBox() {
		var box = $('calc-error');
		if (!box) {
			// Older page HTML: create the message area under the dates.
			var after = document.querySelector('.calculator .date-container');
			if (!after) { return null; }
			box = document.createElement('p');
			box.id = 'calc-error'; box.className = 'calc-error'; box.setAttribute('role', 'alert');
			after.parentNode.insertBefore(box, after.nextSibling);
		}
		return box;
	}

	function showError(msg, fields) {
		clearError();
		var box = errorBox();
		if (box) { box.textContent = msg; box.hidden = false; }
		(fields || []).forEach(function (id) {
			var el = $(id);
			if (el) { el.classList.add('calc-invalid'); el.setAttribute('aria-invalid', 'true'); }
		});
	}

	function clearError() {
		var box = $('calc-error');
		if (box) { box.textContent = ''; box.hidden = true; }
		document.querySelectorAll('.calc-invalid').forEach(function (el) {
			el.classList.remove('calc-invalid'); el.removeAttribute('aria-invalid');
		});
	}

	/* ---------- the calculation (same math as before) ---------- */
	function compute(sY, sM, eY, eM) {
		var a = indexFor(sY, sM), b = indexFor(eY, eM);
		var start = {
			price: D.price[a], nominalPricePlusDividend: D.nominalPricePlusDividend[a],
			realPrice: D.realPrice[a], realPricePlusDividend: D.realPricePlusDividend[a]
		};
		var end = {
			price: D.price[b], nominalPricePlusDividend: D.nominalPricePlusDividend[b],
			realPrice: D.realPrice[b], realPricePlusDividend: D.realPricePlusDividend[b]
		};
		var n = (new Date(eY + '-' + pad(eM) + '-01') - new Date(sY + '-' + pad(sM) + '-01')) /
			(1000 * 60 * 60 * 24 * 365.25);

		function total(k) { return ((end[k] - start[k]) / start[k]) * 100; }
		function annual(k) { return (Math.pow(end[k] / start[k], 1 / n) - 1) * 100; }

		var realRatio = end.realPricePlusDividend / start.realPricePlusDividend;
		return {
			nominalPrice: { total: total('price'), annual: annual('price') },
			nominalTotal: { total: total('nominalPricePlusDividend'), annual: annual('nominalPricePlusDividend') },
			realPrice: { total: total('realPrice'), annual: annual('realPrice') },
			realTotal: {
				total: total('realPricePlusDividend'),
				annual: (Math.pow(Math.abs(realRatio), 1 / n) - 1) * 100 * (realRatio < 0 ? -1 : 1)
			}
		};
	}

	/* ---------- show the results ---------- */
	function render(r, amount, sY, sM, eY, eM) {
		var box = $('sp500-results');

		if (!box) {
			// Older page HTML: fill the original result lines.
			var set = function (id, v) { var el = $(id); if (el) { el.innerText = v; } };
			set('nominal-price-return', formatNumber(r.nominalPrice.total));
			set('annualized-nominal-price-return', formatNumber(r.nominalPrice.annual));
			set('nominal-price-dollar', formatDollar(r.nominalPrice.total, amount));
			set('nominal-total-return', formatNumber(r.nominalTotal.total));
			set('annualized-nominal-total-return', formatNumber(r.nominalTotal.annual));
			set('nominal-total-dollar', formatDollar(r.nominalTotal.total, amount));
			set('real-price-return', formatNumber(r.realPrice.total));
			set('annualized-real-price-return', formatNumber(r.realPrice.annual));
			set('real-price-dollar', formatDollar(r.realPrice.total, amount));
			set('real-total-return', formatNumber(r.realTotal.total));
			set('annualized-real-total-return', formatNumber(r.realTotal.annual));
			set('real-total-dollar', formatDollar(r.realTotal.total, amount));
			var out = $('calc-output');
			if (out) { out.classList.remove('is-empty'); }
			return;
		}

		var amt = '$' + amount.toLocaleString('en-US', { maximumFractionDigits: 2 });
		var period = monthName(sM) + ' ' + sY + ' to ' + monthName(eM) + ' ' + eY;

		function row(label, x) {
			return '<tr><th scope="row">' + label + '</th>' +
				'<td data-label="Total return">' + formatNumber(x.total) + '%</td>' +
				'<td data-label="Annualized">' + formatNumber(x.annual) + '%</td>' +
				'<td data-label="Grew to">' + formatDollar(x.total, amount) + '</td></tr>';
		}

		box.innerHTML =
			'<p class="sp500-headline">' + amt + ' invested in ' + monthName(sM) + ' ' + sY +
			' grew to <strong>' + formatDollar(r.nominalTotal.total, amount) + '</strong> by ' +
			monthName(eM) + ' ' + eY + ' with dividends reinvested (' +
			formatDollar(r.realTotal.total, amount) + ' after inflation).</p>' +
			'<table class="sp500-table">' +
			'<caption class="screen-reader-text">S&amp;P 500 returns, ' + period + '</caption>' +
			'<thead><tr><td></td><th scope="col">Total return</th><th scope="col">Annualized</th>' +
			'<th scope="col">' + amt + ' grew to</th></tr></thead>' +
			'<tbody><tr class="sp500-group"><th colspan="4" scope="rowgroup">Nominal</th></tr>' +
			row('Price only', r.nominalPrice) +
			row('With dividends reinvested', r.nominalTotal) +
			'</tbody><tbody><tr class="sp500-group"><th colspan="4" scope="rowgroup">Inflation-adjusted</th></tr>' +
			row('Price only', r.realPrice) +
			row('With dividends reinvested', r.realTotal) +
			'</tbody></table>' +
			'<p class="sp500-share"><button type="button" class="sp500-copy">Copy link to these results</button>' +
			'<span class="sp500-copied" aria-live="polite"></span></p>';

		wireCopy(box);
	}

	// "Copy link to these results" button (copies the address bar, which holds the dates and amount).
	function wireCopy(scope) {
		var copy = scope.querySelector('.sp500-copy');
		if (!copy || copy.dataset.wired) { return; }
		copy.dataset.wired = '1';
		copy.addEventListener('click', function () {
			track('calculator_copy_link', { calculator: 'sp500' });
			var url = window.location.href, done = scope.querySelector('.sp500-copied');
			var ok = function () { if (done) { done.textContent = 'Link copied'; setTimeout(function () { done.textContent = ''; }, 2500); } };
			if (navigator.clipboard && navigator.clipboard.writeText) {
				navigator.clipboard.writeText(url).then(ok, function () { window.prompt('Copy this link:', url); });
			} else {
				window.prompt('Copy this link:', url);
			}
		});
	}

	/* ---------- Calculate ---------- */
	function calculate(fromLink) {
		clearError();
		var sM = +$('start-month').value, sY = +$('start-year').value;
		var eM = +$('end-month').value, eY = +$('end-year').value;
		var amount = getNumericValue($('initialInvestment') ? $('initialInvestment').value : 10000);

		if (sY > eY || (sY === eY && sM >= eM)) {
			showError('The end month must be after the start month.', ['end-month', 'end-year']);
			return;
		}
		if (indexFor(eY, eM) >= D.price.length) {
			showError('Data is only available through ' + monthName(endM) + ' ' + endY + '.', ['end-month', 'end-year']);
			return;
		}
		if (indexFor(sY, sM) < 0) {
			showError('Data starts in ' + monthName(startM) + ' ' + startY + '.', ['start-month', 'start-year']);
			return;
		}
		if (!(amount > 0)) {
			showError('Enter an initial investment greater than $0.', ['initialInvestment']);
			return;
		}

		render(compute(sY, sM, eY, eM), amount, sY, sM, eY, eM);
		showHistoryRank(indexFor(sY, sM), indexFor(eY, eM), function (x, y) {
			return D.realPricePlusDividend[y] / D.realPricePlusDividend[x];
		}, 'inflation-adjusted, with dividends reinvested');

		// Update the address bar so this exact result can be shared.
		if (window.history && history.replaceState) {
			var q = '?start=' + sY + '-' + pad(sM) + '&end=' + eY + '-' + pad(eM) + '&amount=' + Math.round(amount);
			history.replaceState(null, '', window.location.pathname + q);
		}

		track('calculator_run', {
			calculator: 'sp500',
			trigger: fromLink ? 'shared_link' : 'button',
			start_month: sY + '-' + pad(sM),
			end_month: eY + '-' + pad(eM),
			period_years: Math.round((indexFor(eY, eM) - indexFor(sY, sM)) / 12 * 10) / 10,
			initial_amount: Math.round(amount)
		});
	}
	// Kept as a global so the existing onclick="calculateReturns()" keeps working.
	window.calculateReturns = function () { calculate(false); };

	/* ---------- shared links: ?start=YYYY-MM&end=YYYY-MM&amount=N ---------- */
	function applyLink() {
		var p = new URLSearchParams(window.location.search);
		var s = /^(\d{4})-(\d{2})$/.exec(p.get('start') || ''), e = /^(\d{4})-(\d{2})$/.exec(p.get('end') || '');
		if (!s || !e) { return; }
		var set = function (id, v) { var el = $(id); if (el && el.querySelector('option[value="' + v + '"]')) { el.value = v; } };
		set('start-year', s[1]); set('start-month', s[2]);
		set('end-year', e[1]); set('end-month', e[2]);
		var amt = parseFloat(p.get('amount'));
		if (amt > 0 && $('initialInvestment')) { $('initialInvestment').value = amt.toLocaleString('en-US'); }
		calculate(true);
	}

	function init() {
		buildForm();
		if ($('calc-output')) { wireCopy($('calc-output')); }
		var btn = $('calculate-btn');
		if (btn) { btn.addEventListener('click', function () { calculate(false); }); }
		['start-month', 'start-year', 'end-month', 'end-year', 'initialInvestment'].forEach(function (id) {
			var el = $(id); if (el) { el.addEventListener(el.tagName === 'SELECT' ? 'change' : 'input', clearError); }
		});
		applyLink();
	}

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', init);
	} else {
		init();
	}
})();
