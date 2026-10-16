/*
 * Net Worth by Age Calculator — Of Dollars And Data
 *
 * Uses net_worth_data.js (written by the R script). This file never needs to
 * change for a data update: the ages and the +/- window come from the data file.
 *
 * Shareable links: after Calculate, the address bar updates to e.g.
 *   ?age=32&nw=90000          (home equity included, the default)
 *   ?age=32&nw=90000&home=0   (home equity excluded)
 * Older bracket links (?age=30-34) are mapped to the bracket's middle age.
 *
 * "Include Home Equity?" dropdown: added by this script under the net worth
 * field, but only when the data file has values_ex_home (net worth minus
 * home equity). With an older data file the calculator works as before.
 */
(function () {
	'use strict';

	var D = window.NW_DATA || (typeof NW_DATA !== 'undefined' ? NW_DATA : null);
	if (!D) { return; }

	function $(id) { return document.getElementById(id); }

	/* ---------- home equity option ---------- */
	var HAS_EX_HOME = !!D.values_ex_home;
	var NW_LABEL = 'Household Net Worth:';
	var NW_LABEL_EX = 'Household Net Worth (excluding home equity):';
	var RESULT_LABEL = 'Your Net Worth Percentile: ';
	var RESULT_LABEL_EX = 'Your Net Worth Percentile (excluding home equity): ';
	var HOME_NOTE = ' Home equity is the value of your primary residence minus any mortgage or home equity loan on it.';

	function includeHome() {
		var sel = $('nw-home');
		return !sel || sel.value !== '0';
	}
	function valuesFor(age) {
		return includeHome() ? D.values[age] : D.values_ex_home[age];
	}

	/* ---------- age labels ---------- */
	// "36" -> "36 (34–38)"; "All Ages" stays as is.
	function ageLabel(g) {
		var n = parseInt(g, 10), w = D.window || 0;
		if (isNaN(n) || !w) { return g; }
		return g + ' (' + (n - w) + '–' + (n + w) + ')';
	}
	function titleLabel(g) {
		return isNaN(parseInt(g, 10)) ? g : 'Age: ' + ageLabel(g);
	}

	/* ---------- Google Analytics events (see inc/analytics.php) ---------- */
	function track(name, params) {
		window.dataLayer = window.dataLayer || [];
		(function () { window.dataLayer.push(arguments); })('event', name, params);
	}

	/* ---------- number helpers (same as before) ---------- */
	function getNumericValue(v) { return parseFloat(String(v).replace(/,/g, '')) || 0; }

	// Same as before, but a leading minus sign is kept (net worth can be negative).
	window.formatInputNumber = function (input) {
		var negative = /^\s*-/.test(input.value);
		var value = input.value.replace(/[^0-9.]/g, '');
		if (value) {
			input.value = (negative ? '-' : '') + new Intl.NumberFormat('en-US').format(value);
		} else {
			input.value = negative ? '-' : '';
		}
	};

	function calculateRoundedMax(maxValue) {
		if (maxValue >= 1000000) { return Math.ceil(maxValue / 100000) * 100000; }
		if (maxValue >= 100000) { return Math.ceil(maxValue / 10000) * 10000; }
		return Math.ceil(maxValue / 1000) * 1000;
	}

	function calculateStepSize(maxValue) {
		if (maxValue >= 10000000) { return Math.ceil(maxValue / 10 / 1000000) * 1000000; }
		if (maxValue >= 1000000) { return Math.ceil(maxValue / 10 / 500000) * 500000; }
		return Math.ceil(maxValue / 10 / 100000) * 100000;
	}

	/* ---------- form ---------- */
	function buildForm() {
		var sel = $('nw-age');
		if (sel) {
			sel.innerHTML = '<option value="">Select your age</option>';
			D.groups.forEach(function (g) {
				var o = document.createElement('option');
				o.value = g; o.textContent = ageLabel(g);
				sel.appendChild(o);
			});
		}
		// Label reads "Your Age:" (the reader picks an age; the window is a sample detail).
		var lab = document.querySelector('label[for="nw-age"]');
		if (lab) { lab.textContent = 'Your Age:'; }
		if (HAS_EX_HOME && !$('nw-home')) {
			var nw = $('net-worth');
			var hl = document.createElement('label');
			hl.setAttribute('for', 'nw-home');
			hl.textContent = 'Include Home Equity?';
			var hs = document.createElement('select');
			hs.id = 'nw-home'; hs.name = 'nw-home';
			hs.innerHTML = '<option value="1">Yes</option><option value="0">No</option>';
			if (nw && nw.parentNode) {
				nw.parentNode.insertBefore(hl, nw.nextSibling);
				nw.parentNode.insertBefore(hs, hl.nextSibling);
			}
			// Changing it after a result re-runs the calculation, so the result,
			// its label and the chart always match the dropdown.
			hs.addEventListener('change', function () {
				clearError();
				updateLabels(false);
				var out = $('calc-output');
				if (out && !out.classList.contains('is-empty')) { calculate('home_toggle'); }
			});
		}
		updateLabels(true);
	}

	// Labels and the note below the button follow the home equity choice.
	// The result label only changes when a result is (re)calculated.
	function updateLabels(withResult) {
		var inc = includeHome();
		var nl = document.querySelector('label[for="net-worth"]');
		if (nl) { nl.textContent = inc ? NW_LABEL : NW_LABEL_EX; }
		var res = $('nw-percentile');
		var strong = res && res.parentNode ? res.parentNode.querySelector('strong') : null;
		if (strong && withResult) { strong.textContent = inc ? RESULT_LABEL : RESULT_LABEL_EX; }
		var note = $('data-through');
		if (note) {
			// The survey and year are already named in the page text above the calculator.
			var text = (D.window ? 'Each age includes households within ' + D.window + ' year' + (D.window === 1 ? '' : 's') + ' of that age to ensure a sufficient sample size.' : '') +
				(inc ? '' : HOME_NOTE);
			note.textContent = text.trim();
			note.hidden = !note.textContent;
		}
	}

	/* ---------- inline errors ---------- */
	function showError(msg, fields) {
		clearError();
		var box = $('calc-error');
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

	/* ---------- chart (same look as before) ---------- */
	var chart = null;
	function drawChart(ageGroup, values, userIndex) {
		if (!window.Chart || !$('myChart')) { return; }
		if (chart) { chart.destroy(); }

		var yAxisMax = calculateRoundedMax(Math.max.apply(null, values));
		var stepSize = calculateStepSize(yAxisMax);
		var marks = { 25: true, 50: true, 75: true };
		var styles = D.pct.map(function (p, i) {
			if (i === userIndex) { return { radius: 9, bg: 'black', border: 'black', style: 'circle' }; }
			if (marks[p]) { return { radius: 9, bg: '#349800', border: '#349800', style: 'triangle' }; }
			return { radius: 3, bg: 'transparent', border: '#349800', style: 'circle' };
		});

		chart = new Chart($('myChart').getContext('2d'), {
			type: 'line',
			data: {
				labels: D.pct.map(function (p) { return p + '%'; }),
				datasets: [{
					label: includeHome() ? 'U.S. Household Net Worth' : 'Net Worth Excl. Home Equity',
					data: values,
					borderColor: '#349800',
					fill: false,
					tension: 0,
					pointRadius: styles.map(function (s) { return s.radius; }),
					pointBackgroundColor: styles.map(function (s) { return s.bg; }),
					pointBorderColor: styles.map(function (s) { return s.border; }),
					pointStyle: styles.map(function (s) { return s.style; })
				}]
			},
			options: {
				responsive: true,
				maintainAspectRatio: window.innerWidth > 767,
				title: {
					display: true,
					text: includeHome()
						? ['U.S. Household Net Worth by Percentile', titleLabel(ageGroup)]
						: ['U.S. Household Net Worth by Percentile', 'Excluding Home Equity', titleLabel(ageGroup)],
					fontSize: 16
				},
				scales: {
					xAxes: [{ type: 'category', scaleLabel: { display: true, labelString: 'Percentile' } }],
					yAxes: [{
						ticks: {
							beginAtZero: true,
							max: yAxisMax,
							stepSize: stepSize,
							callback: function (value) { return '$' + value.toLocaleString(); }
						}
					}]
				},
				tooltips: {
					callbacks: {
						labelColor: function (item, c) {
							var col = c.data.datasets[item.datasetIndex].borderColor;
							return { borderColor: col, backgroundColor: col };
						},
						label: function (item, data) {
							var label = data.datasets[item.datasetIndex].label || '';
							if (label) { label += ': '; }
							return label + '$' + parseFloat(item.yLabel).toLocaleString('en-US', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
						}
					}
				}
			}
		});
	}
	window.addEventListener('resize', function () {
		if (chart) { chart.options.maintainAspectRatio = window.innerWidth > 767; chart.resize(); }
	});

	/* ---------- Calculate ---------- */
	function calculate(fromLink) {
		clearError();
		var ageGroup = $('nw-age').value;
		var raw = $('net-worth').value.trim();
		var netWorth = getNumericValue(raw);

		if (!ageGroup || !D.values[ageGroup]) {
			showError('Select your age.', ['nw-age']);
			return;
		}
		if (raw === '' || raw === '-') {
			showError('Enter your household net worth.', ['net-worth']);
			return;
		}

		// Same matching as before: the highest percentile whose net worth is at or below yours.
		var values = valuesFor(ageGroup);
		var userIndex = -1, best = null;
		for (var i = 0; i < values.length; i++) {
			if (values[i] <= netWorth) {
				if (best === null || Math.abs(values[i] - netWorth) < Math.abs(best - netWorth) ||
					(Math.abs(values[i] - netWorth) === Math.abs(best - netWorth) && D.pct[i] > D.pct[userIndex])) {
					best = values[i]; userIndex = i;
				}
			}
		}
		var percentile = userIndex === -1 ? 'Below ' + D.pct[0] + '%' : D.pct[userIndex] + '%';

		updateLabels(true);
		$('nw-percentile').innerText = percentile;
		var out = $('calc-output');
		if (out) { out.classList.remove('is-empty'); }
		drawChart(ageGroup, values, userIndex);

		if (window.history && history.replaceState) {
			var q = '?age=' + encodeURIComponent(ageGroup) + '&nw=' + Math.round(netWorth) +
				(includeHome() ? '' : '&home=0');
			history.replaceState(null, '', window.location.pathname + q);
		}

		track('calculator_run', {
			calculator: 'net_worth',
			trigger: fromLink === true ? 'shared_link' : (fromLink || 'button'),
			age_group: ageGroup,
			amount: Math.round(netWorth),
			home_equity: includeHome() ? 'included' : 'excluded',
			result: percentile
		});
	}
	window.calculateNWPercentile = function () { calculate(false); };

	/* ---------- "Copy link to these results" ---------- */
	function wireCopy(scope) {
		var copy = scope && scope.querySelector('.sp500-copy');
		if (!copy || copy.dataset.wired) { return; }
		copy.dataset.wired = '1';
		copy.addEventListener('click', function () {
			track('calculator_copy_link', { calculator: 'net_worth' });
			var url = window.location.href, done = scope.querySelector('.sp500-copied');
			var ok = function () { if (done) { done.textContent = 'Link copied'; setTimeout(function () { done.textContent = ''; }, 2500); } };
			if (navigator.clipboard && navigator.clipboard.writeText) {
				navigator.clipboard.writeText(url).then(ok, function () { window.prompt('Copy this link:', url); });
			} else {
				window.prompt('Copy this link:', url);
			}
		});
	}

	/* ---------- shared links ---------- */
	function applyLink() {
		var p = new URLSearchParams(window.location.search);
		var age = p.get('age'), nw = p.get('nw');
		// Old bracket links (e.g. "30-34") open at the bracket's middle age ("32").
		var m = age && age.match(/^(\d+)-(\d+)$/);
		if (m) { age = String(Math.round((+m[1] + +m[2]) / 2)); }
		if (!age || nw === null || !D.values[age]) { return; }
		$('nw-age').value = age;
		if (p.get('home') === '0' && $('nw-home')) { $('nw-home').value = '0'; }
		var v = parseFloat(nw);
		if (!isNaN(v)) { $('net-worth').value = v.toLocaleString('en-US'); }
		calculate(true);
	}

	function init() {
		buildForm();
		wireCopy($('calc-output'));
		var btn = $('calculate-btn');
		if (btn) { btn.addEventListener('click', function () { calculate(false); }); }
		['nw-age', 'net-worth'].forEach(function (id) {
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
