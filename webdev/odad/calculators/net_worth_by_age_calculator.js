/*
 * Net Worth by Age Calculator — Of Dollars And Data
 *
 * Uses net_worth_data.js (written by the R script). This file never needs to
 * change for a data update: the age groups and the "Data from the ... Survey of
 * Consumer Finances" line come from the data file.
 *
 * Shareable links: after Calculate, the address bar updates to e.g.
 *   ?age=30-34&nw=90000
 */
(function () {
	'use strict';

	var D = window.NW_DATA || (typeof NW_DATA !== 'undefined' ? NW_DATA : null);
	if (!D) { return; }

	function $(id) { return document.getElementById(id); }

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
			sel.innerHTML = '<option value="">Select age group</option>';
			D.groups.forEach(function (g) {
				var o = document.createElement('option');
				o.value = g; o.textContent = g;
				sel.appendChild(o);
			});
		}
		var note = $('data-through');
		if (note) { note.textContent = 'Data from the ' + D.year + ' Survey of Consumer Finances.'; }
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
					label: 'U.S. Household Net Worth',
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
				title: { display: true, text: ['U.S. Household Net Worth by Percentile', 'Age Group: ' + ageGroup], fontSize: 16 },
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
			showError('Select your age group.', ['nw-age']);
			return;
		}
		if (raw === '' || raw === '-') {
			showError('Enter your household net worth.', ['net-worth']);
			return;
		}

		// Same matching as before: the highest percentile whose net worth is at or below yours.
		var values = D.values[ageGroup];
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

		$('nw-percentile').innerText = percentile;
		var out = $('calc-output');
		if (out) { out.classList.remove('is-empty'); }
		drawChart(ageGroup, values, userIndex);

		if (window.history && history.replaceState) {
			var q = '?age=' + encodeURIComponent(ageGroup) + '&nw=' + Math.round(netWorth);
			history.replaceState(null, '', window.location.pathname + q);
		}

		track('calculator_run', {
			calculator: 'net_worth',
			trigger: fromLink ? 'shared_link' : 'button',
			age_group: ageGroup,
			amount: Math.round(netWorth),
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
		if (!age || nw === null || !D.values[age]) { return; }
		$('nw-age').value = age;
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
