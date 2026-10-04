/*
 * S&P 500 DCA Calculator — Of Dollars And Data
 *
 * Uses the shared sp500_data.js (written by the R script each month). This file
 * never needs to change for a data update.
 *
 * Dates work like the S&P 500 calculator: you invest at the end of the Start
 * Month and the calculator counts the returns after it, through the End Month.
 * Monthly investments are made at the end of each month after the start month,
 * up to the month before the end month.
 *
 * Shareable links: after Calculate, the address bar updates to e.g.
 *   ?start=2015-12&end=2025-12&initial=10000&monthly=1000&inflation=1
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

	// Monthly returns from the index levels (dividends reinvested).
	var nominalReturn = [0], realReturn = [0];
	for (var k = 1; k < N; k++) {
		nominalReturn.push(D.nominalPricePlusDividend[k] / D.nominalPricePlusDividend[k - 1] - 1);
		realReturn.push(D.realPricePlusDividend[k] / D.realPricePlusDividend[k - 1] - 1);
	}

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
	function pad(m) { return (m < 10 ? '0' : '') + m; }
	function monthName(m) { return MONTHS[m - 1]; }
	function indexFor(y, m) { return (y - startY) * 12 + (m - startM); }
	function monthLabel(i) {
		var t = startM - 1 + i;
		return (startY + Math.floor(t / 12)) + '-' + pad(t % 12 + 1) + '-01';
	}

	/* ---------- number helpers (same formatting as before) ---------- */
	function getNumericValue(v) { return parseFloat(String(v).replace(/,/g, '')) || 0; }
	function formatNumber(num) {
		return num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
	}
	function formatNumberNoDecimals(num) {
		return num.toLocaleString('en-US', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
	}
	function formatDCADollar(value) { return '$' + formatNumber(value); }

	window.formatInputNumber = function (input) {
		var value = input.value.replace(/[^0-9.]/g, '');
		if (value) { input.value = new Intl.NumberFormat('en-US').format(value); }
	};

	function calculateIRR(cashflows) {
		var min = -0.5, max = 1.0, guess, NPV, iteration = 0;
		do {
			guess = (min + max) / 2;
			NPV = 0;
			for (var j = 0; j < cashflows.length; j++) {
				NPV += cashflows[j] / Math.pow((1 + guess), j);
			}
			if (NPV > 0) { min = guess; } else { max = guess; }
			iteration++;
			if (iteration >= 1000) { return null; }
		} while (Math.abs(NPV) > 0.000001);
		return Math.pow(1 + guess, 12) - 1;
	}

	function dynamicCeil(number) {
		if (number === 0) { return 0; }
		var magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(number))));
		return Math.ceil(number / magnitude) * magnitude;
	}

	/* ---------- form ---------- */
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

	/* ---------- chart ---------- */
	var chart = null;
	function drawChart(labels, contributions, nominal, real, title) {
		if (!window.Chart || !$('myChart')) { return; }
		if (chart) { chart.destroy(); }
		var yAxisMax = dynamicCeil(Math.max(Math.max.apply(null, nominal), Math.max.apply(null, contributions)));
		chart = new Chart($('myChart').getContext('2d'), {
			type: 'line',
			data: {
				labels: labels,
				datasets: [
					{ label: 'Total Contributions', data: contributions, borderColor: '#000000', fill: false, tension: 0 },
					{ label: 'Nominal Value (with dividends reinvested)', data: nominal, borderColor: '#349800', fill: false, tension: 0 },
					{ label: 'Inflation-Adjusted Value (with dividends reinvested)', data: real, borderColor: '#d95f02', fill: false, tension: 0 }
				]
			},
			options: {
				responsive: true,
				maintainAspectRatio: window.innerWidth > 767,
				title: { display: true, text: title, fontSize: 16 },
				scales: {
					xAxes: [{
						type: 'time',
						time: { unit: 'month', displayFormats: { month: 'MM/YYYY' } },
						scaleLabel: { display: true, labelString: 'Date' }
					}],
					yAxes: [{
						ticks: {
							beginAtZero: true,
							max: yAxisMax,
							callback: function (value) {
								return yAxisMax < 10 ? '$' + value.toFixed(2) : '$' + value.toLocaleString();
							}
						}
					}]
				},
				tooltips: {
					callbacks: {
						title: function (items) { return window.moment ? moment(items[0].xLabel).format('MM/YYYY') : items[0].xLabel; },
						labelColor: function (item, c) {
							var col = c.data.datasets[item.datasetIndex].borderColor;
							return { borderColor: col, backgroundColor: col };
						},
						label: function (item, data) {
							var label = data.datasets[item.datasetIndex].label || '';
							if (label) { label += ': '; }
							return label + '$' + parseFloat(item.yLabel).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
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
		var sM = +$('start-month').value, sY = +$('start-year').value;
		var eM = +$('end-month').value, eY = +$('end-year').value;
		var initialInvestment = getNumericValue($('initial-investment').value);
		var monthlyInvestment = getNumericValue($('monthly-investment').value);
		var adjustForInflation = $('adjust-for-inflation').checked;

		var s = indexFor(sY, sM), e = indexFor(eY, eM);
		if (e <= s) {
			showError('The end month must be after the start month.', ['end-month', 'end-year']);
			return;
		}
		if (e >= N) {
			showError('Data is only available through ' + monthName(endM) + ' ' + endY + '.', ['end-month', 'end-year']);
			return;
		}
		if (s < 0) {
			showError('Data starts in ' + monthName(startM) + ' ' + startY + '.', ['start-month', 'start-year']);
			return;
		}
		if (!(initialInvestment > 0) && !(monthlyInvestment > 0)) {
			showError('Enter an initial or monthly investment greater than $0.', ['initial-investment', 'monthly-investment']);
			return;
		}

		var nominalValue = initialInvestment, realValue = initialInvestment;
		var contribution = monthlyInvestment, totalContributions = initialInvestment;
		var nominalCashflows = [-initialInvestment], realCashflows = [-initialInvestment];

		// Chart starts at the start month with the initial investment.
		var labels = [monthLabel(s)], contributionsArr = [initialInvestment];
		var nominalArr = [initialInvestment], realArr = [initialInvestment];

		for (var i = s + 1; i <= e; i++) {
			if (i > s + 1) {
				// Monthly investment, optionally grown with inflation.
				if (adjustForInflation) { contribution = monthlyInvestment * (D.cpi[i] / D.cpi[s + 1]); }
				nominalCashflows.push(-contribution);
				realCashflows.push(-contribution);
				nominalValue += contribution;
				realValue += contribution;
				totalContributions += contribution;
			}
			nominalValue *= (1 + nominalReturn[i]);
			realValue *= (1 + realReturn[i]);

			labels.push(monthLabel(i));
			contributionsArr.push(totalContributions);
			nominalArr.push(nominalValue);
			realArr.push(realValue);
		}
		nominalCashflows.push(nominalValue);
		realCashflows.push(realValue);

		$('total-contributions').innerText = formatDCADollar(totalContributions);
		$('final-value-nominal-dollars').innerText = formatDCADollar(nominalValue);
		$('final-value-real-dollars').innerText = formatDCADollar(realValue);
		var nomIRR = calculateIRR(nominalCashflows), realIRR = calculateIRR(realCashflows);
		$('nom_irr').innerText = nomIRR === null ? 'n/a' : (nomIRR * 100).toFixed(2) + '%';
		$('real_irr').innerText = realIRR === null ? 'n/a' : (realIRR * 100).toFixed(2) + '%';

		// Same investments over every other period of this length, compared on inflation-adjusted final value.
		showHistoryRank(s, e, function (a0, b0) {
			var v = initialInvestment, c = monthlyInvestment;
			for (var j = a0 + 1; j <= b0; j++) {
				if (j > a0 + 1) {
					if (adjustForInflation) { c = monthlyInvestment * (D.cpi[j] / D.cpi[a0 + 1]); }
					v += c;
				}
				v *= (1 + realReturn[j]);
			}
			return v;
		}, 'inflation-adjusted, with dividends reinvested');

		var out = $('calc-output');
		if (out) { out.hidden = false; }

		drawChart(labels, contributionsArr, nominalArr, realArr, [
			'U.S. Stock DCA Calculator',
			'Initial Investment: $' + formatNumberNoDecimals(initialInvestment),
			'Monthly Investment: $' + formatNumberNoDecimals(monthlyInvestment),
			monthName(sM) + ' ' + sY + ' - ' + monthName(eM) + ' ' + eY
		]);

		if (window.history && history.replaceState) {
			var q = '?start=' + sY + '-' + pad(sM) + '&end=' + eY + '-' + pad(eM) +
				'&initial=' + Math.round(initialInvestment) + '&monthly=' + Math.round(monthlyInvestment) +
				(adjustForInflation ? '&inflation=1' : '');
			history.replaceState(null, '', window.location.pathname + q);
		}

		track('calculator_run', {
			calculator: 'sp500_dca',
			trigger: fromLink ? 'shared_link' : 'button',
			start_month: sY + '-' + pad(sM),
			end_month: eY + '-' + pad(eM),
			period_years: Math.round((indexFor(eY, eM) - indexFor(sY, sM)) / 12 * 10) / 10,
			initial_amount: Math.round(initialInvestment),
			monthly_amount: Math.round(monthlyInvestment),
			inflation_adjusted: adjustForInflation ? 'yes' : 'no'
		});
	}
	window.calculateDCAReturns = function () { calculate(false); };

	/* ---------- shared links ---------- */
	function applyLink() {
		var p = new URLSearchParams(window.location.search);
		var st = /^(\d{4})-(\d{2})$/.exec(p.get('start') || ''), en = /^(\d{4})-(\d{2})$/.exec(p.get('end') || '');
		if (!st || !en) { return; }
		var set = function (id, v) { var el = $(id); if (el && el.querySelector('option[value="' + v + '"]')) { el.value = v; } };
		set('start-year', st[1]); set('start-month', st[2]);
		set('end-year', en[1]); set('end-month', en[2]);
		var money = function (id, v) { v = parseFloat(v); if (v >= 0 && $(id)) { $(id).value = v.toLocaleString('en-US'); } };
		money('initial-investment', p.get('initial'));
		money('monthly-investment', p.get('monthly'));
		if ($('adjust-for-inflation')) { $('adjust-for-inflation').checked = p.get('inflation') === '1'; }
		calculate(true);
	}

	/* ---------- "Copy link to these results" ---------- */
	function wireCopy(scope) {
		var copy = scope && scope.querySelector('.sp500-copy');
		if (!copy || copy.dataset.wired) { return; }
		copy.dataset.wired = '1';
		copy.addEventListener('click', function () {
			track('calculator_copy_link', { calculator: 'sp500_dca' });
			var url = window.location.href, done = scope.querySelector('.sp500-copied');
			var ok = function () { if (done) { done.textContent = 'Link copied'; setTimeout(function () { done.textContent = ''; }, 2500); } };
			if (navigator.clipboard && navigator.clipboard.writeText) {
				navigator.clipboard.writeText(url).then(ok, function () { window.prompt('Copy this link:', url); });
			} else {
				window.prompt('Copy this link:', url);
			}
		});
	}

	function init() {
		buildForm();
		wireCopy($('calc-output'));
		var btn = $('calculate-btn');
		if (btn) { btn.addEventListener('click', function () { calculate(false); }); }
		['start-month', 'start-year', 'end-month', 'end-year', 'initial-investment', 'monthly-investment'].forEach(function (id) {
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
