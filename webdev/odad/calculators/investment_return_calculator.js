/*
 * Investment Return Calculator — Of Dollars And Data
 *
 * Contributions are made at the beginning of each month and everything compounds
 * monthly at (1 + expected annual return)^(1/12). Current age 30 to retirement
 * age 65 = 420 months = 420 monthly contributions.
 *
 * Shareable links: after Calculate, the address bar updates to e.g.
 *   ?age=30&retire=65&amount=10000&monthly=1000&return=7
 */
(function () {
	'use strict';

	function $(id) { return document.getElementById(id); }

	/* ---------- number helpers (same as before) ---------- */
	function getNumericValue(v) { return parseFloat(String(v).replace(/,/g, '')) || 0; }
	function formatNumber(num) {
		return num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
	}
	function dynamicCeil(number) {
		if (number === 0) { return 0; }
		var magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(number))));
		return Math.ceil(number / magnitude) * magnitude;
	}

	window.formatInputNumber = function (input) {
		var value = input.value.replace(/[^0-9.]/g, '');
		if (value) { input.value = new Intl.NumberFormat('en-US').format(value); }
	};

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
	function drawChart(labels, data, finalAmount, title) {
		if (!window.Chart || !$('myChart')) { return; }
		if (chart) { chart.destroy(); }
		var yAxisMax = dynamicCeil(finalAmount);
		chart = new Chart($('myChart').getContext('2d'), {
			type: 'line',
			data: {
				labels: labels,
				datasets: [{
					label: 'Estimated Investment Amount',
					data: data,
					borderColor: '#349800',
					backgroundColor: '#349800',
					fill: false
				}]
			},
			options: {
				responsive: true,
				maintainAspectRatio: window.innerWidth > 767,
				title: { display: true, text: title, fontSize: 16 },
				scales: {
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
						label: function (item, d) {
							var label = d.datasets[item.datasetIndex].label || '';
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
	function calculate() {
		clearError();
		var currentAge = parseInt($('current-age').value, 10);
		var retirementAge = parseInt($('retirement-age').value, 10);
		var currentAmount = getNumericValue($('current-amount').value);
		var monthly = getNumericValue($('monthly-contributions').value);
		var returnRaw = $('expected-return').value.trim();
		var expectedReturn = parseFloat(returnRaw) / 100;

		if (isNaN(currentAge) || currentAge < 18 || currentAge > 80) {
			showError('Current age must be between 18 and 80.', ['current-age']);
			return;
		}
		if (isNaN(retirementAge) || retirementAge < 18 || retirementAge > 100) {
			showError('Retirement age must be between 18 and 100.', ['retirement-age']);
			return;
		}
		if (retirementAge <= currentAge) {
			showError('Retirement age must be at least 1 year greater than the current age.', ['retirement-age']);
			return;
		}
		if (returnRaw === '' || isNaN(expectedReturn)) {
			showError('Enter the expected annual return (e.g. 2 = 2%, 4.1 = 4.1%).', ['expected-return']);
			return;
		}
		if (expectedReturn < 0 || expectedReturn > 0.5) {
			showError('Expected annual return must be between 0% and 50%.', ['expected-return']);
			return;
		}

		var numberOfMonths = (retirementAge - currentAge) * 12;
		var monthlyReturn = Math.pow(1 + expectedReturn, 1 / 12) - 1;
		var total = currentAmount, contributions = 0;
		var labels = ['Age ' + currentAge], data = [currentAmount];

		// Each month: add the contribution at the beginning of the month, then grow it.
		for (var i = 1; i <= numberOfMonths; i++) {
			total = (total + monthly) * (1 + monthlyReturn);
			contributions += monthly;
			if (i % 12 === 0) {
				labels.push('Age ' + (currentAge + i / 12));
				data.push(total);
			}
		}

		$('total-contributions').innerText = '$' + formatNumber(contributions);
		$('final-amount').innerText = '$' + formatNumber(total);

		var out = $('calc-output');
		if (out) { out.classList.remove('is-empty'); }

		drawChart(labels, data, total, [
			'Investment Return Calculator',
			'Age: ' + currentAge + '—' + retirementAge,
			'Current Amount Invested: $' + formatNumber(currentAmount),
			'Monthly Contribution: $' + formatNumber(monthly),
			'Expected Annual Return: ' + (expectedReturn * 100).toFixed(2) + '%'
		]);

		if (window.history && history.replaceState) {
			var q = '?age=' + currentAge + '&retire=' + retirementAge + '&amount=' + Math.round(currentAmount) +
				'&monthly=' + Math.round(monthly) + '&return=' + (+(expectedReturn * 100).toFixed(4));
			history.replaceState(null, '', window.location.pathname + q);
		}
	}
	window.calculate = calculate;

	/* ---------- "Copy link to these results" ---------- */
	function wireCopy(scope) {
		var copy = scope && scope.querySelector('.sp500-copy');
		if (!copy || copy.dataset.wired) { return; }
		copy.dataset.wired = '1';
		copy.addEventListener('click', function () {
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
		if (p.get('age') === null || p.get('return') === null) { return; }
		var set = function (id, key, money) {
			var v = parseFloat(p.get(key));
			if (!isNaN(v) && $(id)) { $(id).value = money ? v.toLocaleString('en-US') : v; }
		};
		set('current-age', 'age'); set('retirement-age', 'retire');
		set('current-amount', 'amount', true); set('monthly-contributions', 'monthly', true);
		set('expected-return', 'return');
		calculate();
	}

	function init() {
		if (!$('current-age')) { return; }
		wireCopy($('calc-output'));
		var btn = $('calculate-btn');
		if (btn) { btn.addEventListener('click', calculate); }
		['current-age', 'retirement-age', 'current-amount', 'monthly-contributions', 'expected-return'].forEach(function (id) {
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
