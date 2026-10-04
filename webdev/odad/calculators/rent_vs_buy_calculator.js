/*
 * Rent vs. Buy Calculator — Of Dollars And Data
 *
 * Results update automatically about a second after you stop typing.
 * Model: 30-year fixed mortgage, 3% closing costs, rent and home prices grow at
 * "Future Inflation", and the renter's savings (or shortfalls) grow at
 * "Portfolio Growth". Runs 360 months.
 *
 * Shareable links: after any change, the address bar holds every input, e.g.
 *   ?rent=2500&inflation=3&growth=6&price=500000&down=20&rate=7&tax=1&maint=1&ins=0.5
 */
(function () {
	'use strict';

	function $(id) { return document.getElementById(id); }

	// Input id => [link parameter, label for error messages, money?]
	var FIELDS = {
		monthlyRent: ['rent', 'monthly rent', true],
		inflation: ['inflation', 'future inflation', false],
		portfolioGrowth: ['growth', 'portfolio growth', false],
		homePrice: ['price', 'home price', true],
		downpayment: ['down', 'downpayment', false],
		interestRate: ['rate', 'interest rate', false],
		propertyTax: ['tax', 'property tax', false],
		maintenance: ['maint', 'HOA/maintenance', false],
		insurance: ['ins', 'insurance', false]
	};

	var money = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', minimumFractionDigits: 0, maximumFractionDigits: 0 });
	function fmt(v) { return money.format(Math.round(v)); }

	/* ---------- formatting (same as before) ---------- */
	function formatCurrency(input) {
		var number = parseFloat(input.value.replace(/[^\d.]/g, ''));
		if (!isNaN(number)) { input.value = money.format(number); }
	}
	function formatPercent(input) {
		var number = parseFloat(input.value.replace(/[^\d.]/g, ''));
		if (!isNaN(number)) { input.value = number.toFixed(2) + '%'; }
	}
	function parseCurrency(value) { return parseFloat(value.replace(/[$,]/g, '')); }
	function parsePercent(value) { return parseFloat(value.replace('%', '')); }

	/* ---------- mortgage math (same as before, plus a 0% rate case) ---------- */
	function monthlyPayment(principal, annualRate, years) {
		var r = annualRate / 12 / 100, n = years * 12;
		if (r === 0) { return principal / n; }
		return principal * r * Math.pow(1 + r, n) / (Math.pow(1 + r, n) - 1);
	}
	function remainingBalance(principal, annualRate, totalMonths, monthsPassed) {
		var r = annualRate / 12 / 100, pay = monthlyPayment(principal, annualRate, totalMonths / 12);
		if (r === 0) { return Math.max(0, principal - pay * monthsPassed); }
		return Math.max(0, principal * Math.pow(1 + r, monthsPassed) - pay * ((Math.pow(1 + r, monthsPassed) - 1) / r));
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
	function drawChart(portfolioValues, homeEquity) {
		if (!window.Chart || !$('valueChart')) { return; }
		if (chart) { chart.destroy(); }
		chart = new Chart($('valueChart').getContext('2d'), {
			type: 'line',
			data: {
				labels: Array.from({ length: 31 }, function (_, i) { return i; }),
				datasets: [{
					label: 'Portfolio Value', data: portfolioValues,
					borderColor: '#4CAF50', backgroundColor: '#4CAF50', fill: false, tension: 0.4, borderWidth: 2
				}, {
					label: 'Home Equity', data: homeEquity,
					borderColor: '#2196F3', backgroundColor: '#2196F3', fill: false, tension: 0.4, borderWidth: 2
				}]
			},
			options: {
				responsive: true,
				maintainAspectRatio: false,
				tooltips: {
					callbacks: {
						title: function (items) { return 'Year = ' + items[0].xLabel; },
						label: function (item, data) { return data.datasets[item.datasetIndex].label + ': ' + money.format(item.yLabel); }
					}
				},
				scales: {
					yAxes: [{ ticks: { callback: function (v) { return money.format(v); }, beginAtZero: true } }],
					xAxes: [{ scaleLabel: { display: true, labelString: 'Year' } }]
				}
			}
		});
	}

	/* ---------- the model ---------- */
	function read() {
		var v = {}, bad = [];
		Object.keys(FIELDS).forEach(function (id) {
			var el = $(id), n = FIELDS[id][2] ? parseCurrency(el.value) : parsePercent(el.value);
			if (isNaN(n)) { bad.push(id); }
			v[id] = n;
		});
		if (bad.length) {
			showError('Enter a valid ' + FIELDS[bad[0]][1] + '.', bad);
			return null;
		}
		if (v.downpayment > 100) {
			showError('The downpayment can\'t be more than 100%.', ['downpayment']);
			return null;
		}
		clearError();
		return v;
	}

	function calculate(updateLink) {
		var v = read();
		if (!v) { return; }

		var inflation = v.inflation / 100;
		var propertyTax = v.propertyTax / 100, maintenance = v.maintenance / 100, insurance = v.insurance / 100;
		var interestRate = v.interestRate, homePrice0 = v.homePrice, rent0 = v.monthlyRent;

		var downpaymentAmount = homePrice0 * (v.downpayment / 100);
		var closingCosts = homePrice0 * 0.03; // 3% closing costs
		var loanAmount = homePrice0 - downpaymentAmount;
		var mortgage = monthlyPayment(loanAmount, interestRate, 30);

		var monthlyReturn = Math.pow(1 + v.portfolioGrowth / 100, 1 / 12) - 1;
		var monthlyInflation = Math.pow(1 + inflation, 1 / 12) - 1;

		// Starting monthly costs
		$('monthlyMortgage').textContent = fmt(mortgage);
		$('monthlyPropertyTax').textContent = fmt(homePrice0 * propertyTax / 12);
		$('monthlyMaintenance').textContent = fmt(homePrice0 * maintenance / 12);
		$('monthlyInsurance').textContent = fmt(homePrice0 * insurance / 12);
		$('totalMonthlyCost').textContent = fmt(mortgage + homePrice0 * propertyTax / 12 +
			homePrice0 * maintenance / 12 + homePrice0 * insurance / 12);

		// Year 0 = the day you buy (or invest the downpayment + closing costs instead)
		var portfolio = downpaymentAmount + closingCosts;
		var homePrice = homePrice0, rent = rent0;
		var portfolioValues = [Math.round(portfolio)];
		var homeEquity = [Math.round(homePrice - loanAmount)];

		// Months 1 to 360
		for (var i = 1; i <= 360; i++) {
			homePrice *= (1 + monthlyInflation);
			rent *= (1 + monthlyInflation);

			var housingCost = mortgage + (homePrice * propertyTax) / 12 +
				(homePrice * maintenance) / 12 + (homePrice * insurance) / 12;
			portfolio = portfolio * (1 + monthlyReturn) + (housingCost - rent);

			if (i % 12 === 0) {
				portfolioValues.push(Math.round(portfolio));
				homeEquity.push(Math.round(homePrice - remainingBalance(loanAmount, interestRate, 360, i)));
			}
		}

		drawChart(portfolioValues, homeEquity);

		$('portfolioValue').textContent = fmt(portfolio);
		$('finalHomeValue').textContent = fmt(homePrice);
		var rentWins = portfolio > homePrice;
		$('finalDecision').textContent = rentWins ? 'RENT' : 'BUY';
		$('finalDecision').style.color = rentWins ? '#4CAF50' : '#2196F3';

		if (updateLink && window.history && history.replaceState) {
			var q = Object.keys(FIELDS).map(function (id) {
				return FIELDS[id][0] + '=' + encodeURIComponent(+v[id].toFixed(4));
			}).join('&');
			history.replaceState(null, '', window.location.pathname + '?' + q);
		}
	}

	/* ---------- typing: format each field, then recalculate after a pause ---------- */
	var calcTimer = null, formatTimers = {};
	function onInput(input) {
		clearTimeout(formatTimers[input.id]);
		formatTimers[input.id] = setTimeout(function () {
			if (FIELDS[input.id][2]) { formatCurrency(input); } else { formatPercent(input); }
		}, 1000);
		clearTimeout(calcTimer);
		calcTimer = setTimeout(function () { calculate(true); }, 1000);
	}

	/* ---------- "Copy link to these results" ---------- */
	function wireCopy() {
		var copy = document.querySelector('.rvb-calc .sp500-copy');
		if (!copy) { return; }
		copy.addEventListener('click', function () {
			calculate(true); // make sure the link matches what's on screen
			var url = window.location.href, done = document.querySelector('.rvb-calc .sp500-copied');
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
		var p = new URLSearchParams(window.location.search), any = false;
		Object.keys(FIELDS).forEach(function (id) {
			var raw = p.get(FIELDS[id][0]);
			if (raw === null || isNaN(parseFloat(raw))) { return; }
			var n = parseFloat(raw), el = $(id);
			el.value = FIELDS[id][2] ? money.format(n) : n.toFixed(2) + '%';
			any = true;
		});
		return any;
	}

	function init() {
		if (!document.querySelector('.calculator-grid-bvr')) { return; }
		var fromLink = applyLink();
		Object.keys(FIELDS).forEach(function (id) {
			var el = $(id);
			if (el) { el.addEventListener('input', function () { onInput(el); }); }
		});
		wireCopy();
		calculate(fromLink);
	}

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', init);
	} else {
		init();
	}
})();
