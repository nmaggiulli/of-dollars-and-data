
function formatInputNumber(input) {
    // Remove existing commas and non-numeric characters
    let value = input.value.replace(/[^0-9.]/g, '');
    
    // Format with commas
    if (value) {
        input.value = new Intl.NumberFormat('en-US').format(value);
    }
}

function getNumericValue(formattedValue) {
    // Remove commas and convert to number
    return parseFloat(formattedValue.replace(/,/g, '')) || 0;
}

function dynamicCeil(number) {
  if (number === 0) return 0;
  
  const magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(number))));
  return Math.ceil(number / magnitude) * magnitude;
}

function formatNumber(num) {
    return num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
}

let myChart = null;

function calculate() {

  const currentAge = parseInt(document.getElementById('current-age').value);
  const retirementAge = parseInt(document.getElementById('retirement-age').value);
  const currentAmount = getNumericValue(document.getElementById('current-amount').value);
  const monthlyContributions = getNumericValue(document.getElementById('monthly-contributions').value);
  const expectedReturn = parseFloat(document.getElementById('expected-return').value) / 100;

  if (currentAge < 18 || currentAge > 80) {
    alert('Current age must be between 18 and 80.');
    return; // Exit the function early
  }

  if (retirementAge < 18 || retirementAge > 100) {
    alert('Retirement age must be between 18 and 100.');
    return; // Exit the function early
  }
  
    // Error handling for retirement age being at least 1 year greater than starting age
  if (retirementAge <= currentAge) {
    alert('Retirement age must be at least 1 year greater than the Current age.');
    return; // Exit the function early
  }
  
    // Error handling for blank return
  if (!expectedReturn) {
    alert('Please enter the Expected Annual Return (i.e. 2 = 2%, 4.1 = 4.1%, etc.)');
    return;  // Exit the function
  }
  
   // Error handling for expected percentage return
  if (expectedReturn < 0 || expectedReturn > 50) {
    alert('Expected annual return must be between 0% and 50%.');
    return;
  }

  // Error handling for starting investment and monthly contributions
  if (currentAmount < 0) {
    alert('Current investment amount must be greater than or equal to 0.');
    return;
  }

  if (monthlyContributions < 0) {
    alert('Monthly contributions must be greater than or equal to 0.');
    return;
  }
  
    // Error handling for number inputs
  if (isNaN(currentAge) || isNaN(retirementAge) || isNaN(currentAmount) || isNaN(monthlyContributions) || isNaN(expectedReturn)) {
    alert('All inputs must be numbers.');
    return;
  }

  const numberOfMonths = (retirementAge - currentAge) * 12;
  let totalContributions = 0;
  let totalAmount = currentAmount;
  const monthlyReturn = Math.pow(1 + expectedReturn, 1 / 12) - 1;

  const data = [];
  const labels = [];

  for (let i = 0; i <= numberOfMonths; i++) {
    // If i is 0, skip adding the monthly contribution
    if (i === 0) {
      totalAmount = totalAmount * (1 + monthlyReturn);
    } else {
      totalAmount = (totalAmount + monthlyContributions) * (1 + monthlyReturn); // Monthly contribution added here
      totalContributions += monthlyContributions;
    }
    
    if (i % 12 === 0) {
      labels.push(`Age ${currentAge + i / 12}`); // Add the age label when we're at a year boundary
      data.push(totalAmount);  // Push data at each year boundary
    }
  }
  
  const yAxisMax = dynamicCeil(totalAmount);
  
  let maintainAspectRatio = true;
  if (window.innerWidth <= 767) {
      maintainAspectRatio = false;
  }

  const ctx = document.getElementById('myChart').getContext('2d');
  if (myChart !== null) {
    myChart.destroy();  // Destroy the existing chart
  }
  
  myChart = new Chart(ctx, {
    type: 'line',
    data: {
      labels: labels,
      datasets: [{
        label: 'Estimated Investment Amount',
        data: data,  // Added a comma here
        borderColor: '#349800',
        backgroundColor: '#349800',
        fill: false
      }],
    },
    options: {
        responsive: true,
        maintainAspectRatio: maintainAspectRatio,
        scales: {
            yAxes: [{
              ticks: {
                beginAtZero: true,
                max: yAxisMax,  // Set the max value here
                callback: function(value, index, values) {
                    if (yAxisMax < 10) {
                      return '$' + value.toFixed(2);
                    } else {
                      return '$' + value.toLocaleString();
                    }
                  }
              }
            }]
        },
        tooltips: {
            callbacks: {
                label: function(tooltipItem, data) {
                    let label = data.datasets[tooltipItem.datasetIndex].label || '';
                    if (label) {
                        label += ': ';
                    }
                    let formattedNumber = parseFloat(tooltipItem.yLabel).toLocaleString('en-US', {
                    minimumFractionDigits: 2, 
                    maximumFractionDigits: 2 
                    });
                    label += '$' + formattedNumber;
                    return label;
                }
            }
          }
    },
  });

  document.getElementById('total-contributions').innerText = `$${totalContributions.toLocaleString('en-US', {maximumFractionDigits: 2})}`;
  document.getElementById('final-amount').innerText = `$${totalAmount.toLocaleString('en-US', {maximumFractionDigits: 2})}`;

  var currentAmountFormatted = formatNumber(currentAmount);
  var monthlyContributionsFormatted = formatNumber(monthlyContributions);
  var expectedReturnFormatted = (expectedReturn*100).toFixed(2);

  myChart.options.title = {
      display: true,
      text: [`Investment Return Calculator`,
      `Age: ${currentAge}—${retirementAge}`, 
      `Current Amount Invested: $${currentAmountFormatted}`,
      `Monthly Contribution: $${monthlyContributionsFormatted}`, 
      `Expected Annual Return: ${expectedReturnFormatted}%`, 
      ],
      fontSize: 16
  };

  // You may need to update the chart to see the new title
  myChart.update();

  window.addEventListener('resize', function() {
    if (window.innerWidth <= 767) {
        myChart.options.maintainAspectRatio = false;
    } else {
        myChart.options.maintainAspectRatio = true;
    }
    myChart.resize();
  });

}

