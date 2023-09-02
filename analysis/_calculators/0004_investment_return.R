cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(jsonlite)
library(zoo)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "_calculators/0004_investment_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Create function string
js_function_string <- "
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
  const currentAmount = parseFloat(document.getElementById('current-amount').value);
  const monthlyContributions = parseFloat(document.getElementById('monthly-contributions').value);
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
  var expectedReturnFormatted = expectedReturn*100;

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
"

#Create HTML string start
html_start1 <- '
<!DOCTYPE html>
<html>
<head>
<title>Investment Return Calculator</title>
</head>
<body>'

html_start2 <- '
  <!-- Your HTML content goes here -->
  <div class="calculator">
  <div class="age-amounts">
  <div class ="ages">
    <label for="current-age">Current Age:</label>
    <input type="number" id="current-age" name="current-age" value="30">
    <label for="retirement-age">Retirement Age:</label>
    <input type="number" id="retirement-age" name="retirement-age" value="65">
  </div>
  <div class ="amounts">
    <label for="current-amount">Current Amount Invested:</label>
    <input type="number" id="current-amount" name="current-amount" value="1">
    <label for="monthly-contributions">Monthly Contributions:</label>
    <input type="number" id="monthly-contributions" name="monthly-contribution" value="0">
  </div>
  </div>

  <div class="expected-return">
    <label for="expected-return">Expected Annual Return (%):</label>
    <input type="number" id="expected-return" name="expected-return" value="4">
  </div>

  <button onclick="calculate()">Calculate</button>
  </div>
  
  <div class="results">
    <p><strong>Total Contributions: </strong><span id="total-contributions"></span></p>
    <p><strong>Estimated Final Amount: </strong><span id="final-amount"></span></p>
  </div>
  <hr>

  <div id="chart-container">
          <canvas id="myChart" width="400" height="200"></canvas>
  </div>
  <hr>
'

html_js_script <- '
<script src="https://cdn.jsdelivr.net/npm/chart.js@2.9.4"></script>
<script>
'

html_end <-
  '
</script>
</body>
</html>
'

# Write the HTML string to a file
writeLines(paste(trimws(html_start1), trimws(html_start2), html_js_script, js_function_string, html_end), 
           paste0(out_path, "/_test_investment_calc.html"))

writeLines(paste(js_function_string), 
           paste0(out_path, "/investment_return_calculator.js"))

html_end <- str_replace_all(html_end, "</script>", "")

writeLines(paste(trimws(html_start2)), 
           paste0(out_path, "/investment_return_calculator.html"))

# ############################  End  ################################## #