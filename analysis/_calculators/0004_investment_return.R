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

  const numberOfMonths = (retirementAge - currentAge) * 12;
  let totalContributions = 0;
  let totalAmount = currentAmount;
  const monthlyReturn = Math.pow(1 + expectedReturn, 1 / 12) - 1;

  const data = [];

  for (let i = 0; i <= numberOfMonths; i++) {
    if (i % 12 === 0) {
      totalContributions += monthlyContributions * 12;
    }
    totalAmount = (totalAmount + monthlyContributions) * (1 + monthlyReturn);
    data.push(totalAmount);
  }
  
  const yAxisMax = dynamicCeil(totalAmount);
  
  let maintainAspectRatio = true;
  if (window.innerWidth <= 767) {
      maintainAspectRatio = false;
  }

  const ctx = document.getElementById('myChart').getContext('2d');
  const myChart = new Chart(ctx, {
    type: 'line',
    data: {
      labels: Array.from({ length: numberOfMonths + 1 }, (_, i) => 'Age ' + (currentAge + Math.floor(i / 12))),
      datasets: [{
        label: 'Projected Invested Amount',
        data: data, // your data array
        borderColor: 'black',
        backgroundColor: 'black'
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
html_start <- '
<!DOCTYPE html>
<html>
<head>
<title>Investment Return Calculator</title>
</head>
<body>

  <div class="current-age">
    <label for="current-age">Current Age:</label>
    <input type="number" id="current-age" name="current-age" value="30">
  </div>

  <div class="retirement-age">
    <label for="retirement-age">Retirement Age:</label>
    <input type="number" id="retirement-age" name="retirement-age" value="65">
  </div>

  <div class="current-amount">
    <label for="current-amount">Current Amount Invested:</label>
    <input type="number" id="current-amount" name="current-amount" value="1">
  </div>

  <div class="monthly-amount">
    <label for="monthly-contributions">Monthly Contributions:</label>
    <input type="number" id="monthly-contributions" name="monthly-contribution" value="0">
  </div>

  <div class="expected-ret-label">
    <label for="expected-return">Expected Annual Return (%):</label>
    <input type="number" id="expected-return" name="expected-return" value="5">
  </div>

  <button onclick="calculate()">Calculate</button>

  <h2>Results:</h2>
  <p>Total Contributions: <span id="total-contributions"></span></p>
  <p>Final Investment Amount: <span id="final-amount"></span></p>

  <div id="chart-container">
          <canvas id="myChart" width="400" height="200"></canvas>
  </div>
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
writeLines(paste(html_start, html_js_script, js_function_string, html_end), 
           paste0(out_path, "/_test_investment_calc.html"))

writeLines(paste(js_function_string), 
           paste0(out_path, "/investment_return_calculator.js"))

html_end <- str_replace_all(html_end, "</script>", "")

writeLines(paste(html_start), 
           paste0(out_path, "/investment_return_calculator.html"))

# ############################  End  ################################## #