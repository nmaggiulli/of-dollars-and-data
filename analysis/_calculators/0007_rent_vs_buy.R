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
library(quantmod)
library(Hmisc)
library(scales)
library(tidyverse)

folder_name <- "_calculators/0007_rent_vs_buy"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

html_start1 <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
  
.calculator {
    max-width: 800px;
    margin: 0 auto;
    padding: 0;
    font-family: Arial, sans-serif;
}

.input-group {
    margin-bottom: 15px;
}

.input-group label {
    display: inline-block;
    width: 180px;
    margin-right: 10px;
    color: #666;
    font-size: 1.1em;
}

.input-group input {
    width: 120px;
    padding: 8px 12px;
    border: 1px solid #ccc;
    border-radius: 4px;
    font-size: 16px;
    background-color: #fff;
}

/* Results section styling */
.results {
    display: flex;
    gap: 60px;
    margin-top: 40px;
}

.results > div {
    flex: 1;
}

/* Headers (Monthly Costs & Final Results) */
.results h2 {
    font-size: 32px;
    margin-bottom: 25px;
    color: #333;
}

/* Individual result lines */
.results p {
    margin: 15px 0;
    font-size: 20px;
    color: #555;
}

/* Decision styling */
.decision {
    margin-top: 20px;
    font-size: 28px;
    font-weight: bold;
    color: #2196F3;
}

  /* Add to existing styles */
  .recharts-default-tooltip {
      background-color: #fff !important;
      border: 1px solid #ccc !important;
      padding: 10px !important;
  }
  
  .chart-container {
      border: 1px solid #eee;
      padding: 20px;
      border-radius: 4px;
      background-color: #fff;
  }

</style>
  </head>
  <body>'

html_start2 <- '
<div class="calculator">
    <div style="border: 4px solid #333; border-radius: 4px; padding: 25px 25px 15px 25px; margin-bottom: 0px; background-color: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.1);">
        <div style="display: flex; gap: 20px;">
        <!-- Column 1 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Monthly Rent:</label>
                <input type="text" id="monthlyRent" value="$2,000" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Future Inflation:</label>
                <input type="text" id="inflation" value="4.00%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Portfolio Growth:</label>
                <input type="text" id="portfolioGrowth" value="4.00%" oninput="handleInput(this)">
            </div>
        </div>

        <!-- Column 2 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Home Price:</label>
                <input type="text" id="homePrice" value="$420,000" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Downpayment:</label>
                <input type="text" id="downpayment" value="20%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Interest Rate:</label>
                <input type="text" id="interestRate" value="7.00%" oninput="handleInput(this)">
            </div>
        </div>

        <!-- Column 3 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Property Tax:</label>
                <input type="text" id="propertyTax" value="1.00%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>HOA/Maintenance:</label>
                <input type="text" id="maintenance" value="1.00%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Insurance:</label>
                <input type="text" id="insurance" value="0.50%" oninput="handleInput(this)">
            </div>
        </div>
    </div>
  </div>  
  
 <!-- Results section (back to original simple layout) -->
    <div class="results">
        <div style="display: flex; justify-content: space-between;">
            <div>
                <h3>Monthly Housing Cost</h3>
                <p>Total: <span id="totalMonthlyCost">$0</span></p>
                <p>Mortgage Payment: <span id="monthlyMortgage">$0</span></p>
                <p>Property Tax: <span id="monthlyPropertyTax">$0</span></p>
                <p>Maintenance/HOA: <span id="monthlyMaintenance">$0</span></p>
                <p>Insurance: <span id="monthlyInsurance">$0</span></p>
            </div>
            
            <div>
                <h3>Final Decision</h3>
                <div class="decision" id="finalDecision" style="text-align: left; margin: 30px 0; font-size: 32px; font-weight: bold; letter-spacing: 1px;">-</div>
                <p>Final Portfolio Value: <span id="portfolioValue">$0</span></p>
                <p>Final Home Value: <span id="finalHomeValue">$0</span></p>
            </div>
        </div>
    </div>
    <div class="chart-container" style="margin-top: 40px;">
        <h3 style="margin-bottom: 20px;">Renting vs. Buying Over Time</h3>
        <canvas id="valueChart" style="width: 100%; height: 400px;"></canvas>
    </div>
</div>'

js_function_string <- '
document.addEventListener("DOMContentLoaded", function() {
  let debounceTimer;
  let valueChart = null;

function handleInput(input) {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    if (input.id === "monthlyRent" || input.id === "homePrice") {
      formatCurrency(input);
    } else {
      formatPercent(input);
    }
    calculateDecision();
  }, 1000);
}

function formatCurrency(input) {
  // Remove any non-digit characters except decimal point
  let value = input.value.replace(/[^\\d.]/g, "");
  // Convert to number and format
  let number = parseFloat(value);
  if (!isNaN(number)) {
    input.value = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      minimumFractionDigits: 0,
      maximumFractionDigits: 0
    }).format(number);
  }
}

function formatPercent(input) {
  // Remove any non-digit characters except decimal point
  let value = input.value.replace(/[^\\d.]/g, "");
  // Convert to number and format
  let number = parseFloat(value);
  if (!isNaN(number)) {
    input.value = number.toFixed(2) + "%";
  }
}

function parseCurrency(value) {
  return parseFloat(value.replace(/[$,]/g, ""));
}

function parsePercent(value) {
  return parseFloat(value.replace("%", ""));
}

function calculateMonthlyMortgage(principal, annualRate, years) {
  const monthlyRate = annualRate / 12 / 100;
  const numberOfPayments = years * 12;
  return principal * monthlyRate * Math.pow(1 + monthlyRate, numberOfPayments) 
  / (Math.pow(1 + monthlyRate, numberOfPayments) - 1);
}
function calculateDecision() {
    // Get input values
    const inflation = parsePercent(document.getElementById("inflation").value) / 100;
    const initialMonthlyRent = parseCurrency(document.getElementById("monthlyRent").value);
    const propertyTax = parsePercent(document.getElementById("propertyTax").value) / 100;
    const maintenance = parsePercent(document.getElementById("maintenance").value) / 100;
    const insurance = parsePercent(document.getElementById("insurance").value) / 100;
    const downpaymentPercent = parsePercent(document.getElementById("downpayment").value) / 100;
    const interestRate = parsePercent(document.getElementById("interestRate").value);
    const initialHomePrice = parseCurrency(document.getElementById("homePrice").value);
    
    // Calculate key values
    const downpaymentAmount = initialHomePrice * downpaymentPercent;
    const closingCosts = initialHomePrice * 0.03; // 3% closing costs
    const loanAmount = initialHomePrice - downpaymentAmount;
    const monthlyMortgage = calculateMonthlyMortgage(loanAmount, interestRate, 30);
    
    // Initialize starting values
    let portfolioValue = downpaymentAmount + closingCosts;
    const portfolioGrowthRate = parsePercent(document.getElementById("portfolioGrowth").value) / 100;
    const totalAnnualReturn = (1 + inflation) * (1 + portfolioGrowthRate) - 1;
    const monthlyReturn = Math.pow(1 + totalAnnualReturn, 1/12) - 1;
    const monthlyInflation = Math.pow(1 + inflation, 1/12) - 1;

    // Calculate month by month
    let currentMonthlyRent = initialMonthlyRent;
    let currentHomePrice = initialHomePrice;
    
    // Display initial monthly costs
    document.getElementById("monthlyMortgage").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round(monthlyMortgage));
    
    document.getElementById("monthlyPropertyTax").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * propertyTax) / 12));
    
    document.getElementById("monthlyMaintenance").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * maintenance) / 12));
    
    document.getElementById("monthlyInsurance").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * insurance) / 12));
    
    document.getElementById("totalMonthlyCost").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round(monthlyMortgage + 
        (initialHomePrice * propertyTax) / 12 + 
        (initialHomePrice * maintenance) / 12 + 
        (initialHomePrice * insurance) / 12));
        
     let monthlyData = [];
      let labels = [];
      let portfolioValues = [];
      let homeValues = [];
    
    for (let i = 0; i <= 360; i++) {
        // Update home price and related costs with inflation
        currentHomePrice *= (1 + monthlyInflation);
        currentMonthlyRent *= (1 + monthlyInflation);
        let currentMonthlyPropertyTax = (currentHomePrice * propertyTax) / 12;
        let currentMonthlyMaintenance = (currentHomePrice * maintenance) / 12;
        let currentMonthlyInsurance = (currentHomePrice * insurance) / 12;
        
        // Calculate current months housing cost
        const totalMonthlyHousingCost = monthlyMortgage + 
          currentMonthlyPropertyTax + 
          currentMonthlyMaintenance + 
          currentMonthlyInsurance;

  // Calculate and apply monthly investment
  const monthlyInvestment = totalMonthlyHousingCost - currentMonthlyRent;
  portfolioValue = portfolioValue * (1 + monthlyReturn) + monthlyInvestment;
  
      if (i % 12 === 0) {
          let year = Math.floor(i/12);
          labels.push(year);
          portfolioValues.push(Math.round(portfolioValue));
          homeValues.push(Math.round(currentHomePrice));
          console.log("Adding data for year:", year);  // Debug log
      }
    }
    
  console.log("Chart.js version:", Chart.version);  
    
  // Test formatting function
    function formatYAxis(value) {
        console.log("Formatting:", value);
        try {
            return "$" + value.toLocaleString();
        } catch (e) {
            console.error("Error formatting:", e);
            return value;
        }
    }  
  
 // Update the chart
        if (valueChart) {
            valueChart.destroy();
        }
        
        // Add this right before creating the chart to test
        console.log("Test format: ", formatYAxis(1000000));
        
        const ctx = document.getElementById("valueChart").getContext("2d");
        valueChart = new Chart(ctx, {
            type: "line",
                  data: {
                    labels: Array.from({length: 31}, (_, i) => i),  // This will create labels 0-30
                    datasets: [{
                        label: "Portfolio Value",
                        data: portfolioValues,
                        borderColor: "#4CAF50",
                        backgroundColor: "#4CAF50",
                        fill: false,
                        tension: 0.4,
                        borderWidth: 2
                    }, {
                        label: "Home Value",
                        data: homeValues,
                        borderColor: "#2196F3",
                        backgroundColor: "#2196F3",
                        fill: false,
                        tension: 0.4,
                        borderWidth: 2
                    }]
                },
            options: {
                    responsive: true,
                    maintainAspectRatio: false,
                   tooltips: {
                        callbacks: {
                            title: function(tooltipItems, data) {
                                return "Year = " + tooltipItems[0].xLabel;
                            },
                            label: function(tooltipItem, data) {
                                let value = tooltipItem.yLabel;
                                return data.datasets[tooltipItem.datasetIndex].label + ": " + 
                                    new Intl.NumberFormat("en-US", {
                                        style: "currency",
                                        currency: "USD",
                                        minimumFractionDigits: 0,
                                        maximumFractionDigits: 0
                                    }).format(value);
                            }
                        }
                    },
                    plugins: {
                        legend: {
                            position: "top",
                            align: "center",
                            labels: {
                                usePointStyle: true,
                                boxWidth: 16,
                                boxHeight: 16,
                                padding: 20,
                                color: "#000000",
                                font: {
                                    size: 14
                                }
                            }
                        }
                    },
                    scales: {
                        yAxes: [{
                            ticks: {
                                callback: function(value) {
                                    return new Intl.NumberFormat("en-US", {
                                        style: "currency",
                                        currency: "USD",
                                        minimumFractionDigits: 0,
                                        maximumFractionDigits: 0
                                    }).format(value);
                                },
                                beginAtZero: true
                            }
                        }],
                        xAxes: [{
                            scaleLabel: {
                                display: true,
                                labelString: "Year"
                            }
                        }]
                    }
                }
        });
  
  // Final home value is already calculated in currentHomePrice
  const homeValue = currentHomePrice;
  
  // Update results display
  document.getElementById("portfolioValue").textContent = new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
          minimumFractionDigits: 0,
          maximumFractionDigits: 0
      }).format(Math.round(portfolioValue));
      
      document.getElementById("finalHomeValue").textContent = new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
          minimumFractionDigits: 0,
          maximumFractionDigits: 0
      }).format(Math.round(homeValue));
  
    document.getElementById("finalDecision").textContent = 
      portfolioValue > homeValue ? "RENT" : "BUY";
    document.getElementById("finalDecision").style.color = 
      portfolioValue > homeValue ? "#4CAF50" : "#2196F3";
  }

// Calculate initial results when page loads
    calculateDecision();

    // Add event listeners to all inputs
    document.querySelectorAll("input").forEach(input => {
        input.addEventListener("input", () => handleInput(input));
    });
});'

html_js_script <- '
        <script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"></script>
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
writeLines(paste(html_start1,
                 html_start2,
                 html_js_script,
                 js_function_string, 
                 html_end), 
           paste0(out_path, "/_test_rent_vs_buy_calc.html"))

writeLines(js_function_string, 
           paste0(out_path, "/rent_vs_buy_calculator.js"))

# Now write code for the HTML to work on Wordpress
html_start1_wp <- str_replace_all(html_start1, "</head>", "")
html_start1_wp <- str_replace_all(html_start1_wp, "<body>", "")

writeLines(paste(trimws(html_start1_wp),
                        html_start2), 
           paste0(out_path, "/rent_vs_buy_calculator.html"))

# ############################  End  ################################## #