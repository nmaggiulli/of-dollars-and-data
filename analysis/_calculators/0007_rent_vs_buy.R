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

html <- '<!DOCTYPE html>
  <html lang="en">
  <head>
  <meta charset="UTF-8">
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

/* Headers (Monthly Costs & 30-Year Projection) */
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

</style>
  </head>
  <body>
  <div style="margin-top: -24vh; padding: 0; height: 0;">&nbsp;</div>
<div class="calculator">
    <div style="border: 4px solid #333; border-radius: 4px; padding: 25px 25px 15px 25px; margin-bottom: 30px; background-color: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.1);">
        <div style="display: flex; gap: 20px;">
        <!-- Column 1 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Monthly Rent:</label>
                <input type="text" id="monthlyRent" value="$3,000" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Future Inflation:</label>
                <input type="text" id="inflation" value="4.00%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Portfolio Growth:</label>
                <input type="text" id="portfolioGrowth" value="4%" oninput="handleInput(this)">
            </div>
        </div>

        <!-- Column 2 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Home Price:</label>
                <input type="text" id="homePrice" value="$500,000" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Downpayment:</label>
                <input type="text" id="downpayment" value="20%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>Interest Rate:</label>
                <input type="text" id="interestRate" value="7.0%" oninput="handleInput(this)">
            </div>
        </div>

        <!-- Column 3 -->
        <div style="flex: 1;">
            <div class="input-group">
                <label>Property Tax:</label>
                <input type="text" id="propertyTax" value="1.0%" oninput="handleInput(this)">
            </div>
            <div class="input-group">
                <label>HOA/Maintenance:</label>
                <input type="text" id="maintenance" value="1.0%" oninput="handleInput(this)">
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
                <h3>Monthly Costs:</h3>
                <p>Mortgage Payment: <span id="monthlyMortgage">$0</span></p>
                <p>Property Tax: <span id="monthlyPropertyTax">$0</span></p>
                <p>Maintenance/HOA: <span id="monthlyMaintenance">$0</span></p>
                <p>Insurance: <span id="monthlyInsurance">$0</span></p>
                <p>Total: <span id="totalMonthlyCost">$0</span></p>
            </div>
            
            <div>
                <h3>30-Year Projection:</h3>
                <p>Final Portfolio Value: <span id="portfolioValue">$0</span></p>
                <p>Final Home Value: <span id="finalHomeValue">$0</span></p>
                <div class="decision" id="finalDecision" style="text-align: left;">-</div>
            </div>
        </div>
    </div>
</div>
  
<script type="text/javascript">
//<![CDATA[
document.addEventListener("DOMContentLoaded", function() {
  let debounceTimer;

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
    
    for (let i = 0; i < 360; i++) {
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
  }
  
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
});
//]]>
</script>
  </body>
  </html>'

writeLines(html, 
           paste0(out_path, "/buy_vs_rent.html"))



# ############################  End  ################################## #