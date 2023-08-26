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

folder_name <- "_calculators/0002_sp500_dca_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

download_file <- 0
filter_date <- "1871-01-01"
url <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
dest_file <- paste0(importdir, "0009_sp500_returns_pe/ie_data.xls") 

if(download_file == 1){
  download.file(url, dest_file, mode = "wb")
}

sp500_ret_pe <- read_excel(paste0(importdir, "0009_sp500_returns_pe/ie_data.xls"),
                           sheet = "Data") 

colnames(sp500_ret_pe) <- c("date", "price", "dividend", "earnings", "cpi", "date_frac", 
                            "long_irate", "real_price", "real_div", "real_tr",
                            "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2")

#Remove first 6 rows
sp500_ret_pe <- sp500_ret_pe[7:nrow(sp500_ret_pe),]

# Convert vars to numeric
sp500_ret_pe$price <- as.numeric(sp500_ret_pe$price)
sp500_ret_pe$dividend <- as.numeric(sp500_ret_pe$dividend)
sp500_ret_pe$real_price <- as.numeric(sp500_ret_pe$real_price)
sp500_ret_pe$real_tr <- as.numeric(sp500_ret_pe$real_tr)
sp500_ret_pe$cpi <- as.numeric(sp500_ret_pe$cpi)
sp500_ret_pe$date <- as.numeric(sp500_ret_pe$date)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(Sys.Date()) + month(Sys.Date())/100

# Filter out missing dividends
sp500_ret_pe <- sp500_ret_pe %>%
  select(date, price, dividend, real_price, real_tr, cpi) %>%
  filter(!is.na(date), date < end_date)

# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- sp500_ret_pe %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d"))) %>%
  rename(month = date,
    realPrice = real_price,
    realPricePlusDividend = real_tr)

sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    
    sp500_ret_pe[i, "nominalMonthlyReturn"] <- 0
    sp500_ret_pe[i, "realMonthlyReturn"] <- 0
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    
    sp500_ret_pe[i, "nominalMonthlyReturn"] <- sp500_ret_pe[i, "nominalPricePlusDividend"]/sp500_ret_pe[(i-1), "nominalPricePlusDividend"] - 1
    sp500_ret_pe[i, "realMonthlyReturn"] <- sp500_ret_pe[i, "realPricePlusDividend"]/sp500_ret_pe[(i-1), "realPricePlusDividend"] - 1
  }
}

to_calc <- sp500_ret_pe %>%
              filter(month >= filter_date) %>%
              select(month, nominalMonthlyReturn, realMonthlyReturn, cpi)

end_year <- year(max(to_calc$month))

# Send data to JSON
json_data <- toJSON(to_calc, dataframe = "rows", pretty = TRUE)

#Create function string
js_function_string <- '
function formatNumber(num) {
    return num.toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 });
}

function formatDollar(value) {
    // Return the formatted string
    return "$" + formatNumber(value);
}

function calculateReturns() {
    const startMonth = document.getElementById("start-month").value;
    const startYear = document.getElementById("start-year").value;
    const endMonth = document.getElementById("end-month").value;
    const endYear = document.getElementById("end-year").value;
    
    const initialInvestment = parseFloat(document.getElementById("initial-investment").value);
    const monthlyInvestment = parseFloat(document.getElementById("monthly-investment").value);
    const adjustForInflation = document.getElementById("adjust-for-inflation").checked;

    if (initialInvestment < 0 || monthlyInvestment < 0) {
        alert("Initial investment and monthly contribution amounts must be non-negative.");
        return; // exit the function early
    }
    
    const startMonthInt = parseInt(document.getElementById("start-month").value, 10);
    const startYearInt = parseInt(document.getElementById("start-year").value, 10);
    const endMonthInt = parseInt(document.getElementById("end-month").value, 10);
    const endYearInt = parseInt(document.getElementById("end-year").value, 10);
    
    if (startYearInt > endYearInt || (startYearInt === endYearInt && startMonthInt >= endMonthInt)) {
        alert("The end month must be AFTER the start month.");
        return; // exit the function early
    }

    // Combine the year and month to get the full date in YYYY-MM-DD format
    const startFullDate = `${startYear}-${startMonth}-01`;
    const endFullDate = `${endYear}-${endMonth}-01`;
    
    const startIndex = data.findIndex(item => item.month === startFullDate);
    const endIndex = data.findIndex(item => item.month === endFullDate);
    
    if (startIndex === -1 || endIndex === -1) {
        alert("Invalid month selection.");
        return;
    }

    const start = data[startIndex];
    const end = data[endIndex];
    
    const selectedData = data.slice(startIndex, endIndex + 1);
    
    const totalMonths = ((endYear - startYear) * 12) + (endMonth - startMonth);
    
    let finalValueNominalDollars = initialInvestment;
    let finalValueRealDollars = initialInvestment;
    let inflationAdjustedMonthlyContribution = monthlyInvestment;
    let totalContributions = initialInvestment;
    
    // Loop through each month
    for (let i = 0; i < selectedData.length; i++) {
        const currentItem = selectedData[i];
    
      // Update the nominal and real values with returns
      finalValueNominalDollars *= (1 + currentItem.nominalMonthlyReturn);
      finalValueRealDollars *= (1 + currentItem.realMonthlyReturn);
      
      // If the "Adjust Contributions for Inflation" toggle is on, adjust the monthly contribution
      if (adjustForInflation && i > 0) {
          inflationAdjustedMonthlyContribution = monthlyInvestment * (currentItem.cpi / selectedData[0].cpi);
      }
      
      // Add the monthly contributions to the final values
      finalValueNominalDollars += inflationAdjustedMonthlyContribution;
      finalValueRealDollars += inflationAdjustedMonthlyContribution;
      totalContributions += inflationAdjustedMonthlyContribution;
    }
    
    // Format and display the results
    document.getElementById("total-contributions").innerText = formatDollar(totalContributions);
    document.getElementById("final-value-nominal-dollars").innerText = formatDollar(finalValueNominalDollars);
    document.getElementById("final-value-real-dollars").innerText = formatDollar(finalValueRealDollars);
    
  //const irr = calculateIRR(initialInvestment, monthlyInvestment, finalValueNominalDollars, totalMonths);
  document.getElementById("irr-result").innerText = "IRR: " + (irr * 100).toFixed(2) + "%";
}

function calculateIRR(initialInvestment, monthlyInvestment, finalValue, totalMonths) {
  let cashFlows = [-initialInvestment];
  for (let i = 0; i < totalMonths; i++) {
    cashFlows.push(-monthlyInvestment); // Monthly investments are considered outflows
  }
  cashFlows.push(finalValue); // The final value is considered an inflow
  
  let rate = 0.07, // initial guess rate of 10%
      maxIter = 5000,
      tol = 0.00000001;
  
  for (let i = 0; i < maxIter; i++) {
    let f = 0,
        df = 0;
    for (let t = 0; t < cashFlows.length; t++) {
      let val = cashFlows[t];
      f += val / Math.pow(1 + rate, t);
      df -= t * val / Math.pow(1 + rate, t + 1);
    }
    let newRate = rate - f / df;
    if (Math.abs(newRate - rate) < tol) {
      return newRate;
    }
    rate = newRate;
  }
  return NaN; // Did not converge to a solution
}
'

#Create HTML string start
html_start1 <- '
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>S&P 500 Historical Performance Calculator</title>
    '

html_start2 <- '</head>
                <body>
    <!-- Your HTML content goes here -->
 <div class="calculator">
 <div class ="date-container">
 <label>Start Month:</label>
 <div class="date-selector">
 <select id="start-month">'


month_html <- paste0('<option value="', sprintf("%02d", 1:12), '">', month.name, '</option>', collapse = "")

start_month <- "12"
start_month_html <- str_replace_all(month_html, paste0('"', start_month, '\"'), paste0('"', start_month, '\" selected'))

html_mid1 <- 
                '</select>
                <select id="start-year">'

# Create dynamic year string
year_html <- paste0('<option value="', seq(year(filter_date), end_year, 1), '">', seq(year(filter_date), end_year, 1), '</option>', collapse = "")
start_year_html <- str_replace_all(year_html, paste0('"', end_year-1, '\"'), paste0('"', end_year-1, '\" selected'))

# Do rest of HTML string
html_mid2 <- '</select>
          </div>
        <label>End Month:</label>
        <div class="date-selector">
        <select id="end-month">
'

end_month <- format.Date(max(to_calc$month), "%m")
end_month_html <- str_replace_all(month_html, paste0('"', end_month, '\"'), paste0('"', end_month, '\" selected'))

html_mid3 <- '
              </select>
               <select id="end-year">
                    <!-- Adjust the years as needed -->'

end_year_html <- str_replace_all(year_html, paste0('"', end_year, '\"'), paste0('"', end_year, '\" selected'))

html_mid4 <- '</select>
          </div>
        </div>
        <hr>
      <div class="initial-investment">
    <label for="initialInvestment">Initial Investment:</label>
    <input type="number" id="initial-investment" name="initialInvestment" value="1">
    </div>
    <label for="monthly-investment">Monthly Investment:</label>
    <input type="number" id="monthly-investment" value="0">
    <label for="adjust-for-inflation">Adjust Monthly Contributions for Inflation?</label>
    <input type="checkbox" id="adjust-for-inflation">
      <button onclick="calculateReturns()">Calculate</button>
      </div>
        <div class="results">
            <p>Total Nominal Contributions (Initial + Monthly): <span id="total-contributions"></span></p>
            <p>Final Nominal Value (with dividends reinvested): <span id="final-value-nominal-dollars"></span></p>
            <p>Final Inflation-Adjusted Value (with dividends reinvested): <span id="final-value-real-dollars"></span></p>
        <hr>
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
                 start_month_html,
                 html_mid1,
                 start_year_html, 
                 html_mid2, 
                 end_month_html,
                 html_mid3, 
                 end_year_html, 
                 html_mid4,
                 " const data = ", json_data, ";", 
                 js_function_string, 
                 html_end), 
           paste0(out_path, "/_test_dca_calc.html"))

writeLines(paste(" const data = ", json_data, ";", 
                 js_function_string), 
           paste0(out_path, "/sp500_dca_calculator.js"))


html_mid4_wp <- str_replace_all(html_mid4, "<script>", "")
html_end <- str_replace_all(html_end, "</script>", "")

# Now write code for the HTML to work on Wordpress
html_start2_wp <- str_replace_all(html_start2, "</head>", "")
html_start2_wp <- str_replace_all(html_start2_wp, "<body>", "")

writeLines(paste(trimws(html_start2_wp),
                 start_month_html,
                 html_mid1,
                 start_year_html, 
                 html_mid2, 
                 end_month_html,
                 html_mid3, 
                 end_year_html, 
                 trimws(html_mid4_wp)), 
           paste0(out_path, "/sp500_dca_calculator.html"))

print(end_year)
print(end_month)

# ############################  End  ################################## #