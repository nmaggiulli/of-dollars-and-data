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

folder_name <- "_calculators/0001_sp500_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

filter_date <- "1871-01-01"
url <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
dest_file <- paste0(importdir, "0009_sp500_returns_pe/ie_data.xls") 

download.file(url, dest_file, mode = "wb")

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
sp500_ret_pe$date <- as.numeric(sp500_ret_pe$date)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(Sys.Date()) + month(Sys.Date())/100

# Filter out missing dividends
sp500_ret_pe <- sp500_ret_pe %>%
  select(date, price, dividend, real_price, real_tr) %>%
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
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
  }
}

to_calc <- sp500_ret_pe %>%
              filter(month >= filter_date) %>%
              select(month, price, nominalPricePlusDividend, realPrice, realPricePlusDividend)

end_year <- year(max(to_calc$month))

# Send data to JSON
json_data <- toJSON(to_calc, dataframe = "rows", pretty = TRUE)

#Create function string
js_function_string <- '
function formatNumber(num) {
    return num.toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 });
}

function formatDollar(value, initial) {
    // Convert percentage to a multiplication factor (e.g., 100% -> 2)
    let dollarGrowth = ((value / 100) + 1)*initial;

    // Return the formatted string
    return "$" + formatNumber(dollarGrowth);
}

function calculateReturns() {
    const startMonth = document.getElementById("start-month").value;
    const startYear = document.getElementById("start-year").value;
    const endMonth = document.getElementById("end-month").value;
    const endYear = document.getElementById("end-year").value;
    const initialInvestment = document.getElementById("initialInvestment").value;
    
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
    const n = (new Date(endFullDate) - new Date(startFullDate)) / (1000 * 60 * 60 * 24 * 365.25);  // Number of years

    const nominalPriceReturn = ((end.price - start.price) / start.price) * 100;
    const annualizedNominalPriceReturn = (Math.pow(end.price / start.price, 1 / n) - 1) * 100;
    const nominalTotalReturn = ((end.nominalPricePlusDividend - start.nominalPricePlusDividend) / start.nominalPricePlusDividend) * 100;
    const nominalTotalReturnProportion = (end.nominalPricePlusDividend - start.nominalPricePlusDividend) / start.nominalPricePlusDividend; // This is a proportion, not percentage.
    const annualizedNominalTotalReturn = (Math.pow(1 + nominalTotalReturnProportion, 1 / n) - 1) * 100; // This will be in percentage.
    const realPriceReturn = ((end.realPrice - start.realPrice) / start.realPrice) * 100;
    const annualizedRealPriceReturn = (Math.pow(end.realPrice / start.realPrice, 1 / n) - 1) * 100;
    const realTotalReturn = ((end.realPricePlusDividend - start.realPricePlusDividend) / start.realPricePlusDividend) * 100;
    const realTotalReturnRatio = end.realPricePlusDividend / start.realPricePlusDividend;
    const annualizedRealTotalReturn = (Math.pow(Math.abs(realTotalReturnRatio), 1 / n) - 1) * 100 * (realTotalReturnRatio < 0 ? -1 : 1);

    // Display the results
    document.getElementById("nominal-price-return").innerText = formatNumber(nominalPriceReturn);
    document.getElementById("annualized-nominal-price-return").innerText = formatNumber(annualizedNominalPriceReturn);
    document.getElementById("nominal-price-dollar").innerText = formatDollar(nominalPriceReturn, initialInvestment);
    document.getElementById("nominal-total-return").innerText = formatNumber(nominalTotalReturn);
    document.getElementById("annualized-nominal-total-return").innerText = formatNumber(annualizedNominalTotalReturn);
    document.getElementById("nominal-total-dollar").innerText = formatDollar(nominalTotalReturn, initialInvestment);
    document.getElementById("real-price-return").innerText = formatNumber(realPriceReturn);
    document.getElementById("annualized-real-price-return").innerText = formatNumber(annualizedRealPriceReturn);
    document.getElementById("real-price-dollar").innerText = formatDollar(realPriceReturn, initialInvestment);
    document.getElementById("real-total-return").innerText = formatNumber(realTotalReturn);
    document.getElementById("annualized-real-total-return").innerText = formatNumber(annualizedRealTotalReturn);
    document.getElementById("real-total-dollar").innerText = formatDollar(realTotalReturn, initialInvestment);
    
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
    <input type="number" id="initialInvestment" name="initialInvestment" value="1">
    </div>
      <button onclick="calculateReturns()">Calculate</button>
      </div>
        <div class="results">
            <p><strong>Nominal Price Return:</strong> <span id="nominal-price-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-price-return"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-price-dollar"></span></p>
            <p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nominal-total-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="annualized-nominal-total-return"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="nominal-total-dollar"></span></p>
            
            <hr>

            <p><strong>Inflation-Adjusted Price Return:</strong> <span id="real-price-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="annualized-real-price-return"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="real-price-dollar"></span></p>
            <p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="annualized-real-total-return"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="real-total-dollar"></span></p>
        </div>
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
           paste0(out_path, "/_test_calc.html"))

writeLines(paste(" const data = ", json_data, ";", 
                 js_function_string), 
           paste0(out_path, "/sp500_calculator.js"))


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
           paste0(out_path, "/sp500_calculator.html"))

# ############################  End  ################################## #