cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(jsonlite)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "_calculators/0001_sp500_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

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
    realPricePlusDividend = real_tr) %>%
  select(month, price, dividend, realPrice, realPricePlusDividend)

sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)

to_calc <- sp500_ret_pe %>%
              filter(month >= "2023-01-01")

# Send data to JSON
json_data <- toJSON(to_calc, dataframe = "rows", pretty = TRUE)

#Create function string
js_function_string <- '
function calculateReturns() {
    const startMonth = document.getElementById("start-month").value;
    const endMonth = document.getElementById("end-month").value;

    const startIndex = data.findIndex(item => item.month === startMonth + "-01");
    const endIndex = data.findIndex(item => item.month === endMonth + "-01");

    if (startIndex === -1 || endIndex === -1) {
        alert("Invalid month selection.");
        return;
    }

    const start = data[startIndex];
    const end = data[endIndex];
    const n = (new Date(endMonth) - new Date(startMonth)) / (1000 * 60 * 60 * 24 * 365.25);  // Number of years

    const dividends = data.slice(startIndex, endIndex + 1).reduce((acc, item) => acc + item.dividend, 0);

    const nominalPriceReturn = ((end.price - start.price) / start.price) * 100;
    const annualizedNominalPriceReturn = (Math.pow(end.price / start.price, 1 / n) - 1) * 100;
    const monthlyDividends = data.slice(startIndex, endIndex + 1).reduce((acc, item) => acc + item.dividend/12, 0);
    const nominalTotalReturn = ((end.price + monthlyDividends - start.price) / start.price) * 100;
    const annualizedNominalTotalReturn = (Math.pow((end.price + monthlyDividends) / start.price, 1 / n) - 1) * 100;
    const realPriceReturn = ((end.realPrice - start.realPrice) / start.realPrice) * 100;
    const annualizedRealPriceReturn = (Math.pow(end.realPrice / start.realPrice, 1 / n) - 1) * 100;
    const realTotalReturn = ((end.realPricePlusDividend - start.realPricePlusDividend) / start.realPricePlusDividend) * 100;
    const realTotalReturnRatio = end.realPricePlusDividend / start.realPricePlusDividend;
    const annualizedRealTotalReturn = (Math.pow(Math.abs(realTotalReturnRatio), 1 / n) - 1) * 100 * (realTotalReturnRatio < 0 ? -1 : 1);

    // Display the results
    document.getElementById("nominal-price-return").innerText = nominalPriceReturn.toFixed(2);
    document.getElementById("annualized-nominal-price-return").innerText = annualizedNominalPriceReturn.toFixed(2);
    document.getElementById("nominal-total-return").innerText = nominalTotalReturn.toFixed(2);
    document.getElementById("annualized-nominal-total-return").innerText = annualizedNominalTotalReturn.toFixed(2);
    document.getElementById("real-price-return").innerText = realPriceReturn.toFixed(2);
    document.getElementById("annualized-real-price-return").innerText = annualizedRealPriceReturn.toFixed(2);
    document.getElementById("real-total-return").innerText = realTotalReturn.toFixed(2);
    document.getElementById("annualized-real-total-return").innerText = annualizedRealTotalReturn.toFixed(2);
}
'

#Create HTML string
html_start <- '
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Your Calculator</title>

    <style>

    body {
        font-family: Arial, sans-serif;
    }

    #calculator-container {
        max-width: 500px;  /* Maximum width of the calculator */
        margin: 0 auto;  /* Center the calculator */
        padding: 15px;
        border: 1px solid #ddd;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
    }

    div {
        display: flex;
        justify-content: space-between;  /* Distribute space between label and input */
        margin-bottom: 10px;
    }

    label {
        flex: 1;  /* Take up 1 part of the available space */
        margin-right: 10px;
        white-space: nowrap; /* Prevent label from wrapping to the next line */
    }
    
    input {
        flex: 2;  /* Take up 2 parts of the available space */
        min-width: 150px;  /* Minimum width for date inputs */
    }

    button {
        width: 100%;
        padding: 10px;
        background-color: #007BFF;
        color: #FFF;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        transition: background-color 0.3s;
    }

    button:hover {
        background-color: #0056b3;
    }

    #results {
        margin-top: 15px;
    }

    #results p {
        margin-bottom: 8px;
    }

    </style>
</head>
<body>

    <!-- Your HTML content goes here -->
<div class="calculator">
    <h2>Return Calculator</h2>

    <label for="start-month">Start Month:</label>
    <input type="month" id="start-month">

    <label for="end-month">End Month:</label>
    <input type="month" id="end-month">

    <button onclick="calculateReturns()">Calculate</button>

    <div class="results">
        <p><strong>Nominal Price Return:</strong> <span id="nominal-price-return"></span>%</p>
        <p><strong>Annualized Nominal Price Return:</strong> <span id="annualized-nominal-price-return"></span>%</p>
        <p><strong>Nominal Total Return (price + dividends):</strong> <span id="nominal-total-return"></span>%</p>
        <p><strong>Annualized Nominal Total Return:</strong> <span id="annualized-nominal-total-return"></span>%</p>
        <p><strong>Real Price Return:</strong> <span id="real-price-return"></span>%</p>
        <p><strong>Annualized Real Price Return:</strong> <span id="annualized-real-price-return"></span>%</p>
        <p><strong>Real Total Return (price + dividends):</strong> <span id="real-total-return"></span>%</p>
        <p><strong>Annualized Real Total Return:</strong> <span id="annualized-real-total-return"></span>%</p>
    </div>
</div>
<script>
'

html_end <-
'
</script>
</body>
</html>
'

# Write the HTML string to a file
writeLines(paste(html_start, " const data = ", json_data, ";", js_function_string, html_end), 
           paste0(out_path, "/sp500_calculator.html"))

# ############################  End  ################################## #