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

folder_name <- "_calculators/0003_us_stock_bond_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

download_file <- 1
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
                            "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2",
                            "excess_cape", "orig_nom_bond_ret", "real_bond_index")

#Remove first 6 rows
sp500_ret_pe <- sp500_ret_pe[7:nrow(sp500_ret_pe),]

# Convert vars to numeric
sp500_ret_pe$price <- as.numeric(sp500_ret_pe$price)
sp500_ret_pe$dividend <- as.numeric(sp500_ret_pe$dividend)
sp500_ret_pe$real_price <- as.numeric(sp500_ret_pe$real_price)
sp500_ret_pe$real_tr <- as.numeric(sp500_ret_pe$real_tr)
sp500_ret_pe$cpi <- as.numeric(sp500_ret_pe$cpi)
sp500_ret_pe$date <- as.numeric(sp500_ret_pe$date)
sp500_ret_pe$orig_nom_bond_ret <- as.numeric(sp500_ret_pe$orig_nom_bond_ret)
sp500_ret_pe$real_bond_index <- as.numeric(sp500_ret_pe$real_bond_index)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(Sys.Date()) + month(Sys.Date())/100

# Filter out missing dividends
sp500_ret_pe <- sp500_ret_pe %>%
  select(date, price, dividend, real_price, real_tr, cpi, orig_nom_bond_ret, real_bond_index) %>%
  filter(!is.na(date), date < end_date) %>%
  mutate(real_bond_ret = real_bond_index/lag(real_bond_index, 1) - 1,
         orig_nom_bond_ret = orig_nom_bond_ret - 1)

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
    
    sp500_ret_pe[i, "nom_sp500_ret"] <- 0
    sp500_ret_pe[i, "real_sp500_ret"] <- 0
    
    sp500_ret_pe[i, "nom_bond_index"] <- 1
    sp500_ret_pe[i, "nom_bond_ret"] <- 0
    sp500_ret_pe[i, "real_bond_ret"] <- 0
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    
    sp500_ret_pe[i, "nom_sp500_ret"] <- sp500_ret_pe[i, "nominalPricePlusDividend"]/sp500_ret_pe[(i-1), "nominalPricePlusDividend"] - 1
    sp500_ret_pe[i, "real_sp500_ret"] <- sp500_ret_pe[i, "realPricePlusDividend"]/sp500_ret_pe[(i-1), "realPricePlusDividend"] - 1
    
    sp500_ret_pe[i, "nom_bond_index"] <- sp500_ret_pe[(i-1), "nom_bond_index"] * (1 + sp500_ret_pe[(i-1), "orig_nom_bond_ret"])
  }
}

to_calc <- sp500_ret_pe %>%
              filter(month >= filter_date) %>%
              mutate(nom_bond_ret = case_when(
                row_number() == 1 ~ nom_bond_ret,
                TRUE ~ nom_bond_index/lag(nom_bond_index,1) - 1)) %>%
              select(month, nom_sp500_ret, real_sp500_ret, nom_bond_ret, real_bond_ret, cpi)

end_year <- year(max(to_calc$month))

# Send data to JSON
json_data <- toJSON(to_calc, dataframe = "rows", pretty = TRUE)

#Create function string
js_function_string <- '
function formatNumber(num) {
    return num.toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 });
}

function formatPortDollar(value) {
    // Return the formatted string
    return "$" + formatNumber(value);
}

function formatNumberNoDecimals(number) {
    return number.toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 0});
}

function monthNumberToName(monthNumber) {
    const monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    return monthNames[monthNumber - 1];
}

function dynamicCeil(number) {
  if (number === 0) return 0;
  
  const magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(number))));
  return Math.ceil(number / magnitude) * magnitude;
}

function calculatePortReturns() {
    //Remove old chart
    document.getElementById("myChart").remove();
    
    // Create a new canvas element
    var newCanvas = document.createElement("canvas");
    newCanvas.setAttribute("id", "myChart");
    newCanvas.setAttribute("width", "400");
    newCanvas.setAttribute("height", "200");
    
    document.getElementById("chart-container").appendChild(newCanvas);

    const startMonth = document.getElementById("start-month").value;
    const startYear = document.getElementById("start-year").value;
    const endMonth = document.getElementById("end-month").value;
    const endYear = document.getElementById("end-year").value;
    
    const initialInvestment = parseFloat(document.getElementById("initial-investment").value);
    const stockPercentage = parseFloat(document.getElementById("percentage-in-stocks").value) / 100;
    
    if (initialInvestment < 0) {
        alert("Initial investment amount must be non-negative.");
        return; // exit the function early
    }
    
    if (stockPercentage < 0 || stockPercentage > 100) {
        alert("Stock percentage must be between 0 and 100.");
        return; // exit the function early
    }
    
    // Combine the year and month to get the full date in YYYY-MM-DD format
    const startFullDate = `${startYear}-${startMonth}-01`;
    const endFullDate = `${endYear}-${endMonth}-01`;
    
    const startIndex = port_data.findIndex(item => item.month === startFullDate);
    const endIndex = port_data.findIndex(item => item.month === endFullDate);
    
    if (startIndex === -1 || endIndex === -1) {
        alert("Invalid month selection. Data does not exist for these dates.");
        return;  // exit the function early
    }
    
    const startMonthInt = parseInt(document.getElementById("start-month").value, 10);
    const startYearInt = parseInt(document.getElementById("start-year").value, 10);
    const endMonthInt = parseInt(document.getElementById("end-month").value, 10);
    const endYearInt = parseInt(document.getElementById("end-year").value, 10);
    
    if (startYearInt > endYearInt || (startYearInt === endYearInt && startMonthInt > endMonthInt)) {
        alert("The End Month must be greater than or equal to the Start Month.");
        return; // exit the function early
    }
    
    let months = (endYearInt - startYearInt) * 12 + (endMonthInt - startMonthInt) + 1;
    let nominalAmount = initialInvestment;
    let realAmount = initialInvestment;
    let nominalStockAmount = nominalAmount * stockPercentage;
    let nominalBondAmount = nominalAmount * (1 - stockPercentage);
    let realStockAmount = realAmount * stockPercentage;
    let realBondAmount = realAmount * (1 - stockPercentage);
    
    //Start and seed the arrays
    const finalValueNominalDollarsArray = [];
    const finalValueRealDollarsArray = [];
    
    const selectedData = port_data.slice(startIndex, endIndex + 1);
    
    for (let i = 0; i < months; i++) {
      const currentItem = selectedData[i];
    
      if (i % 12 === 0) {
        // Rebalance every 12 months
        nominalStockAmount = nominalAmount * stockPercentage;
        nominalBondAmount = nominalAmount * (1 - stockPercentage);
        realStockAmount = realAmount * stockPercentage;
        realBondAmount = realAmount * (1 - stockPercentage);
      }
      
      nominalStockAmount *= (1 + currentItem.nom_sp500_ret);
      nominalBondAmount *= (1 + currentItem.nom_bond_ret);
      nominalAmount = nominalStockAmount + nominalBondAmount;
  
      realStockAmount *= (1 + currentItem.real_sp500_ret);
      realBondAmount *= (1 + currentItem.real_bond_ret);
      realAmount = realStockAmount + realBondAmount;
      
    // Push the current values into the arrays
    finalValueNominalDollarsArray.push(nominalAmount);
    finalValueRealDollarsArray.push(realAmount);
    }
    
    const nominalAnnualized = Math.pow(nominalAmount / initialInvestment, 1 / (months / 12)) - 1;
    const realAnnualized = Math.pow(realAmount / initialInvestment, 1 / (months / 12)) - 1;
  
    // Output the calculated results
    document.getElementById("nom-total-return").innerText = formatNumber(Number(((nominalAmount / initialInvestment - 1) * 100).toFixed(2)));
    document.getElementById("nom-annualized").innerText = formatNumber(Number((nominalAnnualized * 100).toFixed(2)));
    document.getElementById("nom-total").innerText = formatPortDollar(nominalAmount);
  
    document.getElementById("real-total-return").innerText = formatNumber(Number(((realAmount / initialInvestment - 1) * 100).toFixed(2)));
    document.getElementById("real-annualized").innerText = formatNumber(Number((realAnnualized * 100).toFixed(2)));
    document.getElementById("real-total").innerText = formatPortDollar(realAmount);
    
    // Find the maximum value in finalValueNominalDollarsArray and totalContributionsArray
    const maxNominalDollars = Math.max(...finalValueNominalDollarsArray);
    const maxRealDollars = Math.max(...finalValueRealDollarsArray);
    
    // Calculate a suitable maximum value for the y-axis. You can adjust this as needed.
    const yAxisMax = dynamicCeil(Math.max(maxNominalDollars, maxRealDollars));
    
    let maintainAspectRatio = true;
    if (window.innerWidth <= 767) {
        maintainAspectRatio = false;
    }
    
    var ctx = document.getElementById("myChart").getContext("2d");
    const dateLabels = selectedData.map(item => item.month);
    const myChart = new Chart(ctx, {
      type: "line",
      data: {
          labels: dateLabels, // Add your date labels here
          datasets: [{
              label: "Nominal Value (with dividends reinvested)",
              data: finalValueNominalDollarsArray, // your final value in nominal dollars array
              borderColor: "#349800",  // Green color
              fill: false,
              tension: 0 // Make the line straight
          },
          {
              label: "Inflation-Adjusted Value (with dividends reinvested)",
              data: finalValueRealDollarsArray, // your final value in real dollars array
              borderColor: "#d95f02",
              fill: false,
              tension: 0 // Make the line straight
          }
          ]
      },
      options: {
          responsive: true,
          maintainAspectRatio: maintainAspectRatio,
          scales: {
              xAxes: [{
                type: "time",
                time: {
                    unit: "month",
                    displayFormats: {
                        month: "MM/YYYY"
                    }
                },
                scaleLabel: {
                    display: true,
                    labelString: "Date"
                }
            }],
              yAxes: [{
                ticks: {
                  beginAtZero: true,
                  max: yAxisMax,  // Set the max value here
                  callback: function(value, index, values) {
                    if (yAxisMax < 10) {
                      return "$" + value.toFixed(2);
                    } else {
                      return "$" + value.toLocaleString();
                    }
                  }
                }
              }]
          },
           tooltips: {
            callbacks: {
                title: function(tooltipItem, data) {
                  // Convert the tooltip label to a Moment.js object
                  let label = tooltipItem[0]?.xLabel;
                  let date = moment(label);
          
                  // Format the date using Moment.js
                  return date.format("MM/YYYY");
                },
                labelColor: function(tooltipItem, chart) {
                  return {
                    borderColor: chart.data.datasets[tooltipItem.datasetIndex].borderColor,
                    backgroundColor: chart.data.datasets[tooltipItem.datasetIndex].borderColor
                  };
                },
                label: function(tooltipItem, data) {
                    let label = data.datasets[tooltipItem.datasetIndex].label || "";
                    if (label) {
                        label += ": ";
                    }
                    let formattedNumber = parseFloat(tooltipItem.yLabel).toLocaleString("en-US", {
                    minimumFractionDigits: 2, 
                    maximumFractionDigits: 2 
                    });
                    label += "$" + formattedNumber;
                    return label;
                }
            }
          }
      }
  });
  
  var stockPercent = parseFloat(document.getElementById("percentage-in-stocks").value);  // Fetch the value from your input field
  
  // Calculate the bond percentage
  var bondPercent = 100 - stockPercent;
  
  var initialI = parseFloat(document.getElementById("initial-investment").value);
  var initialIFormatted = formatNumberNoDecimals(initialI);
  
  // Convert month numbers to full month names
  var startMonthName = monthNumberToName(startMonthInt);
  var endMonthName = monthNumberToName(endMonthInt);

  myChart.options.title = {
      display: true,
      text: [`${stockPercent}/${bondPercent} Portfolio (U.S. Stock/Bond)`,
      `Initial Investment: $${initialIFormatted}`, 
      `${startMonthName} ${startYearInt} - ${endMonthName} ${endYearInt}`
      ],
      fontSize: 16
  };

// You may need to update the chart to see the new title
  myChart.update();
  
  window.addEventListener("resize", function() {
    if (window.innerWidth <= 767) {
        myChart.options.maintainAspectRatio = false;
    } else {
        myChart.options.maintainAspectRatio = true;
    }
    myChart.resize();
  });
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
    <title>U.S. Stock/Bond Historical Performance Calculator</title>
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
    <div class="investment-amounts">
    <label for="initialInvestment">Initial Investment:</label>
    <input type="number" id="initial-investment" name="initial-investment" value="1">
    <label for="percentage-in-stocks">Percentage in Stocks:</label>
    <input type="number" id="percentage-in-stocks" min="0" max="100" value="60">
    </div>
      <button onclick="calculatePortReturns()">Calculate</button>
      </div>
        <div class="results">
            <p><strong>Nominal Total Return (with dividends reinvested):</strong> <span id="nom-total-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="nom-annualized"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="nom-total"></span></p>
            
            <hr>
            
            <p><strong>Inflation-Adjusted Total Return (with dividends reinvested):</strong> <span id="real-total-return"></span>%</p>
            <p class="indented"><strong>Annualized:</strong> <span id="real-annualized"></span>%</p>
            <p class="indented"><strong>Investment Grew To:</strong> <span id="real-total"></span></p>
        </div>
        <hr>
        <div id="chart-container">
          <canvas id="myChart" width="400" height="200"></canvas>
        </div>
        <hr>'

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
                 start_month_html,
                 html_mid1,
                 start_year_html, 
                 html_mid2, 
                 end_month_html,
                 html_mid3, 
                 end_year_html, 
                 html_mid4,
                 html_js_script,
                 " const port_data = ", json_data, ";", 
                 js_function_string, 
                 html_end), 
           paste0(out_path, "/_test_stock_bond_calc.html"))

writeLines(paste(" const port_data = ", json_data, ";", 
                 js_function_string), 
           paste0(out_path, "/us_stock_bond_calculator.js"))

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
                 trimws(html_mid4)), 
           paste0(out_path, "/us_stock_bond_calculator.html"))

print(end_year)
print(end_month)

# ############################  End  ################################## #