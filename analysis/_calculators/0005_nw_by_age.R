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

folder_name <- "_calculators/0005_nw_by_age"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022
calculate_data <- 0

if(calculate_data == 1){
  scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
                  filter(year == data_year,
                         age >= 23,
                         age <= 80)
        
  df <- scf_stack %>%
          select(hh_id, imp_id, 
                 net_worth, wgt, 
                 age) %>%
          arrange(hh_id, imp_id)
  
  pcts <- seq(0.01, 0.99, 0.01)
  
  final_pct_stack <- data.frame()
  
  for(p in pcts){
    tmp <- df %>%
    group_by(age) %>%
      summarise(
        pct = wtd.quantile(net_worth, weights = wgt, probs=p)
      ) %>%
      ungroup() %>%
      gather(-age, key=key, value=value) %>%
      mutate(pct = p) %>%
      select(-key)
    
    if(p == min(pcts)){
      final_pct_stack <- tmp
    } else{
      final_pct_stack <- final_pct_stack %>% bind_rows(tmp)
    }
  }
  
  saveRDS(final_pct_stack, paste0(localdir, "/nw_by_age_percentiles.Rds"))
}

to_calc <- readRDS(paste0(localdir, "/nw_by_age_percentiles.Rds"))

# Send data to JSON
json_data <- toJSON(to_calc, dataframe = "rows", pretty = TRUE)

#Create function string
js_function_string <- '
function formatNumberNoDecimals(number) {
    return number.toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 0});
}

function calculateRoundedMax(maxValue) {
    let roundedMax;

    if (maxValue >= 1000000) {
        // If the value is in millions, round up to the nearest hundred thousand
        roundedMax = Math.ceil(maxValue / 100000) * 100000;
    } else if (maxValue >= 100000) {
        // If the value is in hundred thousands, round up to the nearest ten thousand
        roundedMax = Math.ceil(maxValue / 10000) * 10000;
    } else {
        // For smaller values, you might want to adjust the rounding logic as needed
        roundedMax = Math.ceil(maxValue / 1000) * 1000;
    }

    return roundedMax;
}

function calculateStepSize(maxValue) {
    let stepSize;

    if (maxValue >= 10000000) {
        // If the max value is 10 million or more, round step size to the nearest million
        stepSize = Math.ceil(maxValue / 10 / 1000000) * 1000000;
    } else if (maxValue >= 1000000) {
        // If the max value is 1 million or more, but less than 10 million, round step size to the nearest half million
        stepSize = Math.ceil(maxValue / 10 / 500000) * 500000;
    } else {
        // For values less than a million, round step size to the nearest hundred thousand
        stepSize = Math.ceil(maxValue / 10 / 100000) * 100000;
    }

    return stepSize;
}

function calculatePercentile() {
    //Remove old chart
    document.getElementById("myChart").remove();
    
    // Create a new canvas element
      var newCanvas = document.createElement("canvas");
      newCanvas.setAttribute("id", "myChart");
      newCanvas.setAttribute("width", "400");
      newCanvas.setAttribute("height", "200");
    
    document.getElementById("chart-container").appendChild(newCanvas);

    const currentAge = parseInt(document.getElementById("age").value);
    const netWorth = parseInt(document.getElementById("net-worth").value);
    let percentile = "Not Found";
    
    // Check if age input is empty or invalid
    if (isNaN(currentAge)) {
        alert("Please enter a valid age.");
        return;
    }

    // Check if net worth input is empty or invalid
    if (isNaN(netWorth)) {
        alert("Please enter a valid net worth.");
        return;
    }
    
    if (currentAge < 23 || currentAge > 80) {
      alert("Age must be between 23 and 80.");
      return; // Exit the function early
    }
    
    // Find the closest net worth value for the given age
    const ageData = nw_data.filter(item => parseInt(item.age) === currentAge);

    if (ageData.length > 0) {
        // Find the closest net worth value for the given net worth
        const closest = ageData.reduce((prev, curr) => {
            return (Math.abs(curr.value - netWorth) < Math.abs(prev.value - netWorth) ? curr : prev);
        });

        percentile = (closest.pct * 100).toFixed(0) + "%";
    }
    
    // Update the result in the DOM
    document.getElementById("nw-percentile").innerText = percentile;
    
    // Find the maximum value in 
    const maxNetWorth = Math.max(...ageData.map(item => item.value));
    
    // Calculate a suitable maximum value for the y-axis. You can adjust this as needed.
    const yAxisMax = calculateRoundedMax(maxNetWorth);
    const stepSize = calculateStepSize(yAxisMax);
    
    let maintainAspectRatio = true;
    if (window.innerWidth <= 767) {
        maintainAspectRatio = false;
    }
    
    var ctx = document.getElementById("myChart").getContext("2d");
    
    // Prepare xLabels (percentiles) and yValues (net worth)
    const xLabels = ageData.map(item => (item.pct * 100).toFixed(0) + "%");  // Convert decimal to percentage string
    const yValues = ageData.map(item => item.value);

    const myChart = new Chart(ctx, {
      type: "line",
      data: {
          labels: xLabels, // Add your labels
          datasets: [{
              label: "U.S. Household Net Worth",
              data: yValues, // your values in the array
              borderColor: "#349800", //green color
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
                type: "category",
                scaleLabel: {
                    display: true,
                    labelString: "Percentile"
                }
            }],
              yAxes: [{
                ticks: {
                  beginAtZero: true,
                  max: yAxisMax,  // Set the max value here
                  stepSize: stepSize, // Set y-tick size here
                  callback: function(value, index, values) {
                return "$" + value.toLocaleString();
              }
            }
          }]
          },
                 tooltips: {
        callbacks: {
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
                minimumFractionDigits: 0, 
                maximumFractionDigits: 0 
                });
                label += "$" + formattedNumber;
                return label;
            }
        }
      }
      }
  });
  
  var ageFormatted = formatNumberNoDecimals(document.getElementById("age").value);
  
  myChart.options.title = {
    display: true,
    text: [`U.S. Household Net Worth by Percentile`, 
    `Age: ${ageFormatted}`],
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
      <title>Net Worth by Age Percentile Calculator</title>
      </head>
      <body>
      '
  
html_start2 <- '
  <!-- Your HTML content goes here -->
  <div class="calculator">
        <div class="inputs">
            <label for="age">Your Age:</label>
            <input type="number" id="age" name="age" placeholder="Enter your age">
            <label for="net-worth">Household Net Worth:</label>
            <input type="number" id="net-worth" name="net-worth" placeholder="Enter your household net worth">
        </div>
        <button onclick="calculatePercentile()">Calculate Percentile</button>
  </div>

  <div class="results">
      <p><strong>Household Net Worth Percentile: </strong><span id="nw-percentile"></span></p>
    </div>
    <hr>

  <div id="chart-container">
          <canvas id="myChart" width="400" height="200"></canvas>
  </div>
  <hr>
'
  
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
writeLines(paste(trimws(html_start1), trimws(html_start2), 
                 html_js_script, 
                 " const nw_data = ", json_data, ";", 
                 js_function_string, html_end), 
           paste0(out_path, "/_test_nw_by_age_calc.html"))

writeLines(paste("const nw_data = ", json_data, ";", 
                 js_function_string), 
           paste0(out_path, "/net_worth_by_age_calculator.js"))

html_end <- str_replace_all(html_end, "</script>", "")

writeLines(paste(trimws(html_start2)), 
           paste0(out_path, "/net_worth_by_age_calculator.html"))


# ############################  End  ################################## #