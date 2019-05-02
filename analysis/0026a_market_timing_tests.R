cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(!is.na(cape), date < "2017-01-01")

first_year <- min(year(sp500_ret_pe$date))
last_year <- max(year(sp500_ret_pe$date))

# Create an empty data frame
results_df <- data.frame(start_date = numeric(), 
                         cape_min = numeric(),
                         cape_max = numeric(),
                         final_value = numeric()
                        )

# Function to test market timing
test_market <- function(cape_min, cape_max, start_date){
  
  # Create a temporary dataframe to hold the results
  temp_df <- data.frame(start_date = numeric(1), 
                           cape_min = numeric(1),
                           cape_max = numeric(1),
                           final_value = numeric(1)
  )
  
  # Filter the data to a 40 year period
  sp500 <- filter(sp500_ret_pe, date >= start_date, date <= start_date + 40)
  
  # Create an empty value vector
  value_vec <- rep(0, nrow(sp500))
  
  # Loop through the rows
  for (j in 1:nrow(sp500)){
    if (j == 1){
      value_vec[j] <- 100
      buy_up       <- 0
    } else {
      cape       <- sp500[j, "cape"]
      price      <- sp500[j, "price_plus_div"]
      price_prev <- sp500[(j - 1), "price_plus_div"]
      
      if (cape < cape_max & buy_up == 1){
        value_vec[j] <- value_vec[j - 1] * (1 + (price/price_prev - 1))
      } else if (cape > cape_min){
        buy_up <- 0
        value_vec[j] <- value_vec[j - 1]
      } else if (cape <= cape_min){
        buy_up       <- 1
        value_vec[j] <- value_vec[j - 1] * (1 + (price/price_prev - 1))
      }
    }
  }
  value_vec <- unlist(value_vec)
  temp_df$final_value     <- value_vec[nrow(sp500)]
  temp_df$start_date      <- start_date
  temp_df$cape_min        <- cape_min
  temp_df$cape_max        <- cape_max
  
  full <- get("results_df", envir = .GlobalEnv)
  full <- bind_rows(full, temp_df)
  assign("results_df", full, envir = .GlobalEnv)
}
  
s_dates <- seq.Date(as.Date("1881-01-01"), as.Date('1976-01-01'), "5 years")

for (s_date in s_dates){
  test_market(cape_min = 30, cape_max = 30, start_date = s_date)
  test_market(cape_min = 25, cape_max = 30, start_date = s_date)
  test_market(cape_min = 20, cape_max = 30, start_date = s_date)
  test_market(cape_min = 15, cape_max = 30, start_date = s_date)
  test_market(cape_min = 10, cape_max = 30, start_date = s_date)

  test_market(cape_min = 25, cape_max = 25, start_date = s_date)
  test_market(cape_min = 20, cape_max = 25, start_date = s_date)
  test_market(cape_min = 15, cape_max = 25, start_date = s_date)

  test_market(cape_min = 999, cape_max = 999, start_date = s_date)
}

write.csv(results_df, paste0(exportdir, "0026a_market_timing_tests/timing-results.csv"))

            

# ############################  End  ################################## #