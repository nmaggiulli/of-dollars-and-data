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
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                    filter(cape != "NA", date < 2017.01)

first_year <- floor(min(sp500_ret_pe$date))
last_year <- floor(max(sp500_ret_pe$date))

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
}

# Convert the cape to a numeric
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)

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
  
s_dates <- seq(1881.01, 1976.01, 5)

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

write.csv(results_df, paste0(exportdir, "26a-market-timing-tests/timing-results.csv"))

            

# ############################  End  ################################## #