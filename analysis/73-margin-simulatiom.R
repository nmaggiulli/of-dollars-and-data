cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(fTrading)
library(quantmod)
library(stats)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# This seed allows us to have reproducible random sampling
set.seed(12345)         

# Make a list of the best stocks (tickers) of 2014-2017 (from Motley Fool)
# I toss any that Yahoo cannot pull the data for
best_stocks_2014 <- c("AGIO", "BLUE", "ACHN",
                      "PTCT", "TTPH", "HA", "CMRX", "SWKS")
best_stocks_2015 <- c("NFLX", "AMZN", "ATVI", "NVDA",
                      "HRL", "VRSN", "SBUX", "FSLR")
best_stocks_2016 <- c("NVDA", "OKE", "FCX", "NEM", 
                      "AMAT", "PWR", "CMA", "MLM", "HAL")
best_stocks_2017 <- c("MDGL", "SGMO", "PIRS", "ESPR", 
                       "NKTR", "DVAX", "IMMU", "SPPI")

# Create a data frame for the year and the tickers
# Subset this data frame if need be
to_pull <- data.frame(ticker = 
                        c(
                        best_stocks_2014,
                        best_stocks_2015,
                        best_stocks_2016, 
                        best_stocks_2017),
                      year = 
                        c(
                        rep(2014, length(best_stocks_2014)),
                        rep(2015, length(best_stocks_2015)),
                        rep(2016, length(best_stocks_2016)), 
                        rep(2017, length(best_stocks_2017)))
                      ) %>%
            mutate(ticker = as.character(ticker))

# Starting capital
starting_capital <- 10000

# Number of trading days to simulate (NYSE average)
n_trading_days <- 2520

# Grab data if dummy is on
grab_data <- 0

if (grab_data == 1){
  
  
  # Create a function to grab the data from Yahoo
  grab_data <- function(ticker, year){
    print(ticker)
    getSymbols(ticker, from = paste0(year,'-01-01'), to = paste0(year,'-12-31'), 
               src="yahoo", periodicity = "daily")
    df <- data.frame(date = index(get(ticker)), 
                     get(ticker), row.names=NULL) %>%
      select(date, contains("Adjust"))
    
    colnames(df) <- c("date", "price")
    
    for (i in 1:nrow(df)){
      df[i, "ticker"] <- ticker
      if (i > 1){
        df[i, "ret"] <- df[i, "price"]/df[(i-1), "price"]
      }
    }
    return(df)
  }
  
  # Download the data for all the top performing stocks and stack it
  for (s in 1:nrow(to_pull)){
    temp <- grab_data(to_pull[s, "ticker"], to_pull[s, "year"])
    if (s == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
    }
  }
  
  final_df <- df %>%
                filter(!is.na(ret)) %>%
                group_by(date) %>%
                summarize(ret = sum(ret/n())) %>%
                ungroup() %>%
                mutate(trading_day = row_number())
  
  write.csv(final_df, paste0(exportdir, "73-margin-simulation/daily_ret_top_stocks.csv"), row.names = FALSE)
}

# Read in data saved on disk to save time
final_df <- read.csv(paste0(exportdir, "73-margin-simulation/daily_ret_top_stocks.csv")) %>%
              mutate(year = year(date)) %>%
              filter(year > 2013)

ret_matrix <- as.matrix(final_df$ret)

# Create a function to simulate the leveraged returns (resampling)
simulate_with_leverage <- function(n_simulations, leverage, sample_length){

  if (sample_length  > 1){
    # Pick days randomly to use in the simulation
    days_sample <- sample(1:nrow(ret_matrix), n_trading_days*n_simulations/sample_length, replace = TRUE)
  
    days_sample_final <- vector()
    for (i in 1:(length(days_sample))){
      for (j in 1:sample_length){
        if (j == 1){
          days_sample_final[sample_length*(i-1)+1] <- days_sample[i]
        } else{
          if (days_sample[i] > (nrow(final_df) - sample_length)){
            days_sample_final[sample_length*(i-1)+j] <- nrow(ret_matrix) - sample_length + j - 1
          } else {
            days_sample_final[sample_length*(i-1)+j] <- days_sample_final[sample_length*(i-1)+1] + (j-1)
          }
        }
      }
    }
  } else if (sample_length == 1){
    days_sample_final <- sample(1:nrow(ret_matrix), n_trading_days*n_simulations, replace = TRUE)
  }
  
  # Calculate the return for each day by looping through the number of trading days and
  # summing n_stocks worth (since they have already been weighted)
  results_matrix <- matrix(data = NA, nrow = n_trading_days, ncol = n_simulations)
  
  for (j in 1:n_simulations){
    for (i in 1:n_trading_days){
      # Calculate the equal weighted return across your N stocks
      ret <- ret_matrix[days_sample_final[i+(j-1)*n_simulations]]
      
      if (i == 1){
        results_matrix[i, j] <- starting_capital*leverage*ret
      } else{
        results_matrix[i, j] <- results_matrix[(i-1), j]*ret
      }
      
      # If your capital ever drops below your starting capital, game over
      if (results_matrix[i, j] - (starting_capital*(leverage-1)) < 0){
        results_matrix[i, j] <- 0
      }
    }
  }
  print(paste0("With ", leverage, "x leverage you go broke in ",
        100*(rowSums(results_matrix == 0)[n_trading_days]/n_simulations),
        "% of all simulations.")
      )
  #assign(paste0("final_results_", leverage), results_matrix, envir = .GlobalEnv)
}

sample_lengths <-c(1, 2, 4)
leverage_ratios <- c(2, 5, 10, 20, 50, 100, 500, 1000, 2000, 3000)

# Loop through different sample lengths and leverage ratios
# Note that sample_length must be 1 or an even number
for (s in sample_lengths){
  print(paste0("The day sample length is ", s))
  for (l in leverage_ratios){
    simulate_with_leverage(1000, l, s)
  }
}

# ############################  End  ################################## #