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

# Make a list of the best stock tickers of 2016 and 2017
best_stocks_2016 <- c("NVDA", "OKE", "FCX", "NEM", 
                      "AMAT", "PWR", "CMA", "MLM", "HAL")
best_stocks_2017 <- c("MDGL", "SGMO", "PIRS", "ESPR", 
                       "NKTR", "DVAX", "IMMU", "SPPI")

# Number of stocks in your portfolio simulation
n_stocks <- 10

# Average number of trading days in the NYSE in a year
n_trading_days <- 252

# Starting capital
starting_capital <- 10000

# Combine the best stock tickers into one big vector and make a year array
all_stocks <- c(best_stocks_2016, best_stocks_2017)
year_array <- c(rep(2016, length(best_stocks_2016)), rep(2017, length(best_stocks_2017)))

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
    if (i > 1){
      df[i, "ret"] <- df[i, "price"]/df[(i-1), "price"] - 1
    }
  }
  return(df)
}

# Download the data for all the top performing stocks and stack it
for (s in 1:length(all_stocks)){
  temp <- grab_data(all_stocks[s], year_array[s])
  if (s == 1){
    df <- temp
  } else {
    df <- bind_rows(df, temp)
  }
}

# Get the mean and sd for the daily returns
mean_ret <- mean(df$ret, na.rm = TRUE)
sd_ret <- sd(df$ret, na.rm = TRUE)

simulate_with_leverage <- function(n_simulations, leverage){

  # Simulate all trading days for the N stocks
  # Also multiply by 1/N to do an equal weighted portfolio
  daily_rets <- rnorm(n_trading_days*n_stocks*n_simulations, mean_ret, sd_ret)*(1/n_stocks)
  
  # Calculate the return for each day by looping through the number of trading days and
  # summing n_stocks worth (since they have already been weighted)
  results_matrix <- matrix(data = NA, nrow = n_trading_days, ncol = n_simulations)
  
  for (j in 1:n_simulations){
    for (i in 1:n_trading_days){
      # Calculate the equal weighted return across your N stocks
      ret <- (1 + sum(daily_rets[((j-1)*n_trading_days+(i-1)*n_stocks+1):
                                   ((j-1)*n_trading_days+i*n_stocks)]))
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
}

leverage_ratios <- c(2, 4, 6, 8, 10, 15, 20, 40, 50, 100)

for (l in leverage_ratios){
  simulate_with_leverage(1000, l)
}





# ############################  End  ################################## #