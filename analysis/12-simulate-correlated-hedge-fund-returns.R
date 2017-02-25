cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)

########################## Start Program Here ######################### #

#Initial setup for full simulation

# Found this online for correlated samples:
#
#  http://stats.stackexchange.com/questions/82261/generating-correlated-distributions-with-a-certain-mean-and-standard-deviation
#

# Set the initial client capital and number of simulations
n_simulations <- 10000
initial_client_capital <- 1 * 10^6      
n_years <- 40

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

# Set the mean and sd of the returns for the market
mu_market         <- 0.1                            
sd_market         <- 0.2 

run_sim <- function(hf_outperformance, 
                    hf_management_fee,
                    hf_performance_fee,
                    hf_deduct_fees,
                    market_management_fee,
                    hf_corr_to_market,
                    file_name,
                    top_title,
                    source_string,
                    note_string
                    ){
  
  # Set the mean and standard deviation for the hedge fund
  # This assumes some level of outperformance by the hedge fund
  mu_hf             <- mu_market + hf_outperformance                                 
  sd_hf             <- sd_market                              
  
  # Correlation between the market and the hedge fund (~0.9 based on recent data)
  rho   <- hf_corr_to_market
  
  # Set the correlation matrix
  cor_matrix <- matrix(c(1, rho,                    
                         rho, 1),
                       ncol = 2, byrow = TRUE)
  
  # Set the variance matrix
  var_matrix <- c(sd_market, sd_hf) %*% t(c(sd_market, sd_hf))  
  
  # Set the covariance matrix
  cov_matrix <-  var_matrix * cor_matrix            
  
  # Initialize value path matrices
  # One is for the client of the hedge fund and one is for the client of the index fund
  client_hf_matrix         <- matrix(NA, nrow = n_simulations, ncol = n_years)
  client_market_matrix     <- matrix(NA, nrow = n_simulations, ncol = n_years)
  
  # Simulate each year
  for (i in 1:n_years){
    # Generate the correlated returns for each simulation
    # Note ret[, 1] == market return, ret[, 2] == hedge fund return
    ret <- mvrnorm(n_simulations, mu = c(mu_market, mu_hf),
                   Sigma = cov_matrix,
                   empirical = TRUE)
    
    # Determine if there is any alpha for each simulation
    alpha <- pmax(0, ret[, 2] - ret[, 1])
    
    if (i == 1){
      client_market_matrix[, i] <- initial_client_capital * (1 + ret[, 1]) * (1 - market_management_fee)
      management_fee            <- initial_client_capital * hf_management_fee
      performance_fee           <- initial_client_capital * alpha * hf_performance_fee
      hf_final_fee              <- pmax(management_fee, performance_fee)
      client_hf_matrix[, i]     <- (initial_client_capital * (1 + ret[, 2])) - hf_final_fee
      if (hf_deduct_fees == 1){
        fee_deduction             <- sapply(performance_fee, function(x){ ifelse(x == 0, management_fee, 0)})
      }else {
        fee_deduction <- 0
      }
    } else {
      client_market_matrix[, i] <- client_market_matrix[, (i - 1)] * (1 + ret[, 1]) * (1 - market_management_fee)
      management_fee            <- client_hf_matrix[, (i - 1)] * hf_management_fee
      performance_fee           <- client_hf_matrix[, (i - 1)] * alpha * hf_performance_fee  
      hf_final_fee              <- pmax(0, pmax(management_fee, performance_fee) - fee_deduction)
      client_hf_matrix[, i]     <- (client_hf_matrix[, (i - 1)] * (1 + ret[, 2])) - hf_final_fee
      if (hf_deduct_fees == 1){
        fee_deduction             <- sapply(performance_fee, function(x){ ifelse(x == 0, management_fee, 0)})
      }else {
        fee_deduction <- 0
      }
    }
  }
  assign("client_hf_matrix", client_hf_matrix, envir = .GlobalEnv)
  assign("client_market_matrix", client_hf_matrix, envir = .GlobalEnv)
  print(paste0("Final market value: ",format(mean(client_market_matrix[, n_years]), big.mark = ",")))
  print(paste0("Final hedge fund value: ", format(mean(client_hf_matrix[, n_years]), big.mark = ",")))
}



# Set simulation parameters
run_sim(
  hf_outperformance = 0.015,
  hf_management_fee = 0.01, 
  hf_performance_fee = 0.3, 
  hf_deduct_fees = 1,
  market_management_fee = 0.0005,
  hf_corr_to_market = 0.9,
  file_name = "hf_over_client_2_and_20_sp500_return.jpeg",
  top_title = "On Average, the Hedge Fund is Richer\nThan Its Clients In Less Than 20 Years",
  source_string = "Source:  Simulated returns (OfDollarsAndData.com)",
  note_string = "Note:  Assumes a 2% management fee and a 20% performance fee."
)


# ############################  End  ################################## #
