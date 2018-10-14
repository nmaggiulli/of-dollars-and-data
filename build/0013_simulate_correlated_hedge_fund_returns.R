cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(MASS)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

#Initial setup for full simulation

# Found this online for correlated samples:
#
#  http://stats.stackexchange.com/questions/82261/generating-correlated-distributions-with-a-certain-mean-and-standard-deviation
#

# Set the initial client capital and number of simulations
n_simulations          <- 10000
initial_client_capital <- 1 * 10^6      
n_years                <- 40

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

# Set the mean and sd of the returns for the market
mu_market         <- 0.1                            
sd_market         <- 0.2 

run_sim <- function(hf_outperformance, 
                    hf_management_fee,
                    hf_performance_fee,
                    hf_performance_above_benchmark,
                    management_and_performance_fee,
                    hf_deduct_fees,
                    market_management_fee,
                    hf_corr_to_market
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
    
    # Determine if there is any ret_above_benchmark for each simulation
    if (hf_performance_above_benchmark == 1){
      ret_above_benchmark <- pmax(0, ret[, 2] - ret[, 1])
    } else {
      ret_above_benchmark <- pmax(0, ret[, 2])
    }
    
    if (i == 1){
      client_market_matrix[, i]   <- initial_client_capital * (1 + ret[, 1]) * (1 - market_management_fee)
      management_fee              <- initial_client_capital * hf_management_fee
      if (management_and_performance_fee == 0){
        performance_fee           <- initial_client_capital * ret_above_benchmark * hf_performance_fee  
        hf_final_fee              <- pmax(management_fee, performance_fee)
      } else {
        performance_fee           <- pmax(0, ((initial_client_capital * ret_above_benchmark - management_fee) * hf_performance_fee))
        hf_final_fee              <- management_fee + performance_fee
      }
      client_hf_matrix[, i]       <- (initial_client_capital * (1 + ret[, 2])) - hf_final_fee
      if (hf_deduct_fees == 1){
        fee_deduction             <- sapply(performance_fee, function(x){ ifelse(x == 0, management_fee, 0)})
      }else {
        fee_deduction <- 0
      }
    } else {
      client_market_matrix[, i]   <- client_market_matrix[, (i - 1)] * (1 + ret[, 1]) * (1 - market_management_fee)
      management_fee              <- client_hf_matrix[, (i - 1)] * hf_management_fee
      if (management_and_performance_fee == 0){
        performance_fee           <- client_hf_matrix[, (i - 1)] * ret_above_benchmark * hf_performance_fee  
        hf_final_fee              <- pmax(0, pmax(management_fee, performance_fee) - fee_deduction)
      } else {
        performance_fee           <- pmax(0, ((client_hf_matrix[, (i - 1)] * ret_above_benchmark - management_fee) * hf_performance_fee))
        hf_final_fee              <- management_fee + performance_fee
      }
      client_hf_matrix[, i]       <- (client_hf_matrix[, (i - 1)] * (1 + ret[, 2])) - hf_final_fee
      if (hf_deduct_fees == 1){
        fee_deduction             <- sapply(performance_fee, function(x){ ifelse(x == 0, management_fee, 0)})
      }else {
        fee_deduction <- 0
      }
    }
  }
  print(paste0("The hedge fund outperformed the market (net of fees) in ", sum(client_hf_matrix[, n_years] > client_market_matrix[, n_years])/n_simulations * 100, "% of simulations"))
  return(sum(client_hf_matrix[, n_years] > client_market_matrix[, n_years])/n_simulations)
}

# Code to do a quick sample run for testing
# I could delete it and do a checkout later in Git, but I am lazy
# run_sim(
#   hf_outperformance              = 0.01,
#   hf_management_fee              = 0,
#   hf_performance_fee             = 0.5,
#   hf_performance_above_benchmark = 1,
#   management_and_performance_fee = 0,
#   hf_deduct_fees                 = 0,
#   market_management_fee          = 0.0005,
#   hf_corr_to_market              = 0.9
# )

# Create a matrix to store the results
results <- data.frame(hf_outperformance              = numeric(),
                      hf_management_fee              = numeric(),
                      hf_performance_fee             = numeric(),
                      hf_performance_above_benchmark = integer(),
                      management_and_performance_fee = integer(),
                      hf_deduct_fees                 = integer(),
                      market_management_fee          = numeric(),
                      hf_corr_to_market              = numeric(),
                      hf_outperform_pct              = numeric(),
                      scenario                       = integer(),
                      mu_market                      = numeric(),
                      sd_market                      = numeric()
                      )

# Loop through outperformance, correlation, and other sensitivities
i <- 1
for (o in seq(0, 0.04, by = 0.01)){
  for (c in c(0, 0.5, 0.9)){
    for (scenario in seq(1, 3, by = 1)){
      if (scenario == 1){
        mf  <- 0.01
        pf  <- 0.3
        pab <- 1
        map <- 0
        df  <- 1
      } else if (scenario == 2){
        mf  <- 0.02
        pf  <- 0.2
        pab <- 0
        map <- 1
        df  <- 0
      } else if (scenario == 3){
        mf  <- 0.01
        pf  <- 0.0
        pab <- 0
        map <- 1
        df  <- 0
      }
      
      results[i, "hf_outperformance"]              <- o
      results[i, "hf_management_fee"]              <- mf
      results[i, "hf_performance_fee"]             <- pf
      results[i, "hf_performance_above_benchmark"] <- pab
      results[i, "management_and_performance_fee"] <- map
      results[i, "hf_deduct_fees"]                 <- df
      results[i, "market_management_fee"]          <- 0.0005
      results[i, "hf_corr_to_market"]              <- c
      results[i, "scenario"]                       <- scenario
      results[i, "mu_market"]                      <- mu_market
      results[i, "sd_market"]                      <- sd_market
      
      results[i, "hf_outperform_pct"] <- run_sim(
        hf_outperformance              = o,
        hf_management_fee              = mf,
        hf_performance_fee             = pf,
        hf_performance_above_benchmark = pab,
        management_and_performance_fee = map,
        hf_deduct_fees                 = df,
        market_management_fee          = 0.0005,
        hf_corr_to_market              = c
      )
      i <- i + 1
      print(i)
    }
  }
}

# Save down RDS given the loops above take a few minutes to run
saveRDS(results, paste0(localdir, "13-hf-correlation-results.Rds"))

# ############################  End  ################################## #
