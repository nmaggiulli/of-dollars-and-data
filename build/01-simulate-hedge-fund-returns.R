cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(MASS)
library(dplyr)

########################## Start Program Here ######################### #

# Set the initial values for the simulation
n_simulations <- 10000
initial_client_capital <- 1 * 10^9                            
initial_hedge_fund_capital <- 0
n_periods <- 50

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

# This will follow the 2 and 20 model initially but can be made dynamic
hedge_fund_management_fee <- 0.02
hedge_fund_performance_fee <- 0.2
hedge_fund_watermark <- 0.05
sample_mean_return <- 0.1
sample_sd_return <- 0.2

# Initialze matrix for the returns and value paths
# I use matrices because they are much, much faster than data frames in this regard
asset_return_matrix <- matrix(NA,nrow=n_simulations,ncol=n_periods)
client_value_matrix <- matrix(NA,nrow=n_simulations,ncol=n_periods)
hedge_fund_value_matrix <- matrix(NA,nrow=n_simulations,ncol=n_periods)
compare_value_matrix <- matrix(NA,nrow=n_simulations,ncol=n_periods)

# Run the simulations for each period by getting asset returns and calculating asset values
# This assumes the returns are annualized
for (i in 1:n_periods){
  # Sample from a normal distribution n_simulations for each of the periods in our model
  # Put the simulated returns into each column (period) of the data
  asset_return_matrix[,i] <- rnorm(n_simulations, sample_mean_return, sample_sd_return)
  
  # At this point each row is a different simulation and the column is the i-th period
  # Calculate the asset values after each period in the simulation
  if (i == 1){
    management_fee <- initial_client_capital * (hedge_fund_management_fee)
    performance_fee <- (initial_client_capital * (1 + asset_return_matrix[,i]) - initial_client_capital) * 
                        hedge_fund_performance_fee * 
                        (asset_return_matrix[,i] > hedge_fund_watermark)
    hedge_fund_value_matrix[,i] <- management_fee + performance_fee
    client_value_matrix[,i] <- (initial_client_capital * (1 + asset_return_matrix[,i])) - management_fee - performance_fee
  } else if (i > 1){
    management_fee <- client_value_matrix[,i-1] * (hedge_fund_management_fee)
    performance_fee <- (client_value_matrix[,i-1] * (1 + asset_return_matrix[,i]) - client_value_matrix[,i-1]) * 
                        hedge_fund_performance_fee * 
                        (asset_return_matrix[,i] > hedge_fund_watermark)    
    hedge_fund_value_matrix[,i] <- (hedge_fund_value_matrix[,i-1] * (1 + asset_return_matrix[,i])) + management_fee + performance_fee
    client_value_matrix[,i] <- (client_value_matrix[,i-1] * (1 + asset_return_matrix[,i])) - management_fee - performance_fee
  }
  
  # Compare the hedge fund's total capital to that of its client
  compare_value_matrix[,i] <- client_value_matrix[,i] > hedge_fund_value_matrix[,i]
}

#Print the number of periods until the hedge fund has more capital than their client
sum(colSums(compare_value_matrix) > n_simulations/2) + 1


# ############################  End  ################################## #