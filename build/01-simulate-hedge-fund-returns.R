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
intial_investor_value <- 1 * 10^9                            
initial_hedge_fund_value <- 0
n_periods <- 50

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

# This will follow the 2 and 20 model initially but can be made dynamic
hedge_fund_management_fee <- 0.02
hegde_fund_performance_fee <- 0.2
sample_mean_return <- 0.1
sample_sd_return <- 0.2

# Initialze matrix for the returns 
# I use matrices because they are much, much faster than data frames in this regard
asset_return_matrix <- matrix(NA,nrow=n_simulations,ncol=n_periods)

# Sample from a normal distribution n_simulations for each of the periods in our model
# Put the simulated returns into each column (period) of the data
for (i in 1:n_periods){
  asset_return_matrix[,i] <- rnorm(n_simulations, sample_mean_return, sample_sd_return)
}

# At this point each row is a simulation and each column is a period


# ############################  End  ################################## #