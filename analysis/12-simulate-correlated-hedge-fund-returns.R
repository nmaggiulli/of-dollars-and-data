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
n_simulations <- 2
initial_client_capital <- 1 * 10^6      
n_years <- 2

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

# Set the mean and sd of the returns for the market
mu_market         <- 0.1                            
sd_market         <- 0.2 

run_sim <- function(hf_outperformance, 
                    hf_management_fee,
                    hf_performance_fee,
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
  
  #Set the correlation matrix
  cor_matrix <- matrix(c(1, rho,                    
                         rho, 1),
                       ncol = 2, byrow = TRUE)
  
  #Set the variance matrix
  var_matrix <- c(sd_market, sd_hf) %*% t(c(sd_market, sd_hf))  
  
  #Set the covariance matrix
  cov_matrix <-  var_matrix * cor_matrix            
  
  # Simulate each year
  for (i in 1:n_years){
    #Generate the correlated returns for each simulation
    ret <- mvrnorm(n_simulations, mu = c(mu_market, mu_hf),
                   Sigma = cov_matrix,
                   empirical = TRUE)
    
    #Convert to data.frame
    ret <- data.frame(ret)
    
    #Set column names
    colnames(ret) <- c("ret_market", "ret_hf")
    
    ret <- mutate(ret, alpha = ifelse(ret_hf > ret_market, ret_hf - ret_market, 0))
    
    print(head(ret))
  }
}



# This first simulation assumes a 2 and 20 traditional hedge fund fee structure
run_sim(
  hf_outperformance = 0.01,
  hf_management_fee = 0.1, 
  hf_performance_fee = 0.3, 
  hf_corr_to_market = 0.9,
  file_name = "hf_over_client_2_and_20_sp500_return.jpeg",
  top_title = "On Average, the Hedge Fund is Richer\nThan Its Clients In Less Than 20 Years",
  source_string = "Source:  Simulated returns (OfDollarsAndData.com)",
  note_string = "Note:  Assumes a 2% management fee and a 20% performance fee."
)


# ############################  End  ################################## #