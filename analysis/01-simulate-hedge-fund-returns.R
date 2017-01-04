cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(MASS)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)

########################## Start Program Here ######################### #

# Set the initial values for the simulation
n_simulations <- 10000
initial_client_capital <- 1 * 10^9                            
initial_hedge_fund_capital <- 0

# This seed allows us to have reproducible random sampling
set.seed(12345)                                   

plot_sim <- function(
                  hedge_fund_management_fee, 
                  hedge_fund_performance_fee, 
                  hedge_fund_watermark, 
                  sample_mean_return, 
                  sample_sd_return,
                  n_periods,
                  file_name,
                  top_title,
                  source_string,
                  note_string
){
  
  # Initialze matrix for the returns and value paths
  # I use matrices because they are much faster than data frames in this regard
  asset_return_matrix     <- matrix(NA, nrow = n_simulations, ncol = n_periods)
  client_value_matrix     <- matrix(initial_client_capital, nrow = n_simulations, ncol = (n_periods + 1))
  hedge_fund_value_matrix <- matrix(initial_hedge_fund_capital, nrow = n_simulations, ncol = (n_periods + 1))
  compare_value_matrix    <- matrix(NA, nrow = n_simulations, ncol = (n_periods + 1))
  hf_over_client_value    <- matrix(NA, nrow = n_simulations, ncol = (n_periods + 1))
  
  # Set the first period of the comparison matrices manually
  # Note that the first column in these matrices represents the starting (0-th period)
  # in the simulation
  compare_value_matrix[, 1] <- client_value_matrix[, 1] > hedge_fund_value_matrix[, 1]
  hf_over_client_value[, 1] <- hedge_fund_value_matrix[, 1] / client_value_matrix[, 1] 
  
  
  # Run the simulations for each period by getting asset returns and calculating asset values
  # This assumes the returns are annualized
  # Start at period 2 and go to n_periods + 1 (since period 1 is actually the 0-th period)
  for (i in 2:(n_periods + 1)){
    # Sample from a normal distribution n_simulations for each of the periods in our model
    # Put the simulated returns into each column (period) of the data
    asset_return_matrix[, i-1] <- rnorm(n_simulations, sample_mean_return, sample_sd_return)
    
    # At this point each row is a different simulation and the column is the i-th period
    # Calculate the asset values after each period in the simulation
    # This assumes that the hedge fund reinvests all of its capital alongside its client
    management_fee  <- client_value_matrix[, i-1] * (hedge_fund_management_fee)
    performance_fee <- (client_value_matrix[, i-1] * (1 + asset_return_matrix[, i-1]) - client_value_matrix[, i-1]) * 
                        hedge_fund_performance_fee * 
                        (asset_return_matrix[, i-1] > hedge_fund_watermark)    
    
    hedge_fund_value_matrix[, i] <- (hedge_fund_value_matrix[, i-1] * (1 + asset_return_matrix[, i-1])) + management_fee + performance_fee
    client_value_matrix[, i]     <- (client_value_matrix[, i-1] * (1 + asset_return_matrix[, i-1])) - management_fee - performance_fee
  
    # Compare the hedge fund's total capital to that of its client
    compare_value_matrix[, i] <- client_value_matrix[, i] > hedge_fund_value_matrix[, i]
    hf_over_client_value[, i] <- hedge_fund_value_matrix[, i] / client_value_matrix[, i]
  }
  # Calculate the number of periods until the hedge fund has more money than its client
  n_periods_until_50pct <- sum(colSums(compare_value_matrix) > (n_simulations / 2))
  
  hf_over_client_df <- as.data.frame(t(hf_over_client_value))
  assign("hf_over_client_df",  hf_over_client_df, envir = .GlobalEnv)
  
  # Define the number of periods for plotting
  periods <- seq(0, n_periods)
  
  # Plot a subset of the simulations for visual clarity and to reduce memory size of plot
  to_plot <- cbind(hf_over_client_df[, 1:(n_simulations/100)], periods)
  to_plot <- melt(to_plot, id.vars = "periods")
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "01-simulate-hedge-fund-returns/", file_name)
  
  # Plot the fund capital over client capital
  # Each colored line is its own simulation
  plot <- ggplot(to_plot, aes(x = periods, y = value, col = variable))  +
    geom_line() +
    # The horizontal line is the point where the fund has the same capital as its client
    geom_hline(yintercept = 1) +
    # The vertical line is the average number of years before the hedge fund has more capital than its client 
    geom_vline(xintercept = n_periods_until_50pct, col = "black", linetype = "longdash", alpha = 0.25) +
    # Removes legend  
    scale_color_discrete(guide=FALSE) +
    scale_y_continuous(limits = c(0, 3), labels = percent) +
    ggtitle(top_title)  +
    of_dollars_and_data_theme +
    labs(x = "Number of Years Invested" , y = "Fund Capital Over Client Capital")
  
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                    gp =gpar(fontfamily = "my_font", fontsize = 7))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                   gp =gpar(fontfamily = "my_font", fontsize = 7))
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}

# This first simulation assumes a 2 and 20 traditional hedge fund fee structure
plot_sim(
  hedge_fund_management_fee = 0.02, 
  hedge_fund_performance_fee = 0.2, 
  hedge_fund_watermark = 0, 
  sample_mean_return = 0.1, 
  sample_sd_return = 0.2,
  n_periods = 50,
  file_name = "hf_over_client_2_and_20_sp500_return.jpeg",
  top_title = "On Average, the Hedge Fund is Richer\nThan Its Clients In Less Than 20 Years",
  source_string = "Source:  Simulated returns",
  note_string = "Note:  Assumes a 2% management fee and a 20% performance fee."
)

# This second simulation assumes a 1.4% management fee and 17% performance fee
# These numbers come from an article showing changes in hedge fund fees
# See here:  http://fortune.com/2016/09/15/hedge-fund-fees-cut/
plot_sim(
  hedge_fund_management_fee = 0.014, 
  hedge_fund_performance_fee = 0.17, 
  hedge_fund_watermark = 0, 
  sample_mean_return = 0.1, 
  sample_sd_return = 0.2,
  n_periods = 50,
  file_name = "hf_over_client_1.4_and_17_sp500_return.jpeg",
  top_title = "On Average, the Hedge Fund is Richer\nThan Its Clients In Less Than 25 Years",
  source_string = "Source:  Simulated returns",
  note_string = "Note:  Assumes a 1.4% management fee and a 17% performance fee."
)

# The third model represents a Vanguard index fund with 0.05% (5 basis points) for its
# annual expense ratio.  There is no performance fee or watermark in this scenario.
plot_sim(
  hedge_fund_management_fee = 0.0005, 
  hedge_fund_performance_fee = 0.0, 
  hedge_fund_watermark = 0.0, 
  sample_mean_return = 0.1, 
  sample_sd_return = 0.2,
  n_periods = 1500,
  file_name = "vanguard_over_client_0.05pct_sp500_return.jpeg",
  top_title = "A Low-Cost Index Fund Would Take\nAlmost 1,500 Years To Be Richer Than Its Clients",
  source_string = "Source:  Simulated returns",
  note_string = "Note:  Assumes a 0.05% management fee and no performance fee."
)

# ############################  End  ################################## #