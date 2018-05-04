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
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# This seed allows us to have reproducible random sampling
set.seed(12345)         

# Set starting values for simulation
n_simulations    <- 100
n_periods        <- 40
results_df       <- matrix(NA, nrow = n_simulations, ncol = 1)

run_share_simulation <- function(starting_marbles, n_players, win_marbles){
  #Loop through each simulation
  for (s in 1:n_simulations){
    
    # Initialize shares dataframe
    shares_df        <- matrix(NA, nrow = n_periods, ncol = n_players)
    marbles_vector   <- c()
    marbles_shares   <- c()
    total_marbles    <- starting_marbles * n_players
    
    # Loop through the rounds
    for (i in 1:n_periods){
      
      # Loop through each player to create a market share
      for (n in 1:n_players){
        # Initialize number of marbles in the first round per player
        if (i == 1){
          marbles_vector[n] <- starting_marbles
          val <- 0
        } else {
          # Grab a random value between 0 and 1
          val <- runif(1)
        }
        
        # Create marble market shares
        if (n == 1){
          marbles_shares[n] <- marbles_vector[n]/total_marbles
        } else{
          marbles_shares[n] <- sum(marbles_vector[1:n])/total_marbles
        }
      }
      
      # Loop through each player to add winning marbles to the right player
      for (n in 1:n_players){
        if (val < marbles_shares[n]){
          marbles_vector[n] <- marbles_vector[n] + win_marbles
          break
        }
      }
      # Increment the total_marbles
      total_marbles <- total_marbles + win_marbles
      shares_df[i, ] <- marbles_shares
      if (i == n_periods){
        shares_df        <- as.data.frame(shares_df)
        shares_df$period <- seq(1, n_periods)
    
      }
    }
    results_df[s, 1] <- shares_df[n_periods, 1]
  }
  
  # Plot the distribution of final market share across all simulations
  results_df <- as.data.frame(results_df)
  
  # Calculate the initial advantage for the titles
  starting_advantage <-  (win_marbles+starting_marbles) / 
    ((n_players*starting_marbles) + win_marbles) - (1/n_players)
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "71-cumulative-advantage/dist_", win_marbles,".jpeg")
  
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  Simulated data (OfDollarsAndData.com)"),
                            width = 85)
  
  note_string <- str_wrap(paste0("Note:  Assumes ", n_players, " players start with equal shares with 1 player 
                        given an advantage of ", 
                        100*round(starting_advantage, 2),
                        "% in the first period only."),
                        width = 85)
  
  plot <- ggplot(results_df, aes(x=V1)) +
            geom_density(fill = "black") +
            of_dollars_and_data_theme +
            scale_x_continuous(limits = c(0,1), label = percent) +
            ggtitle(paste0("Distribution of Final Market Share\nAcross ", n_simulations, 
                           " Simulations With\nA Starting Advantage of ", 
                           100*round(starting_advantage, 2),
                           "%")) +
            labs(x = "Final Market Share", y = "Density",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Plot the market shares over time (only for final simulation)
  to_plot <- shares_df %>%
              gather(key=key, value=value, -period)
  
  to_plot$key <- reorder(to_plot$key, to_plot$value, function(x) -max(x) )
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "71-cumulative-advantage/shares_", win_marbles,".jpeg")
  
  plot <- ggplot(to_plot, aes(x=period, y=value, fill = key)) +
            geom_area(position="identity") +
            scale_fill_discrete(guide = FALSE) +
            of_dollars_and_data_theme +
            scale_y_continuous(label = percent) +
            scale_x_continuous() +
            ggtitle(paste0("Market Shares for 1 Simulation With\na Starting Advantage of ", 
                           100*round(starting_advantage, 2),
                           "% ")) +
            labs(x = "Period", y = "Market Share",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

run_share_simulation(100, 4, 5)
run_share_simulation(100, 4, 100)
run_share_simulation(100, 4, 600)

# ############################  End  ################################## #