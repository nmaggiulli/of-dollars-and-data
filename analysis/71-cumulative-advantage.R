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

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#000000", "#E41A1C", "#377EB8", "#4DAF4A")

# Set starting values for simulation
n_simulations    <- 1000
n_rounds        <- 40
results_df       <- matrix(NA, nrow = n_simulations, ncol = 1)

# Function to run simulations of cumulative advantage
run_share_simulation <- function(starting_advantage, n_colors){
  starting_marbles <- 100
  
  #This was solved using Wolfram Alpha
  win_marbles <- -(starting_advantage * n_colors * starting_marbles)/(starting_advantage - 1)
  
  win_marbles <- max(win_marbles, 1)
  
  #Loop through each simulation
  for (s in 1:n_simulations){
    
    # Initialize shares dataframe
    shares_df        <- matrix(NA, nrow = n_rounds, ncol = n_colors)
    marbles_vector   <- c()
    marbles_shares   <- c()
    total_marbles    <- starting_marbles * n_colors
    
    # Loop through the rounds
    for (i in 1:n_rounds){
      
      # Loop through each player to create a market share
      for (n in 1:n_colors){
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
      for (n in 1:n_colors){
        if (val < marbles_shares[n]){
          marbles_vector[n] <- marbles_vector[n] + win_marbles
          break
        }
      }
      # Increment the total_marbles
      total_marbles <- total_marbles + win_marbles
      shares_df[i, ] <- marbles_shares
      if (i == n_rounds){
        shares_df        <- as.data.frame(shares_df)
        shares_df$round <- seq(1, n_rounds)
    
      }
    }
    results_df[s, 1] <- shares_df[n_rounds, 1]
  }
  
  # Plot the distribution of final market share across all simulations
  results_df <- as.data.frame(results_df)
  
  if (starting_advantage == 0){
    starting_advantage_string <- paste0(starting_advantage, ".0")
  } else{
    starting_advantage_string <- paste0(starting_advantage)
  }
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "71-cumulative-advantage/dist_", starting_advantage_string,".jpeg")
  
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  Simulated data (OfDollarsAndData.com)"),
                            width = 85)
  
  main_note <- paste0("Note:  Assumes ", n_colors, " colors start with equal shares with 1 color 
                        given an advantage of ", 
                      100*round(starting_advantage, 2),
                      "% in the first round only.")
  
  note_string1 <- str_wrap(paste0(main_note, " Results shown are for ", n_simulations, " simulations of ",
                                  n_rounds, " rounds."),
                        width = 85)
  
  plot <- ggplot(results_df, aes(x=V1)) +
            geom_density(fill = "black") +
            of_dollars_and_data_theme +
            theme(axis.ticks.y     = element_blank(),
                  axis.text.y     = element_blank()) +
            scale_x_continuous(limits = c(0,1), label = percent) +
            ggtitle(paste0("Distribution of Final Market Share\n",
                           "With A Starting Advantage of ", 
                           100*round(starting_advantage, 2),
                           "%")) +
            labs(x = "Final Market Share", y = "Density",
                 caption = paste0("\n", source_string, "\n", note_string1))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Plot the market shares over time (only for final simulation)
  to_plot <- shares_df %>%
              gather(key=key, value=value, -round)
  
  to_plot$key <- reorder(to_plot$key, to_plot$value, function(x) -max(x) )
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "71-cumulative-advantage/shares_", starting_advantage_string,".jpeg")
  
  note_string2 <- str_wrap(main_note,
                           width = 85)
  
  plot <- ggplot(to_plot, aes(x=round, y=value, fill = key)) +
            geom_area(position="identity") +
            scale_fill_manual(guide = FALSE, values=my_palette) +
            geom_hline(yintercept = (1/n_colors), linetype = "dashed") + 
            of_dollars_and_data_theme +
            scale_y_continuous(label = percent) +
            scale_x_continuous() +
            ggtitle(paste0("Market Shares for 1 Simulation With\na Starting Advantage of ", 
                           100*round(starting_advantage, 2),
                           "% ")) +
            labs(x = "Round", y = "Market Share",
                 caption = paste0("\n", source_string, "\n", note_string2))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

share_seq <- seq(0.0, 0.5, 0.05)

for (s in share_seq){
  run_share_simulation(s, 4)
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# convert -delay 60 loop -0 dist-*.jpeg dist_plots.gif

# ############################  End  ################################## #