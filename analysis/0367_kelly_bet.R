cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyverse)

folder_name <- "0367_kelly_bet"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

custom_dollar_sci <- function(x) {
  paste0("$", format(x, scientific = TRUE))
}

plot_sims <- function(n_flips){
  
  win_probability <- 0.1
  lose_probability <- 1 - win_probability
  starting_bankroll <- 100000
  win_size <- 10000
  lose_size <- 1
  kelly_bet_size <- 0.1
  sbf_bet_size <- 0.5
  n_sims <- 100
  
  set.seed(12345)
  
   simulation_results <- data.frame()
  
  counter <- 1
  for(i in 1:n_sims){
    print(i)
    flip_results <- runif(n_flips)
    for(j in 1:n_flips){
      simulation_results[counter, "simulation"] <- i
      simulation_results[counter, "flip"] <- j
      if(j == 1){
        if(flip_results[j] <= 0.1){
          simulation_results[counter, "kelly_bankroll"] <- starting_bankroll + (kelly_bet_size*starting_bankroll*win_size)
          simulation_results[counter, "sbf_bankroll"] <- starting_bankroll + (sbf_bet_size*starting_bankroll*win_size)
        } else{
          simulation_results[counter, "kelly_bankroll"] <- starting_bankroll - (kelly_bet_size*starting_bankroll*lose_size)
          simulation_results[counter, "sbf_bankroll"] <- starting_bankroll - (sbf_bet_size*starting_bankroll*lose_size)
        }
      } else{
        if(flip_results[j] <= 0.1){
          simulation_results[counter, "kelly_bankroll"] <- simulation_results[(counter-1), "kelly_bankroll"] + (kelly_bet_size*simulation_results[(counter-1), "kelly_bankroll"]*win_size)
          simulation_results[counter, "sbf_bankroll"] <- simulation_results[(counter-1), "sbf_bankroll"] + (sbf_bet_size*simulation_results[(counter-1), "sbf_bankroll"]*win_size)
        } else{
          simulation_results[counter, "kelly_bankroll"] <- simulation_results[(counter-1), "kelly_bankroll"] - (kelly_bet_size*simulation_results[(counter-1), "kelly_bankroll"]*lose_size)
          simulation_results[counter, "sbf_bankroll"] <- simulation_results[(counter-1), "sbf_bankroll"] - (sbf_bet_size*simulation_results[(counter-1), "sbf_bankroll"]*lose_size)
        }
      }
      
      if(simulation_results[counter, "kelly_bankroll"] < 0.01){
        simulation_results[counter, "kelly_bankroll"] <- 0
      }
      
      if(simulation_results[counter, "sbf_bankroll"] < 0.01){
        simulation_results[counter, "sbf_bankroll"] <- 0
      }
      
      counter <- counter + 1
    }
  }
  
  final_results <- simulation_results %>%
                    filter(flip == n_flips)
  
  assign("final_results", final_results, envir = .GlobalEnv)
  
  if(n_flips == 1){
    to_plot <- final_results %>%
      select(-flip, -simulation) %>%
      rename(`Kelly` = kelly_bankroll,
             `SBF` = sbf_bankroll) %>%
      gather(key=key, value=value)
    
    file_path <- paste0(out_path, "/kelly_bet_sim_results_", n_flips, ".jpeg")
    source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note: Assumes 100 simulations of ", n_flips, " flip(s) where you lose your bet ",
                                   100*lose_probability, "% of time and win ",  formatC(win_size, big.mark = ",", format = "f", digits = 0), "x your bet ", 100*win_probability, "% of the time. ",
                                   "Kelly betting assumes you bet 10% of your bankroll in each round. ",
                                   "SBF betting assumes you bet 50% of your bankroll in each round."),
                            width = 85)
    
    plot <- ggplot(data = to_plot, aes(x = value, fill = key)) +
      geom_density(alpha = 0.5) +
      scale_y_continuous(label = comma) +
      scale_x_continuous(label = dollar) +
      scale_fill_manual(values = c("black", "blue")) +
      ggtitle(paste0("Simulation Results After ", n_flips, " Flip(s)\nBy Betting Style")) +
      of_dollars_and_data_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      labs(x = "Final Bankroll" , y = "Frequency",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  } else if(n_flips > 1){
    to_plot <- final_results %>%
      select(-flip, -simulation) %>%
      rename(`Kelly` = kelly_bankroll,
             `SBF` = sbf_bankroll) %>%
      gather(key=key, value=value)
    
    file_path <- paste0(out_path, "/kelly_bet_sim_results_", n_flips, ".jpeg")
    source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note: Assumes 100 simulations of ", n_flips, " flip(s) where you lose your bet ",
                                   100*lose_probability, "% of time and win ",  formatC(win_size, big.mark = ",", format = "f", digits = 0), "x your bet ", 100*win_probability, "% of the time. ",
                                   "Kelly betting assumes you bet 10% of your bankroll in each round. ",
                                   "SBF betting assumes you bet 50% of your bankroll in each round."),
                            width = 85)
    
    plot <- ggplot(data = to_plot, aes(x = value, fill = key)) +
      geom_density(alpha = 0.5) +
      scale_y_continuous(label = comma) +
      scale_x_continuous(trans = log10_trans(), label = custom_dollar_sci) +
      scale_fill_manual(values = c("black", "blue")) +
      ggtitle(paste0("Simulation Results After ", n_flips, " Flip(s)\nBy Betting Style")) +
      of_dollars_and_data_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      labs(x = "Final Bankroll (Log Scale)" , y = "Frequency",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
    #Do bucketed plots for n_flips > 100
    if(n_flips == 100){
    to_plot <- final_results %>%
      select(-flip, -simulation) %>%
      rename(`Kelly` = kelly_bankroll,
             `SBF` = sbf_bankroll) %>%
      gather(key=key, value=value) %>%
      mutate(bankroll_bucket = case_when(
        value == 0 ~ "Bankrupt",
        value < starting_bankroll ~ "<1x Bankroll",
        value > starting_bankroll & value < 10000*starting_bankroll ~ "1x-10,000x Bankroll",
        TRUE ~ ">10,000x Bankroll"
      )) %>%
      group_by(key, bankroll_bucket) %>%
      summarise(count = n()) %>%
      ungroup()
    
    to_plot$bankroll_bucket <- factor(to_plot$bankroll_bucket,
                                      levels = c("Bankrupt",
                                                 "<1x Bankroll",
                                                 "1x-10,000x Bankroll",
                                                 ">10,000x Bankroll"))
    
    file_path <- paste0(out_path, "/kelly_bet_bucketed_results_", n_flips, ".jpeg")
    source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note: Assumes 100 simulations of ", n_flips, " flip(s) where you lose your bet ",
                                   100*lose_probability, "% of time and win ",  formatC(win_size, big.mark = ",", format = "f", digits = 0), "x your bet ", 100*win_probability, "% of the time. ",
                                   "Kelly betting assumes you bet 10% of your bankroll in each round. ",
                                   "SBF betting assumes you bet 50% of your bankroll in each round. ",
                                   "Once your bankroll is less than $0.01 you are considered bankrupt."),
                            width = 85)
    
    plot <- ggplot(data = to_plot, aes(x = bankroll_bucket, y=count/100, fill = key)) +
      geom_bar(position = "dodge", stat="identity") +
      scale_fill_manual(values = c("black", "blue")) +
      scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
      ggtitle(paste0("Simulation Results After ", n_flips, " Flip(s)\nBy Betting Style")) +
      of_dollars_and_data_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom") +
      labs(x = "Final Bankroll Bucket" , y = "Percentage of Simulations",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    }
  }
}

plot_sims(1)
plot_sims(10)
plot_sims(100)

# ############################  End  ################################## #