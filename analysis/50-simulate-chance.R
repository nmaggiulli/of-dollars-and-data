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
library(quadprog)
library(lubridate)
library(fTrading)
library(quantmod)
library(tidyr)
library(ggjoy)
library(dplyr)

########################## Start Program Here ######################### #

p_win         <- 0.8
ret_lose      <- -0.9
ret_win       <- 0.2
ret_sd        <- 0.05
n_simulations <- 20
n_periods     <- 20
start_wealth  <- 10000

# This seed allows us to have reproducible random sampling
set.seed(12345)  

# Create a blank results matrix
results_matrix <- matrix(NA, nrow = n_simulations, ncol = n_periods + 1)

# Set the starting wealth
results_matrix[, 1] <- rep(start_wealth, n_simulations)

# Loop through each period in the simulation
for (i in 2:(n_periods + 1)){
  uni     <- runif(n_simulations)
  rets_win  <- rnorm(n_simulations, ret_win, ret_sd)
  rets_lose <- rnorm(n_simulations, ret_lose, ret_sd)
  returns <- ifelse(uni > p_win, (1 + ret_lose), (1 + rets_win))
  results_matrix[, i] <- results_matrix[, (i-1)] * returns 
  results_matrix[, i] <- ifelse(results_matrix[, i] < 0.01, 0, results_matrix[, i])
}

#Create a periods column
periods <- seq(0, n_periods)

# Bind the periods and results and then use gather to transpose
df      <- cbind(as.data.frame(t(results_matrix)), periods)
df <- gather(df, key=key, value=value, -periods)

period_list <- c(10, 20)

for (p in period_list){
  to_plot <- filter(df, periods <= p)
  
  if (p == 10){
    title <- "Sometimes Losing Strategies Can\nPerform Well In the Short Run"
    y_max  <- 70000
    y_unit <- 10000
  } else {
    title <- "In the Long Run, Mean Reversion Sets In"
    y_max <- 100000
    y_unit <- 20000
  }

  
  # Plot the output
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "50-simulate-chance/simulated-chance-", p, ".jpeg")
  
  # Plot the fund capital over client capital
  # Each colored line is its own simulation
  plot <- ggplot(to_plot, aes(x = periods, y = value, col = key))  +
  geom_line() +
  scale_color_discrete(guide=FALSE) +
  scale_y_continuous(labels = dollar, breaks = seq(0, y_max, y_unit)) +
  scale_x_continuous(breaks = seq(0, p, 2)) +
  geom_hline(yintercept = start_wealth, col = "black", linetype = "dashed") +
  ggtitle(title)  +
  of_dollars_and_data_theme +
  labs(x = "Periods" , y = "Total Wealth ($)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Simulated returns (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes a ",  
                        p_win*100,"% chance of a ",  
                        ret_win*100,"% return, and a ", 
                        (1-p_win)*100,"% chance of a ", 
                        ret_lose*100,"% return.")
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}

# ############################  End  ################################## #

