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

plot_simulation <- function(df){
  
  # Set simulation inputs
  bankroll <- 100
  bet_pct_vec <- seq(0.1, 0.5, 0.1)
  p_win <- df[, "p_win"]
  n_flips <- df[, "n_flips"]
  top_title <- df[, "title"]
  
  # Get the number of different betting percentages to test
  n_bet_pcts <- length(bet_pct_vec)
  
  # Simulate the flips and create a blank data frame for results
  flip_results <- runif(n_flips)
  df_results  <- data.frame(matrix(NA, nrow = n_bet_pcts * (n_flips + 1), ncol = 0))
  
  print(flip_results[1:5])
  
  index <- 0
  for (bet_pct in bet_pct_vec){
    for (i in 1:(n_flips+1)){
      index <- index + 1
      df_results[index, "bet_pct"] <- bet_pct
      df_results[index, "period"] <- i
      if (i == 1){
        df_results[index, "bankroll"] <- bankroll
      } else {
        if (flip_results[(i - 1)] < p_win){
          df_results[index, "bankroll"]  <- round(df_results[(index-1), "bankroll"] + (df_results[(index-1), "bankroll"] * bet_pct), 2)   
        } else {
          df_results[index, "bankroll"]  <- max(round(df_results[(index-1), "bankroll"] - (df_results[(index-1), "bankroll"] * bet_pct), 2), 0)
        }
      }
    }
  }
  
  to_plot <- df_results
  
  file_path <- paste0(exportdir, "41-kelly-betting-simulation/sim_results_flips", n_flips, "_win", (p_win*100), ".jpeg")
  
  plot<- ggplot(to_plot, aes(x = period, y = bankroll, col = as.factor(bet_pct))) +
          geom_line(alpha = 0.5) +
          scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(100, 10^5)) +
          ggtitle(top_title)  +
          of_dollars_and_data_theme +
          scale_color_discrete(guide = FALSE) +
          labs(x = "Number of Flips" , y = "Bankroll (on Log Scale)") +
          geom_text_repel(data = filter(to_plot, period == max(to_plot$period)),
                          aes(x = period, 
                              y = bankroll,
                              col = as.factor(bet_pct),
                              label = paste0(bet_pct*100, "%"),
                              family = "my_font"),
                          max.iter = 5000
          )
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes a win probability of ", p_win*100, "% on even odds.")
  
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

flips_vec <- c(100, 1000, 1000)
p_win_vec <- c(0.6, 0.6, 0.7)
title_vec <- c("The Optimal Bet Size Can Underperform\nOver Shorter Periods of Time",
               "Over Longer Periods of Time,\nThe Optimal Bet Size Will Emerge",
               "As Your Probability of Winning Increases,\nSo Should Your Bet Size")

simulations_to_run <- data.frame(n_flips = flips_vec,
                                 p_win = p_win_vec,
                                 title = title_vec)

for (i in 1:nrow(simulations_to_run)){
  plot_simulation(simulations_to_run[i, ])
}

## Part 2, find the equal weighted portfolio and how often it underperforms

# Load in BV returns and only keep assets in optimal portfolio
full_bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds")) %>%
                      select(year, `S&P 500`, `Int. Stocks`, `REIT`, `Treasury 10yr`, `Gold`)

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year, "%d/%m/%y")

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

# Create the optimal portfolio
# The weights come from analysis #6
# This was done manually for simplicity
full_bv_returns$port_optimal <- 0.34*full_bv_returns$`Treasury 10yr` +
                                0.29*full_bv_returns$`S&P 500` +
                                0.24*full_bv_returns$`REIT` +
                                0.10*full_bv_returns$`Gold` +
                                0.03*full_bv_returns$`Int. Stocks`
                                

## Count the number of assets with negative returns in a given year
for(i in 1:nrow(full_bv_returns)){
  counter <- 0
  for(j in 2:6){
    if(full_bv_returns[i,j] < 0){
      counter <- counter + 1
    }
  }
  full_bv_returns[i, "neg_returns"] <- counter
}

print(summary(full_bv_returns))
print(lapply(full_bv_returns, sd))

## Turn from wide to long and remove unnecessary columns
to_plot <- full_bv_returns %>%
            mutate(sp500 = `S&P 500`) %>%
            select(year, sp500, port_optimal) %>%
            gather(key=key, value=value, -year)


file_path <- paste0(exportdir, "41-kelly-betting-simulation/optimal_portfolio_vs_market.jpeg")

##Plot the bars
plot <- ggplot(to_plot, aes(x=year, y=value, fill=key)) +
          geom_bar(stat="identity", position="dodge") +
          ggtitle("Even An Optimal Portfolio Can\nLose Money 25% of the Time")  +
          of_dollars_and_data_theme + 
          scale_y_continuous(label = percent) +
          scale_fill_discrete(guide = FALSE) +
          scale_color_discrete(guide = FALSE) +
          labs(x = "Year" , y = "Annual Real Return (%)") +
          geom_text_repel(data = filter(to_plot, year == as.Date("1993-01-01", format="%Y-%m-%d"), key == "port_optimal"),
                  aes(x = as.Date("1993-01-01", format="%Y-%m-%d"), 
                      y = -0.10,
                      col = key,
                      label = "Optimal\nPortfolio",
                      family = "my_font"),
                  segment.color = 'transparent'
          ) +
          geom_text_repel(data = filter(to_plot, year == as.Date("2005-01-01", format="%Y-%m-%d"), key == "sp500"),
                          aes(x = as.Date("2005-01-01", format="%Y-%m-%d"), 
                              y = 0.35,
                              col = key,
                              label = "S&P 500",
                              family = "my_font"),
                          segment.color = 'transparent'
          )

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 

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
      
# ############################  End  ################################## #

  
