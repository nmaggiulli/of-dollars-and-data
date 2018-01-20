cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Create a set of savings rates and annual returns to loop through
savings_rates <- seq(0.05, 1, 0.01)
annual_returns <- seq(0.04, 0.1, 0.02)
after_tax_inc  <- 100000

# Create an empty results dataframe
results_df <- as.data.frame(matrix(NA, nrow = length(savings_rates)*length(annual_returns), ncol = 3))

# Create a counter
counter <- 0

# Loop through all annual returns and savings rates
for (annual_return in annual_returns){
  for (savings_rate in savings_rates){
    counter <- counter + 1
  
  live_inc      <- after_tax_inc * (1 - savings_rate)
  fv            <- live_inc/(annual_return) 
  
  n_periods_to_retire <- log(1 + ((fv * annual_return)/(savings_rate*after_tax_inc)))/log(1 + annual_return)
  
  results_df[counter, 1] <- paste0(annual_return*100, "%")
  results_df[counter, 2] <- savings_rate 
  results_df[counter, 3] <- n_periods_to_retire 
  
  }
}

# Add column names
names(results_df) <- c("annual_return", "savings_rate", "n_periods_to_retire")

# Create a copy of the dataframe for plotting
to_plot <- results_df 

# Plot the results
# Set note and source string
source_string <- str_wrap("Source: Simulated data  (OfDollarsAndData.com)", 
                          width = 80)
note_string   <- str_wrap(paste0("Note:  Assumes you save the same after-tax amount annually until retirement.  ",
                                 "Retirement is achieved once the annual returns from your total savings are the same as the amount of after-tax income you require to live on."), 
                          width = 80)

# Set output path
file_path <- paste0(exportdir, "56-brute-force-savings/savings-rate-vs-time.jpeg")

plot <- ggplot(to_plot, aes(x = savings_rate, y = n_periods_to_retire, col = annual_return)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  ggtitle(paste0("As You Increase Your Savings Rate,\nYour Returns Become Less Important"))  +
  scale_x_continuous(label = percent, breaks = seq(0.1, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  of_dollars_and_data_theme +
  geom_text_repel(data = filter(to_plot, savings_rate ==  min(to_plot$savings_rate), annual_return != "10%"),
                  aes(x = savings_rate, 
                      y = n_periods_to_retire,
                      label = annual_return,
                      col = annual_return,
                      family = "my_font"),
                  segment.color = 'transparent',
                  nudge_x =-0.01
  ) +
  geom_text_repel(data = filter(to_plot, savings_rate ==  min(to_plot$savings_rate), annual_return == "10%"),
                  aes(x = savings_rate, 
                      y = n_periods_to_retire,
                      label = annual_return,
                      col = annual_return,
                      family = "my_font"),
                  segment.color = 'transparent',
                  nudge_y = -7
  ) +
  labs(x = "Savings Rate (Annual)" , y = "Number of Years Until Retirement",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 

# ############################  End  ################################## #