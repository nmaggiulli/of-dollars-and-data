cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(magrittr)

out_path <- paste0(exportdir, "0026b_sp500_global_max")

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(date >= "1800-01-01")

first_year <- min(year(sp500_ret_pe$date))
last_year <- max(year(sp500_ret_pe$date))

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "global_max"]      <- 1
    global_max                         <- sp500_ret_pe[i, "price_plus_div"]
  } else{
    if (sp500_ret_pe[i, "price_plus_div"] > global_max){
      sp500_ret_pe[i, "global_max"]      <- 1
      global_max                         <- sp500_ret_pe[i, "price_plus_div"]
    } else{
      sp500_ret_pe[i, "global_max"]      <- 0
    }
  }
}

# Function for setting up breaks on y axis for the plot
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# Get the original price for the first date
orig_price <- as.numeric(sp500_ret_pe[1, "price_plus_div"])

# Use the original price to rescale to $1 for the first month
sp500_ret_pe %<>%
  mutate(price_final = price_plus_div / orig_price)

# Get the years sequence and add the last year a few extra times for the GIF
years <- seq(first_year, last_year, 1)

years <- years[which(years %% 4 == 0)]

years <- c(years, rep(last_year, 12))

for (i in 2:length(years)){
  to_plot <- sp500_ret_pe %>%
                filter(year(date) <= years[i])

  if(i < 10){
    i_string <- paste0("00", i)
  } else if (i < 100){
    i_string <- paste0("0", i)
  } else {
    i_string <- i
  }

  # File path to save plot
  file_path = paste0(exportdir, "0026b_sp500_global_max/sp500-global-maxima-", i_string, ".jpeg")

  # Plot the entire price series with local maxima
  plot <- ggplot(to_plot, aes(x = date, y = price_final)) +
    geom_line() +
    geom_point(data=filter(to_plot, global_max == 1), col="red", alpha = 0.5, size = 1) +
    scale_y_continuous(label = dollar, limits = c(0.5, 20000), trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 10000, 20000)) +
    scale_x_date(limits = c(as.Date("1871-01-01"), as.Date("2018-12-01"))) +
    of_dollars_and_data_theme +
    labs(x = "Year", y = "Real Price w/ Dividends (Log Scale)") +
    ggtitle("The Fits and Starts of the U.S. Stock Market")

  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  A red point represents an all-time high in the U.S. stock market.")

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

create_gif(out_path, "sp500-global-maxima-*.jpeg", 20, out_name = "gif_global_maxima.gif")

# Section to plot the number of months between global maxima
global_max_vec <- sp500_ret_pe$global_max
max_dist_vec   <- c()
counter        <- 1

# Loop through global max to get the counts between peaks
for (j in 1:length(global_max_vec)){
  if (global_max_vec[j] == 1){
    max_dist_vec <- c(max_dist_vec, counter)
    counter      <- 1
  } else if (global_max_vec[j] == 0){
    counter      <- counter + 1
  }
}

# Print summary stats about the max distance vector
print(paste0("Max between peaks:", max(max_dist_vec)))
print(paste0("Avg between peaks:", mean(max_dist_vec)))
print(paste0("Median between peaks:", median(max_dist_vec)))

# Get percentages of 1 month peaks over all peaks
table(max_dist_vec[which(max_dist_vec < 5)])/length(max_dist_vec)

# Re-run the max distance vector for the number of months between peaks plot
global_max_vec <- sp500_ret_pe$global_max
max_dist_vec   <- c()
counter        <- 1

for (j in 1:length(global_max_vec)){
  if (global_max_vec[j] == 1){
    counter      <- 1
    max_dist_vec <- c(max_dist_vec, counter)
  } else if (global_max_vec[j] == 0){
    counter      <- counter + 1
    max_dist_vec <- c(max_dist_vec, counter)
  }
}

# Create a data frame for the max_dist_vec
df_months <- data.frame(x = sp500_ret_pe$date, y = max_dist_vec) 

# File path to save plot
file_path = paste0(exportdir, "0026a_market_timing_tests/sp500-peaks-global-maxima.jpeg")

plot <- ggplot(df_months, aes(x = x)) +
  geom_area(data = df_months, aes(y = y), fill = "blue", stat = "identity") +
  of_dollars_and_data_theme +
  labs(x = "Date", y = "Number of Months From Previous Peak") +
  ggtitle("Most Market Peaks Occur\nIn Consecutive Months")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Shows the number of months between peaks for the S&P 500 real return with dividends.")

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