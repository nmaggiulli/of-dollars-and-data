cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

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

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                    filter(cape != "NA", Date < 2017.01)

first_year <- floor(min(sp500_ret_pe$Date))
last_year <- floor(max(sp500_ret_pe$Date))

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "global_max"]      <- 1
    global_max                         <- sp500_ret_pe[i, "price_plus_div"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
    if (sp500_ret_pe[i, "price_plus_div"] > global_max){
      sp500_ret_pe[i, "global_max"]      <- 1
      global_max                         <- sp500_ret_pe[i, "price_plus_div"]
    } else{
      sp500_ret_pe[i, "global_max"]      <- 0
    }
  }
}

# Convert the cape to a numeric
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)

# Function for setting up breaks on y axis for the plot
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

orig_price <- as.numeric(sp500_ret_pe[1, "price_plus_div"])

years <- seq(first_year, last_year, 1)

for (i in 2:length(years)){
  to_plot <- sp500_ret_pe %>%
                filter(Date <= years[i]) %>%
                mutate(price_final = price_plus_div / orig_price)

  if(i < 10){
    i_string <- paste0("00", i)
  } else if (i < 100){
    i_string <- paste0("0", i)
  } else {
    i_string <- i
  }

  # File path to save plot
  file_path = paste0(exportdir, "26b-sp500-global-max/sp500-global-maxima-", i_string, ".jpeg")

  # Plot the entire price series with local maxima
  plot <- ggplot(to_plot, aes(x = Date, y = price_final)) +
    geom_line() +
    geom_point(data=filter(to_plot, global_max == 1), col="red") +
    scale_y_continuous(label = dollar, limits = c(0.5, 5000), trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 5000)) +
    scale_x_continuous(limits = c(1880, 2020), breaks = seq(1880, 2020, 20)) +
    of_dollars_and_data_theme +
    labs(x = "Year", y = "Real Price w/ Dividends (Log Scale)") +
    ggtitle("The Fits and Starts of the S&P 500")

  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Red points represent the highest value of the S&P 500 as of that point in time.")

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

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 10 loop -0 *.jpeg all_sp500_plots.gif

# Section to plot the number of months between global maxima
global_max_vec <- sp500_ret_pe$global_max
max_dist_vec    <- c()
counter <- 1

for (j in 2:length(global_max_vec)){
  if (global_max_vec[j] == 1){
    max_dist_vec <- c(max_dist_vec, counter)
    counter      <- 1
  } else if (global_max_vec[j] == 0){
    counter <- counter + 1
  }
}

print(paste0("Max between peaks:", max(max_dist_vec)))
print(paste0("Avg between peaks:", mean(max_dist_vec)))
print(paste0("Median between peaks:", median(max_dist_vec)))

# File path to save plot
file_path = paste0(exportdir, "26c-sp500-global-max/sp500-dist-to-global-max.jpeg")

ggplot(as.data.frame(max_dist_vec), aes(x=seq(1, length(max_dist_vec), 1), y = max_dist_vec)) +
  geom_bar(stat = "identity") +
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Red points represent the highest value of the S&P 500 as of that point in time.")

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