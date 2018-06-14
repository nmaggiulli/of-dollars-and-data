cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(stats)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

spy <- readRDS(paste0(localdir, "77_spy_volatility.Rds"))

plot_avg_return <- function(volatility_bucket, num){
  
  # Create data to plot
  to_plot <- spy %>%
    filter(vol_bucket == volatility_bucket) %>%
    group_by(ret_pos) %>%
    summarize(`25th` = quantile(ret_spy, p = 0.25),
              `50th` = quantile(ret_spy, p = 0.5),
              `75th` = quantile(ret_spy, p = 0.75)) %>%
    ungroup() %>%
    gather(key=key, value=value, -ret_pos)
  
  print(head(to_plot))
  
  if (num < 10){
    num_string <- paste0("0", num)
  } else {
    num_string <- paste0(num)
  }
  
  file_path <- paste0(exportdir, "77-spy-volatility/vol-bucket-", num_string, "-", volatility_bucket ,".jpeg")
  
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source: YCharts.com (OfDollarsAndData.com)"),
                            width = 85)
  
  note_string <- str_wrap(paste0("Note: Shows daily returns for a given range of 30-day rolling volatility."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x= key, y = value, fill = ret_pos)) +
    geom_bar(stat="identity") +
    facet_grid(~ret_pos) +
    scale_fill_manual(guide = FALSE, values = c("red", "green")) +
    scale_y_continuous(label = percent, limits = c(-0.06, 0.06)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500 Daily Returns When Volatility is ", volatility_bucket)) +
    labs(x = "Percentile", y = "Daily Return",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

vol_buckets <- c("<=8", "8-16", "16-24", "24-32", "32-40", "40-48", ">=48", ">=48")

for (i in 1:length(vol_buckets)){
  plot_avg_return(vol_buckets[i], i)
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# convert -delay 80 loop -0 vol-*.jpeg dist_plots.gif

to_plot <- spy %>%
            arrange(vol_spy)

source_string <- str_wrap(paste0("Source: YCharts.com (OfDollarsAndData.com)"),
                          width = 85)

note_string <- str_wrap(paste0("Note: Shows daily returns sorted by 30-day volatility."),
                        width = 85)

file_path <- paste0(exportdir, "77-spy-volatility/ret-and-vol.jpeg")

plot <- ggplot(spy, aes(x=vol_spy, y=ret_spy)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype='dashed') +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Daily And 30-Day Volatility")) +
  labs(x = "Volatility", y = "Daily Return",
       caption = paste0("\n", source_string, "\n", note_string))

# Save plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #