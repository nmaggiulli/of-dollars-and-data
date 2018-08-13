cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(ggrepel)
library(tidyverse)

folder_name <- "85-speed-of-information"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_min_per_week <- 60*24*7

years <- c(1492, 1700, 1845, 1866, 1990)
times <- c(9*n_min_per_week, 6*n_min_per_week, 2*n_min_per_week, 5, 0.1)

to_plot <- data.frame(yr = years, tm = times)

# Set the file_path based on the function input 
file_path = paste0(out_path, "/london-to-nyc-information.jpeg")

# Strings for source and note
source_string <- str_wrap("Source:  Smaller Faster Lighter Denser Cheaper by Robert Bryce (OfDollarsAndData.com)",
                          width = 80)

plot <-  ggplot(to_plot, aes(x=yr, y=tm)) +
          geom_point() +
          geom_line() +
          geom_text_repel(aes(x=yr, y=tm), label = to_plot$yr, size = 3) +
          scale_y_log10(label = comma) +
          of_dollars_and_data_theme +
          theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
          ggtitle("Time To Send A Message\nFrom London to New York") +
          labs(x = "Year" , y = "Time in Minutes (Log Scale)",
            caption = paste0("\n", source_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #