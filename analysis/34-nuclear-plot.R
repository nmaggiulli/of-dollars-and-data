cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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

########################## Start Program Here ######################### #

years    <- seq(1945, 2016, 1)
n_events <- c(2, rep(0, length(years)-1))

to_plot <- data.frame(year = years, value = n_events)

# Set the file_path based on the function input 
file_path = paste0(exportdir, "34-cooperation/nuclear-events.jpeg")

# Create title with ticker in subtitle
top_title <- paste0("Number of Nuclear Bombs Dropped\non Civilian Locations\n1945 - 2016")

# Create the plot object
plot <- ggplot(to_plot, aes(x = year, y = value)) +
  geom_line() +
  ggtitle(top_title) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 1)) +
  scale_x_continuous(limits = c(1945, 2017), breaks = seq(1945, 2015, 10)) +
  labs(x = "Year", y = "Number of Events")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Historical data (OfDollarsAndData.com)"

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 

# ############################  End  ################################## #