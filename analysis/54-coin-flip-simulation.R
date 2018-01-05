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
library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

# Create fake data
to_plot <- data.frame(flip=c(seq(1, 10), seq(1, 10)),
                sum = c(seq(1, 10), c(0, 1, 1, 1, 2, 2, 3, 4, 4, 5)),
                sim = c(rep(1, 10), rep(2, 10))
                )

# Set output path
file_path <- paste0(exportdir, "54-coin-flip-simulation/coin-flip-simulation.jpeg")

plot <- ggplot(to_plot, aes(x = flip, y = sum, col = as.factor(sim))) +
          geom_line() +
          scale_color_discrete(guide = FALSE) +
          scale_x_continuous(breaks = seq(1, 10)) +
          scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
          ggtitle(paste0("Equally Random Coin Flip Sequences"))  +
          of_dollars_and_data_theme +
          labs(x = "Flip" , y = "Cumulative Heads")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Shows two simulations of flipping a fair coin 10 times.")

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