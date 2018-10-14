cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(magick)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# This seed allows us to have reproducible random sampling
set.seed(12345)    

#Setup initial values for simulation
n_draws <- 50
mean    <- 5
sd      <- 1.5

# Sample data
q <- data.frame(x = rnorm(n_draws, mean, sd))

# Loop through all draws
for (i in 1:n_draws){

  # For naming the file
  if (i < 10){
    i_string <- paste0("0", i)
  } else {
    i_string <- i
  }
  # Set the file_path for the next output
  file_path = paste0(exportdir, "0020_positive_lottery/draw_", i_string,".jpeg")
  
  # Grab data in increasing increments and only color the last observation
  to_plot <- data.frame(x = q[1:i,])
  to_plot$col <- c(rep(0, i-1), 1)
  
  # Plot the distribution with the color on the random row
  plot <- ggplot(data = to_plot, aes(x = x, y = 0.01, color = as.factor(col))) +
            geom_point(alpha = 0.5, size = 7) +
            stat_function(fun=dnorm, args=list(mean=mean, sd=sd), color = "black") +
            scale_color_manual(values =  c("gray", "red"), guide = FALSE) +
            scale_x_continuous(limits = c(0, 11), breaks = seq(1, 9, 2)) +
            scale_y_continuous(limits = c(0, 0.3)) +
            ggtitle(paste0("With Enough Trials\nYou Can Produce Great Work")) +
            of_dollars_and_data_theme +
            labs(x = "Quality" , y = "Frequency")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source: Simulated distribution (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes a normal distribution of quality.") 
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 20 loop -0 *.jpeg all_draws.gif
#
# 


# ############################  End  ################################## #