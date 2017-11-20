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
library(ggjoy)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Create fake data for Dunning-Kruger effect

low <- 38
y <- c(seq(0, 100, 25), seq(99, low), seq(low + 0.5, 75, 0.5))  
x <- seq(0, length(y)-1)

to_plot <- data.frame(x = x, y = y)

# Set the file_path for the next output
file_path = paste0(exportdir, "46-a-little-knowledge-is-dangerous/dunning-krueger.jpeg")

plot <- ggplot(data = to_plot, aes(x = x, y = y)) +
  geom_line(col= "red") +
  geom_point(data=filter(to_plot, x == 0), col='black') +
  geom_point(data=filter(to_plot, y == max(y)), col='black') +
  geom_point(data=filter(to_plot, y == low), col='black') +
  geom_point(data=filter(to_plot, x == max(x)), col='black') +
  scale_color_discrete(guide = FALSE) +
  ggtitle(paste0("The Dunning-Kruger Effect for Investors")) +
  of_dollars_and_data_theme +
  theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_text_repel(data = filter(to_plot, x == 0), 
                  aes(x = x, 
                      y = y, 
                      label = "401k? I've never run that far before.",
                      family = "my_font"
                  ), col = 'black',
                  nudge_y = 2,
                  nudge_x = 42,
                  max.iter = 4000,
                  segment.color = 'transparent') +
  geom_text_repel(data = filter(to_plot, y == max(y)), 
                  aes(x = x, 
                      y = y, 
                      label = "I can be the next Warren Buffett",
                      family = "my_font"
                  ), col = 'black',
                  nudge_x = 42,
                  nudge_y = 0,
                  max.iter = 4000,
                  segment.color = 'transparent') +
  geom_text_repel(data = filter(to_plot, y == low), 
                  aes(x = x, 
                      y = y, 
                      label = "Low Fees + Diversification.\nGod, I hope this works.",
                      family = "my_font"
                  ), col = 'black',
                  nudge_y = -8,
                  max.iter = 4000,
                  segment.color = 'transparent') +
  geom_text_repel(data = filter(to_plot, x == max(x)), 
                  aes(x = x, 
                      y = y, 
                      label = "The actual\nWarren Buffett",
                      family = "my_font"
                  ), col = 'black',
                  nudge_y = 8,
                  max.iter = 4000,
                  segment.color = 'transparent') +
  labs(x = "Experience" , 
       y = "Confidence")

# Add a source and note string for the plots
source_string <- paste0("Source: Simulated data (OfDollarsAndData.com)")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)

# Save the gtable
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")





# ############################  End  ################################## #