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
library(stats)
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

# Set seed
set.seed(12345)

# Choose a level of n for the nxn grid
n <- 20
  
# Create fake data
to_plot   <- expand.grid(x = 1:n, y = 1:n)
to_plot$z <- sample(0:1, n^2, replace = TRUE)

# Set output path
file_path <- paste0(exportdir, "54-coin-flip-simulation/coin-flip-grid-", n, ".jpeg")

plot <- ggplot(to_plot, aes(x = x, y = y, fill = z)) +
          geom_raster() +
          scale_fill_continuous(low = "red", high = "black", guide = FALSE) +
          scale_x_continuous(limits = c(0, n)) +
          scale_y_continuous(limits = c(0, n)) +
          ggtitle(paste0("Do You See Patterns in the Noise?"))  +
          of_dollars_and_data_theme +
          theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()
            ) + 
          labs(x = "x" , y = "y")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Simulates ", formatC(as.numeric(n^2), format="f", digits=0, big.mark=","), " coin flips plotted on a ", n ,"x", n, " grid. Red = tails, Black = heads.")

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