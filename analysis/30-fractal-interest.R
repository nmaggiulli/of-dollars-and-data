cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

starting_value <- 100
interest_rate  <- 0.5

principal <- data.frame(year = seq(0,3), marker=-4, 
                        lower = 0, 
                        middle= 0, 
                        ymin=0, 
                        ymax=0, 
                        upper = rep(starting_value, 4),
                        color=rep(0, 4)
                        )
interest1 <- data.frame(year = seq(1,3), marker= c(-3, -2, 0), 
                        lower = rep(starting_value, 3), 
                        middle=  rep(starting_value, 3), 
                        ymin= rep(starting_value, 3), 
                        ymax= rep(starting_value*(1+interest_rate), 3),
                        upper = rep(starting_value*(1+interest_rate), 3),
                        color=rep(1, 3)
                        )
interest2 <- data.frame(year = c(2,3,3), marker= c(-1, 1, 2), 
                        lower = rep(starting_value*(1+interest_rate), 3), 
                        middle=  rep(starting_value*(1+interest_rate), 3), 
                        ymin= rep(starting_value*(1+interest_rate), 3), 
                        ymax= rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2), 3),
                        upper = rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2), 3),
                        color=rep(2, 3)
                        )
interest3 <- data.frame(year = 3, marker= c(3), 
                        lower = rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2), 1), 
                        middle=  rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2), 1), 
                        ymin= rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2), 1), 
                        ymax= rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2)+(starting_value*interest_rate^3), 1),
                        upper = rep(starting_value*(1+interest_rate)+(starting_value*interest_rate^2)+(starting_value*interest_rate^3), 1),
                        color=rep(3, 1)
                        )

all_data <- bind_rows(principal, interest1, interest2, interest3)

plot_sequence <- c(0, 1, 2, 3, 3)

for (i in 1:length(plot_sequence)){

  to_plot <- filter(all_data, year <= plot_sequence[i])

  # Set the file_path for the next output
  file_path = paste0(exportdir, "30-fractal-compounding/compounding-year-", i, ".jpeg")
  
if (i == 1){
  plot <- ggplot(to_plot, aes(x = marker, lower=lower, middle=middle, upper=upper, ymin=ymin, ymax=ymax, 
                              fill=as.factor(color), width=1)) +
      geom_boxplot(stat="identity") +
      of_dollars_and_data_theme +
      scale_x_continuous(limits = c(-5, 5)) +
      scale_y_continuous(limits = c(0, 200), breaks = seq(0,200, 20)) +
      scale_fill_manual(guide = FALSE, values=my_palette) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
    ggtitle(paste0("Compounding Becomes Fractal Over Time\nYear ", plot_sequence[i])) +
    labs(y = "Value ($)")
} else if (i == 2){
  plot <- ggplot(to_plot, aes(x = marker, lower=lower, middle=middle, upper=upper, ymin=ymin, ymax=ymax, 
                              fill=as.factor(color), width=1)) +
    geom_boxplot(stat="identity") +
    of_dollars_and_data_theme +
    scale_x_continuous(limits = c(-5, 5)) +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0,200, 20)) +
    scale_fill_manual(guide = FALSE, values=my_palette) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    ggtitle(paste0("Compounding Becomes Fractal Over Time\nYear ", plot_sequence[i])) +
    labs(y = "Value ($)") +
    annotate("segment", x = -4.5, xend = -3.7, y = 115, yend = 115, colour="black", size=1, arrow=arrow())
} else if (i == 3){
  plot <- ggplot(to_plot, aes(x = marker, lower=lower, middle=middle, upper=upper, ymin=ymin, ymax=ymax, 
                              fill=as.factor(color), width=1)) +
    geom_boxplot(stat="identity") +
    of_dollars_and_data_theme +
    scale_x_continuous(limits = c(-5, 5)) +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0,200, 20)) +
    scale_fill_manual(guide = FALSE, values=my_palette) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    ggtitle(paste0("Compounding Becomes Fractal Over Time\nYear ", plot_sequence[i])) +
    labs(y = "Value ($)") +
    annotate("segment", x = -4.5, xend = -3.7, y = 115, yend = 115, colour="black", size=1, arrow=arrow()) +
    annotate("segment", x = -2.5, xend = -1.7, y = 165, yend = 165, colour="black", size=1, arrow=arrow())
} else if (i >= 4){
  plot <- ggplot(to_plot, aes(x = marker, lower=lower, middle=middle, upper=upper, ymin=ymin, ymax=ymax, 
                              fill=as.factor(color), width=1)) +
    geom_boxplot(stat="identity") +
    of_dollars_and_data_theme +
    scale_x_continuous(limits = c(-5, 5)) +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0,200, 20)) +
    scale_fill_manual(guide = FALSE, values=my_palette) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    ggtitle(paste0("Compounding Becomes Fractal Over Time\nYear ", plot_sequence[i])) +
    labs(y = "Value ($)") +
    annotate("segment", x = -4.5, xend = -3.7, y = 115, yend = 115, colour="black", size=1, arrow=arrow()) +
    annotate("segment", x = -2.5, xend = -1.7, y = 165, yend = 165, colour="black", size=1, arrow=arrow()) +
    annotate("segment", x = -0.4, xend = 0.4, y = 165, yend = 165, colour="black", size=1, arrow=arrow()) +
    annotate("segment", x = 1.5, xend = 2.3, y = 186, yend = 186, colour="black", size=1, arrow=arrow()) 
} 

  
    # Add a source and note string for the plots
    source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
    note_string   <- paste0("Note:  Assumes starting principal of $", starting_value, " with an annual return of ", interest_rate*100, "%.") 
    
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
# magick convert -delay 110 loop -0 *.jpeg all_plots.gif


# ############################  End  ################################## #