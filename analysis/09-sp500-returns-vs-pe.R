cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

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

# Load in S&P data from Shiller

sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, cape != "NA", Date < 2017.01 & Date > 1909.01)

# Convert the cape to a numeric
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)

# Setup a vector of different returns to calculate
# This will be for 5 year, 10 year, 20 year, and 30 year returns
returns_to_calc <- c(5, 10, 20, 30)

# Calculate returns over different time frames
for (i in 1:nrow(sp500_ret_pe)){
  for (x in returns_to_calc){
    if (i + (x * 12) > nrow(sp500_ret_pe)){
      break
    } else{
      name <- paste0("ret_", x)
      sp500_ret_pe[i, name] <- (sp500_ret_pe[(i + (x * 12)), "price_plus_div"]/sp500_ret_pe[(i), "price_plus_div"])^(1/(x)) - 1
    }
  }
}

plot_ret_pe <- function(var){
  yvar        <- paste0("ret_", var)
  filter_line <- paste0("!is.na(",yvar,")")
  to_plot     <- filter_(sp500_ret_pe, filter_line)
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "09-sp500-returns-pe/returns-", var,"-year.jpeg")
  
  plot <- ggplot(data = to_plot, aes_string(x = "cape", y = yvar)) +
    geom_point() +
    geom_smooth() +
    scale_y_continuous(label = percent, limits = c(-0.1, 0.3)) +
    ggtitle(paste0("S&P P/E Ratio vs. ", var, " Year  \nCompound Growth Rate")) +
    of_dollars_and_data_theme +
    labs(x = "S&P 500 P/E Ratio" , y = "S&P 500 Compound Annual Growth Rate (%)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  Yahoo Finance, Irrational Exhuberence by Robert Shiller (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Growth rate shown is the growth of real returns with reinvested dividends.") 
  
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

for (x in returns_to_calc){
  plot_ret_pe(x)
}


# ############################  End  ################################## #