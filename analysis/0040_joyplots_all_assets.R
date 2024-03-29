cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(ggjoy)
library(dplyr)

folder_name <- "0040a_joyplot_all_assets"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "0006_bv_returns.Rds"))

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year, "%d/%m/%y")

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

yrs_seq <- c(1, 5, 10, 15, 20)

# Loop over the years 1-20 and 21 which is a repeat loop
for (yr in yrs_seq){
  
  to_plot <- gather(full_bv_returns, key=key, value=value, -year)
  
  if (yr < 10){
    file_path <- paste0(exportdir, "0040a_joyplot_all_assets/0", yr, "-joyplot.jpeg")
  } else if (yr < 21){
    file_path <- paste0(exportdir, "0040a_joyplot_all_assets/", yr, "-joyplot.jpeg")
  } else {
    file_path <- paste0(exportdir, "0040b_joyplot_20yr_only/20-joyplot.jpeg")
  }
  
  # Set the fixed_axis for most of the plots
  fixed_axis <- 1
  
  # Set year to 20 for the 21st loop
  if (yr == 21){
    yr <- 20
    fixed_axis <- 0
  }
  
  # Calculate the annualized returns
  for (i in yr:nrow(to_plot)){
    if (to_plot[(i-yr+1), "key"] == to_plot[i, "key"]){
      start_index <- i - yr + 1
      to_plot[i, "ret"] <- prod(1 + to_plot[(start_index:i), "value"])^(1/yr) - 1
    } else {
      to_plot[i, "ret"] <- NA
    }
  }
  
  if (fixed_axis == 1){
    plot <- ggplot(data = to_plot, aes(x=ret, y=factor(key), fill = factor(key))) +
      geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
      scale_fill_discrete(guide = FALSE) +
      scale_x_continuous(label = percent, limit = c(-0.5, 0.75), breaks = seq(-0.5, 0.75, 0.25)) +
      of_dollars_and_data_theme +
      ggtitle(paste0(yr, "-Year Annualized Returns by Asset Class")) +
      labs(x = paste0(yr, "-Year Annualized Real Return (%)" ), y = "Asset Class")
  } else {
    plot <- ggplot(data = to_plot, aes(x=ret, y=factor(key), fill = factor(key))) +
      geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
      scale_fill_discrete(guide = FALSE) +
      scale_x_continuous(label = percent) +
      of_dollars_and_data_theme +
      ggtitle(paste0(yr, "-Year Annualized Returns by Asset Class")) +
      labs(x = paste0(yr, "-Year Annualized Real Return (%)" ), y = "Asset Class")
  }
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 
  
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

create_gif(path = paste0(out_path),
           file_stub = "*-joyplot.jpeg",
           speed_milliseconds = 120,
           out_name = paste0("/_gif_asset_class_joyplots_reduced.gif"))

# ############################  End  ################################## #

  
