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

########################## Start Program Here ######################### #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "0006_bv_returns.Rds"))

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year, "%d/%m/%y")

plot_bars <- function(start_year, end_year){
  
  to_plot <- filter(full_bv_returns, year(year) %in% seq(start_year, end_year, 1)) %>%
                gather(key=key, value=value, -year)
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "0060_the_abnormal_environment/annual-bars-", start_year, "-", end_year,".jpeg")
  
  # Set note and source string
  source_string <- str_wrap("Source: BullionVault U.S. Asset Class Performance Data (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note:   Returns are adjusted using the U.S. Consumer Price Index."
                                   ),
                            width = 85)
  
  plot <- ggplot(to_plot, aes(x= year, y = value, fill=key)) +
      geom_bar(stat="identity", position="dodge") +
      scale_y_continuous(label = percent) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      of_dollars_and_data_theme +
      ggtitle("Risky Assets Tend to Behave Differently\nDuring Abnormal Environments") +
      labs(x = "Year", y = "1-Year Real Return",
           caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_bars(2006, 2011)

# ############################  End  ################################## #

  
