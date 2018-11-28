cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
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
library(quadprog)
library(lubridate)
library(fTrading)
library(quantmod)
library(tidyr)
library(ggjoy)
library(tidyverse)

folder_name <- "0049_sp_index_pe_heatmap"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    mutate(year = year(date)) %>%
                     filter(!is.na(cape))

#Create function for S&P data
plot_sp <- function(from_year, to_year, lag_cape, title){
  
  to_plot <- sp500_ret_pe %>%
                mutate(final_date = lead(date) - 1,
                       cape = lag(cape, n = lag_cape)) %>%
                filter(year >= from_year, year <= to_year)
  
  for (i in 1:nrow(to_plot)){
    if (i == 1){
      to_plot[i, "index"] <- 100
    } else {
      to_plot[i, "index"] <- to_plot[(i-1), "index"] * (1 + (to_plot[(i), "price_plus_div"]/to_plot[(i-1), "price_plus_div"] - 1))
    }
  }
  
  file_path <- paste0(exportdir, "0049_sp_index_pe_heatmap/sp-price-",from_year, "-", to_year, "-lag-", lag_cape ,".jpeg")
  
  y_max <- max(to_plot$index)
  
  plot <- ggplot(data = to_plot, aes(x=date, y=index)) +
    geom_rect(data=to_plot, aes(xmin = date, ymin = 50, 
                                 xmax = final_date, ymax = y_max, fill = cape)) +
    geom_line() +
    scale_fill_gradient(limits = c(4, 44), low="blue", high="red", name = "CAPE\nRange") +
    of_dollars_and_data_theme +
    scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
    scale_x_date(date_labels = "%Y", breaks = seq(as.Date("1900-01-01"), as.Date("2000-12-31"), by="20 years")) +
    ggtitle(paste0(title)) +
    labs(x = "Date", y = "Real Index (Start = $100)")
  
  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Real index includes reinvested dividends.") 
  
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

plot_sp(1900, 2017, 0, "CAPE May Only Be Useful Near Its Extremes")