cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "0111_sp500_watermarks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_watermarks <- function(start_date, end_date, select_vars, file_name_string){
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)

  # Filter by dates
  sp500_ret_pe  <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                      filter(date >= start_date, date <= end_date)
  
  # Create index that starts at 1
  sp500_ret_pe <-  sp500_ret_pe %>%
                    mutate(index = price_plus_div/pull(sp500_ret_pe[1, "price_plus_div"])) %>%
                      select(date, index)
  
  # Find high watermark
  absolute_maximum <- 0
  
  for(i in 1:nrow(sp500_ret_pe)){
    current_p <- pull(sp500_ret_pe[i, "index"])
    if (current_p > absolute_maximum){
      sp500_ret_pe[i, "high_watermark"] <- current_p
      absolute_maximum <- current_p
    } else{
      sp500_ret_pe[i, "high_watermark"] <- absolute_maximum
    }
  }
  
  # Sort the data in descending order
  sp500_ret_pe <- arrange(sp500_ret_pe, desc(date))
  
  # Find low matermark
  absolute_minumum <- 10^8
  
  for(i in 1:nrow(sp500_ret_pe)){
    current_p <- pull(sp500_ret_pe[i, "index"])
    if (current_p < absolute_minumum){
      sp500_ret_pe[i, "low_watermark"] <- current_p
      absolute_minumum <- current_p
    } else{
      sp500_ret_pe[i, "low_watermark"] <- absolute_minumum
    }
  }
  
  # File path and source/note strings
  file_path <- paste0(out_path, "/", file_name_string, "_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
  note_string <- paste0("Note:  Real return includes reinvested dividends.") 
  
  to_plot <- sp500_ret_pe %>%
              select_("date", .dots = select_vars) %>%
              gather(-date, key=key, value=value)
  
  # Define the colors based on the length of "select_vars"
  if(length(select_vars) <= 1){
    my_colors <- "black"
  } else{
    my_colors <- c("green", "red", "black")
  }
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar, trans = log10_trans()) +
    scale_color_manual(guide = FALSE, values = my_colors) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Progress of the S&P 500\n", 
                   format.Date(start_date, "%Y"), 
                   "-", 
                   format.Date(end_date, "%Y"))) +
    labs(x="Date", y="Real Growth of $1",
         caption = paste0(source_string, "\n", note_string))
      
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_watermarks("1871-01-01", "2018-12-01", c("index"),
                      "banded_01_sp500")

plot_watermarks("1871-01-01", "2018-12-01", c("high_watermark", "low_watermark", "index"),
                      "banded_02_sp500")

plot_watermarks("1871-01-01", "2018-12-01", c("high_watermark", "low_watermark"),
                      "banded_03_sp500")



# ############################  End  ################################## #