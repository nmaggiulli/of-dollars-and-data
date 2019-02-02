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

folder_name <- "xxxx_sp500_watermarks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

color_map <- data.frame(var = c("high_watermark", "low_watermark", "price_plus_div"),
                        color = c("green", "red", "black"))

plot_watermarks <- function(start_date, end_date, select_vars, file_name_string){
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)

  # Filter by dates
  sp500_ret_pe  <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                      filter(date >= start_date, date <= end_date)
  
  # Start price_plus_div at 1
  sp500_ret_pe <-  sp500_ret_pe %>%
                    mutate(price_plus_div = price_plus_div/pull(sp500_ret_pe[1, "price_plus_div"])) %>%
                      select(date, price_plus_div) %>%
                      arrange(desc(date))
  
  # Find low matermark
  absolute_minumum <- 10^8
  
  for(i in 1:nrow(sp500_ret_pe)){
    current_p <- pull(sp500_ret_pe[i, "price_plus_div"])
    if (current_p < absolute_minumum){
      sp500_ret_pe[i, "low_watermark"] <- current_p
      absolute_minumum <- current_p
    } else{
      sp500_ret_pe[i, "low_watermark"] <- absolute_minumum
    }
    
    if (i > 1){
      low_wm     <- sp500_ret_pe[i, "low_watermark"]
      prior_low  <- sp500_ret_pe[(i-1), "low_watermark"]
      
      if(low_wm == prior_low){
        sp500_ret_pe[i, "low_wm_length"] <- sp500_ret_pe[(i-1), "low_wm_length"] + 1
      } else{
        sp500_ret_pe[i, "low_wm_length"] <- 1
      }
    } else{
      sp500_ret_pe[i, "low_wm_length"]  <- 1
    }
  }
  
  # Re-sort data
  sp500_ret_pe <- sp500_ret_pe %>% arrange(date)
  
  # Find high watermark and add length of watermarks
  absolute_maximum <- 0
  
  for(i in 1:nrow(sp500_ret_pe)){
    current_p <- pull(sp500_ret_pe[i, "price_plus_div"])
    if (current_p > absolute_maximum){
      sp500_ret_pe[i, "high_watermark"] <- current_p
      absolute_maximum <- current_p
    } else{
      sp500_ret_pe[i, "high_watermark"] <- absolute_maximum
    }

    if (i > 1){
      high_wm    <- sp500_ret_pe[i, "high_watermark"]
      prior_high <- sp500_ret_pe[(i-1), "high_watermark"]
      
      if(high_wm == prior_high){
        sp500_ret_pe[i, "high_wm_length"] <- sp500_ret_pe[(i-1), "high_wm_length"] + 1
      } else{
        sp500_ret_pe[i, "high_wm_length"] <- 1
      }

    } else{
      sp500_ret_pe[i, "high_wm_length"] <- 1
    }
  }
  
  assign("sp500_ret_pe", sp500_ret_pe, envir = .GlobalEnv)
  
  # File path and source/note strings
  file_path <- paste0(out_path, "/", start_date_string, "_", end_date_string, "_", file_name_string, ".jpeg")
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
  note_string <- paste0("Note:  Real return includes reinvested dividends.") 
  
  to_plot <- sp500_ret_pe %>%
              select_("date", .dots = select_vars) %>%
              gather(-date, key=key, value=value)
  
  # Define the colors based on the select_vars
  my_colors <- c()
  
  # Sort the select vars
  select_vars_sorted <- sort(select_vars)
  
  for(i in 1:length(select_vars_sorted)){
    my_var       <- select_vars_sorted[i]
    my_colors[i] <- filter(color_map, var == my_var) %>% pull(color)
  }
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar, trans = log10_trans()) +
    scale_color_manual(guide = FALSE, values = my_colors) +
    of_dollars_and_data_theme +
    ggtitle(paste0("The Watermarks of the S&P 500\n", 
                   format.Date(start_date, "%Y"), 
                   "-", 
                   format.Date(end_date, "%Y"))) +
    labs(x="Date", y="Real Growth of $1 (Log Scale)",
         caption = paste0(source_string, "\n", note_string))
      
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Print watermark lengths
  to_plot <- sp500_ret_pe %>%
              select(date, low_wm_length) %>%
              gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/low_watermark_duration_", start_date_string, "_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, fill = key)) +
    geom_area(alpha = 0.5) +
    scale_fill_manual(values = c("red"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Number of Months Until Bottom\n", 
                   format.Date(start_date, "%Y"), 
                   "-", 
                   format.Date(end_date, "%Y"))) +
    labs(x="Date", y="Number of Months",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_all <- function(start_date, end_date){
  plot_watermarks(start_date, end_date, c("high_watermark", "price_plus_div"), 
                  "banded_01_sp500")
  # plot_watermarks(start_date, end_date, c("high_watermark", "low_watermark", "price_plus_div"),
  #                 "banded_02_sp500")
  # plot_watermarks(start_date, end_date, c("high_watermark", "low_watermark"),
  #                 "banded_03_sp500")
  # start_date_string <- date_to_string(start_date)
  # create_gif(path = out_path, 
  #            file_stub = paste0(start_date_string, "*"), 
  #            speed_milliseconds = 120, 
  #            out_name = paste0("_gif_", start_date_string, ".gif")
  #)
}

plot_all("1871-01-01", "2018-12-01")
plot_all("1990-01-01", "2018-12-01")





# ############################  End  ################################## #