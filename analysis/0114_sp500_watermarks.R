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

folder_name <- "0114_sp500_watermarks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

color_map <- data.frame(var = c("high_watermark", "low_watermark", "index"),
                        color = c("green", "red", "black"))

#Bring in all data
sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  rename(index = price_plus_div) %>%
                  select(date, index)

jpy <- read.csv(paste0(importdir, "0114_japan_nikkei/nikk.csv"), skip = 1) %>%
  rename(date = Date,
         index = Close) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  as.tibble() %>%
  select(date, index) 

plot_watermarks <- function(df, data_name, start_date, end_date, select_vars, file_name_string){
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  if(data_name == "sp500"){
    title_string <- "S&P 500"
  } else if (data_name == "jpy"){
    title_string <- "Nikkei"
  }

  # Filter by dates
  ret  <- df %>%
            filter(date >= start_date, date <= end_date)
  
  # Start index at 1
  ret <-  ret %>%
            mutate(index = index/pull(ret[1, "index"])) %>%
            select(date, index) %>%
            arrange(desc(date))
  
  # Find low matermark
  absolute_minumum <- 10^8
  
  for(i in 1:nrow(ret)){
    current_p <- pull(ret[i, "index"])
    if (current_p < absolute_minumum){
      ret[i, "low_watermark"] <- current_p
      absolute_minumum <- current_p
    } else{
      ret[i, "low_watermark"] <- absolute_minumum
    }
    
    if (i > 1){
      low_wm     <- ret[i, "low_watermark"]
      prior_low  <- ret[(i-1), "low_watermark"]
      
      if(low_wm == prior_low){
        ret[i, "low_wm_length"] <- ret[(i-1), "low_wm_length"] + 1
      } else{
        ret[i, "low_wm_length"] <- 1
      }
    } else{
      ret[i, "low_wm_length"]  <- 1
    }
  }
  
  # Re-sort data
  ret <- ret %>% arrange(date)
  
  # Find high watermark and add length of watermarks
  absolute_maximum <- 0
  
  for(i in 1:nrow(ret)){
    current_p <- pull(ret[i, "index"])
    if (current_p > absolute_maximum){
      ret[i, "high_watermark"] <- current_p
      absolute_maximum <- current_p
    } else{
      ret[i, "high_watermark"] <- absolute_maximum
    }

    if (i > 1){
      high_wm    <- ret[i, "high_watermark"]
      prior_high <- ret[(i-1), "high_watermark"]
      
      if(high_wm == prior_high){
        ret[i, "high_wm_length"] <- ret[(i-1), "high_wm_length"] + 1
      } else{
        ret[i, "high_wm_length"] <- 1
      }

    } else{
      ret[i, "high_wm_length"] <- 1
    }
  }
  
  assign("ret", ret, envir = .GlobalEnv)
  
  # File path and source/note strings
  file_path <- paste0(out_path, "/", data_name, "_", start_date_string, "_", end_date_string, "_", file_name_string, ".jpeg")
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
  note_string <- paste0("Note:  Real return includes reinvested dividends.") 
  
  to_plot <- ret %>%
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
    ggtitle(paste0("High and Low Watermarks of the ", title_string, "\n", 
                   format.Date(start_date, "%Y"), 
                   "-", 
                   format.Date(end_date, "%Y"))) +
    labs(x="Date", y="Real Growth of $1 (Log Scale)",
         caption = paste0(source_string, "\n", note_string))
      
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Print watermark lengths
  to_plot <- ret %>%
              select(date, low_wm_length) %>%
              gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/", data_name, "_low_watermark_duration_", start_date_string, "_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, fill = key)) +
    geom_area(alpha = 0.5) +
    scale_fill_manual(values = c("red"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Number of Months Until Bottom for the ", title_string, "\n", 
                   format.Date(start_date, "%Y"), 
                   "-", 
                   format.Date(end_date, "%Y"))) +
    labs(x="Date", y="Number of Months",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_all <- function(df, data_name, start_date, end_date){
  
  plot_watermarks(df, data_name, start_date, end_date, c("high_watermark", "index"), 
                  "banded_01")
  plot_watermarks(df, data_name, start_date, end_date, c("high_watermark", "low_watermark", "index"),
                  "banded_02")
  plot_watermarks(df, data_name, start_date, end_date, c("high_watermark", "low_watermark"),
                  "banded_03")
  start_date_string <- date_to_string(start_date)
  create_gif(path = out_path,
             file_stub = paste0(data_name, "_", start_date_string, "*"),
             speed_milliseconds = 120,
             out_name = paste0("_gif_", data_name, "_", start_date_string, ".gif")
  )
}

plot_all(sp500, "sp500", "1871-01-01", "2018-12-01")
plot_all(sp500, "sp500", "1990-01-01", "2018-12-01")
plot_all(jpy, "jpy", "1970-01-01", "2018-11-01")

# ############################  End  ################################## #