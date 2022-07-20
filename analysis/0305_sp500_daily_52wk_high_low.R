cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0305_sp500_daily_52wk_high_low"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_data <- function(index_name){
  
  if(index_name == "sp500"){
    proper_name <- "S&P 500"
  } else if(index_name == "russell3000"){
    proper_name <- "Russell 3000"
  }
  
  raw <- read.csv(paste0(importdir, "/0305_ycharts_stocks_daily_2018/timeseries_7-15-2022_", index_name, ".csv"),
                   skip = 6) %>%
    select(-Metric, -Name) %>%
    rename(symbol = Symbol) %>%
    gather(-symbol, key=key, value=price) %>%
    mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
           month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
           day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
           date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
    select(date, symbol, price) %>%
    drop_na() %>%
    arrange(symbol, date, price)
  
  fnrollminmax <- function (x, min_max) {
    if (length(x) < 250) {
      rep(NA,length(x)) 
    } else {
      if(min_max == "max"){
        rollmax(x,250,align="right",na.pad=TRUE)
      } else if (min_max == "min"){
        -1*rollmax(-x,250,align="right",na.pad=TRUE)
      }
    }
  }
  
  df <- raw %>%
          group_by(symbol) %>%
          mutate(high_52wk = fnrollminmax(price, "max"),
                 low_52wk = fnrollminmax(price, "min")) %>%
          ungroup()
  
  highs <- df %>%
    filter(price == high_52wk) %>%
    select(symbol, date, high_52wk)
  
  lows <- df %>%
    filter(price == low_52wk) %>%
    select(symbol, date, low_52wk)
  
  highs_lows <- highs %>%
    bind_rows(lows) %>%
    arrange(symbol, date) %>%
    mutate(high_date = lag(date), 
           high = lag(high_52wk),
           n_days_to_low = date - high_date) 
  
  high_to_low <- highs_lows %>%
    filter(!is.na(low_52wk), !is.na(lag(high_52wk)), date > high_date) %>%
    select(-high_52wk) %>%
    rename(low_date = date,
           high_52wk = high) %>%
    select(symbol, high_date, high_52wk, low_date, low_52wk, n_days_to_low)
  
  lows_march_2020 <- high_to_low %>%
                      filter(low_date >= "2020-02-24", low_date <= "2020-03-23") %>%
                      arrange(low_date)
  
  lows_2022 <- high_to_low %>%
                filter(low_date >= "2022-01-18") %>%
                arrange(low_date)
  
  to_plot <- lows_march_2020 %>%
                group_by(low_date) %>%
                summarise(n_lows = n()) %>%
                ungroup()
  
  max_y_limit <- round_to_nearest(max(to_plot$n_lows), "up", 20)
  
  file_path <- paste0(out_path, "/", index_name, "_stocks_lows_march_2020.jpg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Shows number of stocks in the ", proper_name, " that hit 52-week lows. ",
                          "Does not include dividends and is not adjusted for inflation."), 
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=low_date, y=n_lows)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = comma, limits = c(0, max_y_limit)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("When Stocks Bottomed in Early 2020\n", proper_name)) +
    labs(x = "Date", y = "Number of Stocks",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Plot 2022
  to_plot <- lows_2022 %>%
    group_by(low_date) %>%
    summarise(n_lows = n()) %>%
    ungroup()
  
  file_path <- paste0(out_path, "/", index_name, "_stocks_lows_2022.jpg")
  
  plot <- ggplot(data = to_plot, aes(x=low_date, y=n_lows)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = comma, limits = c(0, max_y_limit)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("When Stocks Bottomed in 2022\n", proper_name)) +
    labs(x = "Date", y = "Number of Stocks",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Un-fix the scale
  file_path <- paste0(out_path, "/", index_name, "_stocks_lows_2022_dynamic_scale.jpg")
  
  plot <- ggplot(data = to_plot, aes(x=low_date, y=n_lows)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = comma) +
    of_dollars_and_data_theme +
    ggtitle(paste0("When Stocks Bottomed in 2022\n", proper_name)) +
    labs(x = "Date", y = "Number of Stocks",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_data("sp500")
plot_data("russell3000")




# ############################  End  ################################## #