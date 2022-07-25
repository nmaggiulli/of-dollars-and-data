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

plot_data <- function(index_name, start_dt, end_dt, yr){
  
  if(index_name == "sp500"){
    proper_name <- "S&P 500"
  } else if(index_name == "russell2000"){
    proper_name <- "Russell 2000"
  }
  
  if(yr == 2020){
    yr_string <- "March 2020"
  } else {
    yr_string <- yr
  }
  
  raw <- read.csv(paste0(importdir, "/0305_ycharts_stocks_daily_2018/timeseries_7-20-2022_", index_name, ".csv"),
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
  
  assign(paste0("df_", index_name), df, envir = .GlobalEnv)
  
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
  
  assign(paste0("highs_lows_", index_name), highs_lows, envir = .GlobalEnv)
  
  high_to_low <- highs_lows %>%
    filter(!is.na(low_52wk), !is.na(lag(high_52wk)), date > high_date) %>%
    select(-high_52wk) %>%
    rename(low_date = date,
           high_52wk = high) %>%
    select(symbol, high_date, high_52wk, low_date, low_52wk, n_days_to_low)
  
  assign(paste0("high_to_low_", index_name), high_to_low, envir = .GlobalEnv)
  
  to_plot <- high_to_low %>%
                filter(low_date >= start_dt, low_date <= end_dt) %>%
                arrange(low_date) %>%
                group_by(low_date) %>%
                summarise(n_lows = n()) %>%
                ungroup()
  
  print(paste0("Number of stocks that hit 52-week lows in ", yr, " for ", index_name, ":", sum(to_plot$n_lows)))
  
  max_y_limit <- round_to_nearest(max(to_plot$n_lows), "up", 10)
  
  file_path <- paste0(out_path, "/", index_name, "_stocks_52wk_lows_march_", yr, ".jpg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Shows number of stocks in the ", proper_name, " that first hit 52-week lows. ",
                          "Does not include dividends and is not adjusted for inflation."), 
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=low_date, y=n_lows)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = comma, limits = c(0, max_y_limit)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("When Stocks First Hit Their 52-Week Lows\n", proper_name, " (", yr_string, ")")) +
    labs(x = "Date", y = "Number of Stocks",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  #Plot number of stocks at 52-week lows
  to_plot <- df %>%
    filter(price == low_52wk, date >= start_dt, date <= end_dt) %>%
    group_by(date) %>%
    summarise(n_stocks = n()) %>%
    ungroup()
  
  file_path <- paste0(out_path, "/", index_name, "_stocks_number_52wk_lows_", yr, ".jpg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Shows number of stocks in the ", proper_name, " that are at 52-week lows. ",
                                 "Does not include dividends and is not adjusted for inflation."), 
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=date, y=n_stocks)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = comma) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Number of Stocks At Their 52-Week Lows\n", proper_name, " (", yr_string, ")")) +
    labs(x = "Date", y = "Number of Stocks",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

indices <- c("sp500", "russell2000")

for(index in indices){
  plot_data(index, as.Date("2020-02-19"), as.Date("2020-03-23"), "2020")
  plot_data(index, as.Date("2022-01-18"), as.Date("2022-12-31"), "2022")
}

# Plot S&P 500 drawdowns since 2013
raw <- read.csv(paste0(importdir, "/0305_ycharts_stocks_daily_2018/SPX_data.csv")) %>%
          mutate(date = as.Date(Period),
                 index_sp500 = `S.P.500...SPX..Level`) %>%
          select(date, index_sp500) %>%
          arrange(date)

dd <- drawdown_path(raw)
dd <- add_dd_counter(dd)

dd_10pct_plus <- dd %>%
            group_by(dd_counter) %>%
            summarise(max_dd = min(pct),
                      min_date = min(date)) %>%
            ungroup() %>%
            filter(max_dd < -0.1) %>%
            select(dd_counter, max_dd, min_date)

to_plot <- dd %>%
            inner_join(dd_10pct_plus) %>%
            group_by(dd_counter) %>%
            mutate(day = row_number(),
                   dd_start_date = as.Date(paste0(year(min_date), "-", month(min_date), "-01")),
                   dd_start_date_string = format.Date(dd_start_date, "%Y (%b)")) %>%
            ungroup() %>%
            select(date, day, pct, dd_start_date_string)

file_path <- paste0(out_path, "/aligned_sp500_dd_since_2013_dd_gt_10pct.jpg")
source_string <- paste0("Source: YCharts")
note_string <- str_wrap(paste0("Note: Does not include dividends and is not adjusted for inflation."), 
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=day, y=pct, col=as.factor(dd_start_date_string),
                                   size = as.factor(dd_start_date_string))) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  scale_size_manual(values = c(rep(0.5, 4), 1.5)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("All S&P 500 Drawdowns\nGreater Than 10% Since 2013")) +
  labs(x = "Trading Days", y = "Percentage Decline",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")           

# ############################  End  ################################## #