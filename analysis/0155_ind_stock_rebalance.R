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

folder_name <- "0155_ind_stock_rebalance"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Set seed for randomization
set.seed(12345)

# Import data
raw <- read.csv(paste0(importdir, "0155_ind_stocks_rebalance/ycharts.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%y-%m-%d")) %>%
  filter(symbol != "", !is.na(value)) %>%
  select(date, year, month, symbol, name, value) %>%
  arrange(symbol, date)

starting_stocks <- raw %>%
                filter(date == min(raw$date)) %>%
                select(symbol) %>%
                distinct()

raw_starting <- raw %>%
        inner_join(starting_stocks)

full_dates <- raw_starting %>%
  group_by(date) %>%
  summarize(n_stocks = n()) %>%
  ungroup() %>%
  filter(n_stocks == length(unique(raw_starting$symbol))) %>%
  select(date)

df <- raw_starting %>%
        inner_join(full_dates) %>%
        mutate(day_of_week = weekdays(date))

latest_date_in_month <- df %>%
                        group_by(year, month) %>%
                        summarize(month_date = max(date)) %>%
                        ungroup()

latest_date_in_qtr <- latest_date_in_month %>%
                        filter(month %in% c(1, 4, 7, 10)) %>%
                        rename(qtr_date = month_date)

latest_date_in_yr <- latest_date_in_month %>%
  filter(month %in% c(7)) %>%
  rename(yr_date = month_date)

df <- df %>%
        left_join(latest_date_in_month) %>%
        left_join(latest_date_in_qtr) %>%
        left_join(latest_date_in_yr) %>%
        mutate(latest_date_in_month = ifelse(date == month_date, 1, 0),
               latest_date_in_qtr = ifelse(date == qtr_date, 1, 0),
               latest_date_in_yr = ifelse(date == yr_date, 1, 0)) %>%
        select(date, symbol, name, value, day_of_week, 
               latest_date_in_month, latest_date_in_qtr, latest_date_in_yr)

all_stocks <- unique(df$symbol)

n_stocks <- 20
n_sims <- 100
rebal_types <- c("daily", "weekly", "monthly", "quarterly", "yearly")

final_results <- data.frame()
counter <- 1

for(i in 1:n_sims){
  print(i)
  sim_stocks <- sample(all_stocks, n_stocks, replace=TRUE)
  
  for(rebal_type in rebal_types){
    
    if(rebal_type == "weekly"){
      filter_string <- paste0("day_of_week == 'Thursday'")
      exponent_numerator <- 52
    } else if (rebal_type == "monthly"){
      filter_string <- paste0("latest_date_in_month == 1")
      exponent_numerator <- 12
    } else if (rebal_type == "quarterly"){
      filter_string <- paste0("latest_date_in_qtr == 1")
      exponent_numerator <- 4
    } else if (rebal_type == "yearly"){
      filter_string <- paste0("latest_date_in_yr == 1")
      exponent_numerator <- 1
    } else{
      filter_string <- paste0("value > 0")
      exponent_numerator <- 250
    }

    rets <- df %>%
                filter(symbol %in% sim_stocks) %>%
                filter_(filter_string) %>%
                mutate(ret = ifelse(symbol == lag(symbol), value/lag(value) - 1, NA)) %>%
                filter(!is.na(ret)) %>%
                group_by(date) %>%
                summarize(mean_ret = mean(ret) + 1) %>%
                ungroup()
      
    final_results[counter, "simulation"] <- i
    final_results[counter, "n_stocks"] <- n_stocks
    final_results[counter, "rebalance_freq"] <- rebal_type
    final_results[counter, "exp"] <- exponent_numerator
    final_results[counter, "ret_geo"] <- rets %>%
                summarize(ret_geo = prod(mean_ret)^(exponent_numerator/nrow(rets)) - 1)
    counter <- counter + 1
  }
}

final_results_summarized <- final_results %>%
                              group_by(rebalance_freq, exp) %>%
                              summarize(avg_ret_geo = mean(ret_geo)) %>%
                              ungroup() %>%
                              arrange(-exp) %>%
                              select(rebalance_freq, avg_ret_geo)


# ############################  End  ################################## #