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
        mutate(day_of_week = as.numeric(format(date, "%w")))

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

all_stocks <- df %>%
                select(symbol) %>%
                distinct() %>%
                arrange() %>%
                mutate(stock_num = row_number())

all_stock_nums <- unique(all_stocks$stock_num)

df <- df %>%
        left_join(latest_date_in_month) %>%
        left_join(latest_date_in_qtr) %>%
        left_join(latest_date_in_yr) %>%
        left_join(all_stocks) %>%
        mutate(latest_date_in_month = ifelse(date == month_date, 1, 0),
               latest_date_in_qtr = ifelse(date != qtr_date | is.na(qtr_date), 0, 1),
               latest_date_in_yr = ifelse(date != yr_date | is.na(yr_date), 0, 1),
               date_num = as.numeric(date)) %>%
        select(date_num, stock_num, value, day_of_week, 
               latest_date_in_month, latest_date_in_qtr, latest_date_in_yr)

n_stocks_list <- c(5, 10, 20, 50, 100, 250)
n_sims <- 1000
rebal_types <- c("daily", "weekly", "monthly", "quarterly", "yearly")

stock_sims <- data.frame(n_stock = c(), simulation = c())
stock_sim_counter <- 1
for(n_stocks in n_stocks_list){
  for(i in 1:n_sims){
    sim_stocks <- sort(sample(all_stock_nums, n_stocks, replace=TRUE))
    
    stock_sims[stock_sim_counter, "n_stock"] <- n_stocks
    stock_sims[stock_sim_counter, "simulation"] <- i
    stock_sims[stock_sim_counter, "sim_stocks"] <- paste(sim_stocks, collapse = ";")
    stock_sim_counter <- stock_sim_counter + 1
  }
}

final_results <- data.frame()
counter <- 1

for(rebal_type in rebal_types){
  print(rebal_type)
  if(rebal_type == "weekly"){
    filter_string <- paste0("day_of_week == 4")
    exponent <- 52
  } else if (rebal_type == "monthly"){
    filter_string <- paste0("latest_date_in_month == 1")
    exponent <- 12
  } else if (rebal_type == "quarterly"){
    filter_string <- paste0("latest_date_in_qtr == 1")
    exponent <- 4
  } else if (rebal_type == "yearly"){
    filter_string <- paste0("latest_date_in_yr == 1")
    exponent <- 1
  } else{
    filter_string <- paste0("value > 0")
    exponent <- 250
  }

  rets <- df %>%
    filter_(filter_string) %>%
    mutate(ret = ifelse(stock_num == lag(stock_num), value/lag(value) - 1, NA)) %>%
    filter(!is.na(ret))
  
  for(n_stocks in n_stocks_list){
    print(paste0("new number of stocks = ", n_stocks))
    for(i in 1:n_sims){
      print(i)
      
      sim_stocks <- as.numeric(unlist(strsplit(filter(stock_sims, n_stock == n_stocks & simulation == i) %>% select(sim_stocks) %>% as.character(), split = ";")))
      
      rets_filtered <- rets %>%
              filter(stock_num %in% sim_stocks) %>%
              group_by(date_num) %>%
              summarize(mean_ret = mean(ret) + 1) %>%
              ungroup() %>%
              summarize(ret_geo = prod(mean_ret)^(1/(length(unique(rets$date_num))/exponent)) - 1)
      
      final_results[counter, "simulation"] <- i
      final_results[counter, "n_stocks"] <- n_stocks
      final_results[counter, "rebalance_freq"] <- rebal_type
      final_results[counter, "exp"] <- exponent
      final_results[counter, "ret_geo"] <- rets_filtered 
      counter <- counter + 1
    }
  }
}

final_results_summarized <- final_results %>%
                              group_by(n_stocks, rebalance_freq, exp) %>%
                              summarize(avg_ret_geo = mean(ret_geo)) %>%
                              ungroup() %>%
                              arrange(n_stocks, -exp) %>%
                              select(n_stocks, rebalance_freq, avg_ret_geo)

to_plot <- final_results_summarized

source_string <- str_wrap(paste0("Source: YCharts (OfDollarsAndData.com)"), 
                          width = 85)
note_string <-  str_wrap(paste0("Note:  Shows ", n_sims, " simulations of various N-stock equal weighted portfolios by rebalancing frequency."), 
                         width = 85)

file_path <- paste0(out_path, "/rebal_freq.jpeg")

plot <- ggplot(data = to_plot, aes(x=n_stocks, y = avg_ret_geo, col = factor(rebalance_freq, levels = rebal_types))) +
  geom_line() +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Annualized Return by Number of Stocks\nand Rebalancing Frequency")) +
  labs(x = paste0("Number of Stocks in Portfolio"), y = paste0("Annualized Return"),
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #