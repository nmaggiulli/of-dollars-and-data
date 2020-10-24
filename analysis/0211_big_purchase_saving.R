cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0211_big_purchase_saving"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Sim paramaters
monthly_payment <- 1000

# Expected months must be >= 24
expected_months <- 60
required_savings <- monthly_payment*expected_months

bond_ret <- read.csv(paste0(importdir, "/0209_bond_rets/treasury_5yr.csv"), skip = 7,
                     col.names = c("date", "index_cpi", "index_bond")) %>%
  drop_na() %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(date = as.Date(paste0(year(date), "-", month(date), "-01")),
         ret_bond = ifelse(is.na(lag(index_bond)), 0, index_bond/lag(index_bond) - 1),
         ret_cash_real = ifelse(is.na(lag(index_cpi)), 0, -1*(index_cpi/lag(index_cpi) - 1)),
         ret_bond_real = ret_bond + ret_cash_real) %>%
  select(date, ret_bond_real, ret_cash_real)

stock_bond <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
  select(date, price_plus_div) %>%
  mutate(ret_sp500 = price_plus_div/lag(price_plus_div) - 1) %>%
  filter(date >= "1926-01-01") %>%
  left_join(bond_ret)

min_year <- year(min(stock_bond$date))
max_year <- year(max(stock_bond$date))

dates_to_run <- stock_bond %>%
                  select(date) %>%
                  distinct() %>%
                  filter(month(date) %in% c(1), year(date) < 2016) %>%
                  pull(date)

run_saving_sim <- function(s_weight, b_weight, c_weight, name, i_name){
  
  final_results <- data.frame()
  
  counter <- 1
  for(d in dates_to_run){
    print(as.Date(d, format = "%Y-%m-%d"))
    tmp<- stock_bond %>% filter(date >= d)
    
    current_savings <- 0
    liquid_savings <- 0
    cost_basis <- 0
    month_counter <- 1
    cap_gains <- 0.15
    
    while(liquid_savings < required_savings & month_counter < nrow(tmp)){
      s_ret <- tmp[month_counter, "ret_sp500"]
      b_ret <- tmp[month_counter, "ret_bond_real"]
      c_ret <- tmp[month_counter, "ret_cash_real"]
      ret <- s_weight*s_ret + b_weight*b_ret + c_weight*c_ret
      
      cost_basis <- cost_basis + monthly_payment 
      
      if(current_savings == 0){
        current_savings <- monthly_payment * (1 + ret)
      } else{
        current_savings <- (current_savings + monthly_payment) * (1 + ret)
      }
      
      if(cost_basis > current_savings){
        liquid_savings <- current_savings
      } else{
        liquid_savings <- ((current_savings - cost_basis) * (1 - cap_gains)) + cost_basis
      }
      
      month_counter <- month_counter + 1
    }
    
    if(month_counter >= nrow(tmp)){
      month_counter <- NA
    }
    
    final_results[counter, "start_date"] <- as.Date(d, format = "%Y-%m-%d")
    final_results[counter, "n_months_to_goal"] <- month_counter
    print(month_counter)
    
    counter <- counter + 1
  }
  
  assign(paste0("final_results_", i_name), final_results, envir = .GlobalEnv)
  
  time_buckets <- final_results %>%
    mutate(time_bucket = case_when(
      n_months_to_goal < (expected_months - 12) ~ "12m+ early",
      n_months_to_goal < (expected_months - 6) ~ "6-12m early",
      n_months_to_goal < expected_months ~ "1-6m early",
      n_months_to_goal < (expected_months + 5) ~ "1-6m late",
      n_months_to_goal < (expected_months + 13) ~ "6-12m late",
      TRUE ~ "12m+ late"
    )) %>%
    group_by(time_bucket) %>%
    summarize(n_quarters = n(),
          pct = n()/ nrow(final_results)) %>%
    ungroup()
  
  tb_levels <- c("12m+ early",
                 "6-12m early",
                 "1-6m early",
                 "1-6m late",
                 "6-12m late",
                 "12m+ late"
                 )

  to_plot <- data.frame(time_bucket = tb_levels) %>%
    full_join(time_buckets) %>% 
    mutate(pct = ifelse(is.na(pct), 0, pct))
  
  to_plot$time_bucket <- factor(to_plot$time_bucket, levels = tb_levels)
  
  to_plot <- to_plot %>% arrange(time_bucket)
  
  for(i in 1:nrow(to_plot)){
    if(i == 1){
      to_plot[i, "pct_cumulative"] <- to_plot[i, "pct"]
    } else{
      to_plot[i, "pct_cumulative"] <- to_plot[(i-1), "pct_cumulative"] + to_plot[i, "pct"]
    }
  }
  
  assign(paste0("to_plot_", i_name), to_plot, envir = .GlobalEnv)
  
  file_path <- paste0(out_path, "/", name, "_distribution_of_saving_times.jpeg")
  source_string <- paste0("Source: Shiller data, Returns 2.0, ", min_year, "-", max_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(
    paste0("Note: Assumes you save ", format_as_dollar(monthly_payment), 
           " per month, that capital gains taxes are paid at the long term rate of ", 
           100*cap_gains, 
           "%, and that all returns are adjusted for inflation."),
    width = 85)
  
  plot <- ggplot(to_plot, aes(x = time_bucket, y = pct)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("How Long it Takes to Save ", format_as_dollar(required_savings, 0), "\nWhen Fully Invested in ", i_name)) +
    labs(x = "Number of Months" , y = paste0("Percentage of Time"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do cumulative plot
  file_path <- paste0(out_path, "/", name, "_cumulative_saving_times.jpeg")
  
  plot <- ggplot(to_plot, aes(x = time_bucket, y = pct_cumulative)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Cumulative Distribution of Savings Times\nWhen Fully Invested in ", i_name)) +
    labs(x = "Number of Months" , y = paste0("Cumulative Percentage"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do date plot
  file_path <- paste0(out_path, "/", name, "_by_qtr.jpeg")
  
  plot <- ggplot(final_results, aes(x = start_date, y = n_months_to_goal)) +
    geom_bar(stat = "identity", fill = chart_standard_color, width = 92) +
    scale_x_date(date_labels = "%Y",
                 limits = c(as.Date("1926-01-01"), as.Date("2016-10-01")),
                 breaks = seq.Date(from = as.Date("1920-01-01"),
                                   to = as.Date("2010-01-01"),
                                   by = "10 years")) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Number of Months to Reach Goal\nWhen Fully Invested in ", i_name)) +
    labs(x = "Start Date" , y = paste0("Number of Months"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

run_saving_sim(1, 0, 0, "all_stock", "Stocks")
run_saving_sim(0, 1, 0, "all_bond", "Bonds")
run_saving_sim(0, 0, 1, "all_cash", "Cash")

final_results_all <- final_results_Bonds %>% rename(n_months_bonds = n_months_to_goal) %>%
                      left_join(final_results_Cash %>% rename(n_months_cash = n_months_to_goal)) %>%
                      left_join(final_results_Stocks %>% rename(n_months_stocks = n_months_to_goal)) %>%
                      mutate(bonds_win = ifelse(n_months_bonds < n_months_cash, 1, 0),
                             stocks_win = ifelse(n_months_stocks < n_months_cash, 1, 0))

win_summary <- final_results_all %>%
                summarize(bonds_win_pct = mean(bonds_win, na.rm = TRUE),
                          stocks_win_pct = mean(stocks_win, na.rm = TRUE))

# ############################  End  ################################## #