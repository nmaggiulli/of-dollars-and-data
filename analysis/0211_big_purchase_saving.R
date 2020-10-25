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

#Bring in raw data
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

# Global Sim paramaters
monthly_payment <- 1000
cap_gains <- 0.15

source_string <- paste0("Source: Shiller data, Returns 2.0, ", min_year, "-", max_year, " (OfDollarsAndData.com)")

# Select dates
dates_to_run <- stock_bond %>%
                  select(date) %>%
                  distinct() %>%
                  filter(month(date) %in% c(1, 4, 7, 10), year(date) < 2016) %>%
                  pull(date)

# Find months to test
months_to_test <- seq(24, 60, 6)

for(expected_months in months_to_test){
  # Sim paramaters
  print(expected_months)
  required_savings <- monthly_payment*expected_months
  
  run_saving_sim <- function(s_weight, b_weight, c_weight, name, i_name){
    
    tmp_results <- matrix(nrow = length(dates_to_run), ncol = 2)
    
    counter <- 1
    for(d in dates_to_run){
      tmp<- stock_bond %>% 
        filter(date >= d) %>%
        select(ret_sp500, ret_bond_real, ret_cash_real) %>%
        as.matrix()
      
      current_savings <- 0
      liquid_savings <- 0
      cost_basis <- 0
      month_counter <- 1
      
      while(liquid_savings < required_savings & month_counter < nrow(tmp)){
        s_ret <- tmp[month_counter, 1]
        b_ret <- tmp[month_counter, 2]
        c_ret <- tmp[month_counter, 3]
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
      
      tmp_results[counter, 1] <- as.Date(d, format = "%Y-%m-%d")
      tmp_results[counter, 2] <- month_counter
      
      counter <- counter + 1
    }
    
    final_results <- data.frame(start_date = as.Date(tmp_results[, 1]),
                                n_months_to_goal = tmp_results[, 2])
    
    assign(paste0("final_results_", i_name), final_results, envir = .GlobalEnv)
    
    time_buckets <- final_results %>%
      mutate(time_bucket = case_when(
        n_months_to_goal <= (expected_months - 12) ~ "12m+\nearly",
        n_months_to_goal <= (expected_months - 6) ~ "6m-12m\nearly",
        n_months_to_goal <= expected_months ~ "0m-6m\nearly",
        n_months_to_goal <= (expected_months + 6) ~ "1m-6m\nlate",
        n_months_to_goal <= (expected_months + 12) ~ "6m-12m\nlate",
        TRUE ~ "12m+\nlate"
      )) %>%
      group_by(time_bucket) %>%
      summarize(n_quarters = n(),
                pct = n()/ nrow(final_results)) %>%
      ungroup()
    
    tb_levels <- c("12m+\nearly",
                   "6m-12m\nearly",
                   "0m-6m\nearly",
                   "1m-6m\nlate",
                   "6m-12m\nlate",
                   "12m+\nlate"
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
    
    file_path <- paste0(out_path, "/_", expected_months, "m_", name, "_distribution_of_saving_times.jpeg")
    #Create note for charts
    note_string <- str_wrap(
      paste0("Note: Assumes you save ", format_as_dollar(monthly_payment), 
             " per month for ", 
             expected_months,
             " months, that capital gains taxes are paid at the long term rate of ", 
             100*cap_gains, 
             "%, and that all returns are adjusted for inflation. ",
             "It takes ", round(mean(final_results$n_months_to_goal, na.rm = TRUE),0), " months on average to reach your savings goal when investing in ", tolower(i_name), "."),
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
    file_path <- paste0(out_path, "/_", expected_months, "m_", name, "_cumulative_saving_times.jpeg")
    
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
    file_path <- paste0(out_path, "/_", expected_months, "m_", name, "_by_qtr.jpeg")
    
    plot <- ggplot(final_results, aes(x = start_date, y = n_months_to_goal)) +
      geom_bar(stat = "identity", fill = chart_standard_color, width = 92) +
      geom_hline(yintercept = expected_months, linetype = "dashed", col = "black") +
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
           stocks_win = ifelse(n_months_stocks < n_months_cash, 1, 0),
           net_bonds =  case_when(n_months_cash - n_months_bonds > 12 ~ 12,
                                  n_months_cash - n_months_bonds < -12 ~ -12,
                                  TRUE ~ n_months_cash - n_months_bonds),
           net_stocks = case_when(n_months_cash - n_months_stocks >12 ~ 12,
                                  n_months_cash - n_months_stocks < -12 ~ -12,
                                  TRUE ~ n_months_cash - n_months_stocks))
  
  
  file_path <- paste0(out_path, "/net_bonds_", expected_months, "m_.jpeg")
  #Create note for charts
  note_string <- str_wrap(
    paste0("Note: Assumes you save ", format_as_dollar(monthly_payment), 
           " per month for ", 
           expected_months,
           " months, that capital gains taxes are paid at the long term rate of ", 
           100*cap_gains, 
           "%, and that all returns are adjusted for inflation.  ",
           "Differences greater than 12 months have been capped at 12 months for simplicity.  ",
           "Bonds outperform cash in ", round(100*mean(final_results_all$bonds_win, na.rm = TRUE), 1), "% of periods."),
    width = 85)
  
  plot <- ggplot(final_results_all, aes(x = start_date, y = net_bonds)) +
    geom_bar(stat = "identity", fill = chart_standard_color, width = 92) +
    scale_x_date(date_labels = "%Y",
                 limits = c(as.Date("1926-01-01"), as.Date("2016-10-01")),
                 breaks = seq.Date(from = as.Date("1920-01-01"),
                                   to = as.Date("2010-01-01"),
                                   by = "10 years")) +
    scale_y_continuous(limits = c(-3, 12), breaks = seq(-3, 12, 3)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Additional Months for Cash to Reach Savings Goal\nCompared to Investing in Bonds\nOver ", expected_months, " Months")) +
    labs(x = "Start Date" , y = paste0("Additional Months"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do stocks
  file_path <- paste0(out_path, "/net_stocks_", expected_months, "m_.jpeg")
  note_string <- str_wrap(
    paste0("Note: Assumes you save ", format_as_dollar(monthly_payment), 
           " per month for ", 
           expected_months,
           " months, that capital gains taxes are paid at the long term rate of ", 
           100*cap_gains, 
           "%, and that all returns are adjusted for inflation.  ",
           "Differences greater than 12 months have been capped at 12 months for simplicity.  ",
           "Stocks outperform cash in ", round(100*mean(final_results_all$stocks_win, na.rm = TRUE), 1), "% of periods."),
    width = 85)
  
  plot <- ggplot(final_results_all, aes(x = start_date, y = net_stocks)) +
    geom_bar(stat = "identity", fill = chart_standard_color, width = 92) +
    scale_x_date(date_labels = "%Y",
                 limits = c(as.Date("1926-01-01"), as.Date("2016-10-01")),
                 breaks = seq.Date(from = as.Date("1920-01-01"),
                                   to = as.Date("2010-01-01"),
                                   by = "10 years")) +
    scale_y_continuous(limits = c(-12, 12), breaks = seq(-12, 12, 6)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Additional Months for Cash to Reach Savings Goal\nCompared to Investing in Stocks\nOver ", expected_months, " Months")) +
    labs(x = "Start Date" , y = paste0("Additional Months"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_gif(path = paste0(out_path), 
           file_stub = "net_bonds_*.jpeg",
           speed_milliseconds = 100,
           out_name = paste0("/_net_bonds_all_months.gif"))

create_gif(path = paste0(out_path), 
           file_stub = "net_stocks_*.jpeg",
           speed_milliseconds = 100,
           out_name = paste0("/_net_stocks_all_months.gif"))


# ############################  End  ################################## #