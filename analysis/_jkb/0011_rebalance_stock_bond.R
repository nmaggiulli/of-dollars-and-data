cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(readxl)
library(tidyverse)

folder_name <- "_jkb/0011_rebalance_stock_bond"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

make_plots <- 0
n_years <- 30
wt_stock <- 0.6
wt_bond <- 1 - wt_stock
bw_colors <- c("#969696", "#000000")

raw <- read.csv(paste0(importdir, "_jkb/0011_rebalance_stock_bond/sp500_5yr_treasury.csv"), skip = 6) %>%
          filter(Date != "", `S.P.500.Index` != "") %>%
          mutate(date = as.Date(Date, format = "%m/%d/%Y"),
                 ret_stock = as.numeric(`S.P.500.Index`),
                 ret_bond = as.numeric(`Five.Year.US.Treasury.Notes`)) %>%
          select(date, contains("ret_")) %>%
          filter(date <= "2021-01-31")

run_rebal <- function(start_date, end_date, rebal_months, rebal_string, rebal_addition){
  
  df <- raw %>%
          filter(date >= start_date,
                 date < end_date)
  
  rebal_counter <- 0
  
  # Loop through rows
  for(i in 1:nrow(df)){
    if(i == 1){
      df[i, "value_stock"] <- wt_stock*100
      df[i, "value_bond"] <- wt_bond*100
      df[i, "value_port"] <- df[i, "value_stock"] + df[i, "value_bond"]
      df[i, "rebalance"] <- 0
      df[i, "ret_port"] <- 0
    } else{
      if(rebal_counter == rebal_months){
        rebal_counter <- 0
        
        df[i, "rebalance"] <- 1
        df[i, "value_stock"] <- (df[(i-1), "value_port"] * wt_stock) * (1 + df[i, "ret_stock"]) + (rebal_addition*wt_stock)
        df[i, "value_bond"] <- (df[(i-1), "value_port"] * wt_bond) * (1 + df[i, "ret_bond"]) + (rebal_addition*wt_bond)
      } else{
        df[i, "rebalance"] <- 0
        df[i, "value_stock"] <- df[(i-1), "value_stock"] * (1 + df[i, "ret_stock"]) + (rebal_addition*wt_stock)
        df[i, "value_bond"] <- df[(i-1), "value_bond"] * (1 + df[i, "ret_bond"]) + (rebal_addition*wt_bond)
      }
      df[i, "value_port"] <- df[i, "value_stock"] + df[i, "value_bond"]
      df[i, "ret_port"] <- df[i, "value_port"]/df[(i-1), "value_port"] - 1
      
    }
    rebal_counter <- rebal_counter + 1
  }
  
  dd <- df %>%
          select(date, value_port) %>%
          drawdown_path()
  
  file_path <- paste0(out_path, "/rebal_", rebal_months, "_", rebal_addition, "/", date_to_string(start_date), "_area.jpeg")
  
  if(!file.exists(file_path) & make_plots == 1){
  
    to_plot <- df %>%
      select(date, contains("value_")) %>%
      mutate(`Stock %` = value_stock/value_port,
             `Bond %` = 1 - `Stock %`) %>%
      select(date, `Stock %`, `Bond %`) %>%
      gather(-date, key=key, value=value)
    
    plot <- ggplot(to_plot, aes(x=date, y=value, fill=key)) + 
      geom_area(alpha=0.6 , size=1) +
      scale_fill_manual(values = bw_colors[1:2]) +
      scale_y_continuous(label = percent, breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
      of_dollars_and_data_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom") +
      ggtitle(paste0(100*wt_stock, "/", 100*wt_bond," Stock/Bond Portfolio Share\n", rebal_string)) +
      labs(x = "Date" , y = "Percentage in Stocks/Bonds")
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
  final_value <- df[nrow(df), "value_port"]
  final_stock_pct <- df[nrow(df), "value_stock"]/final_value
  final_sd <- sd(df$ret_port) * sqrt(12)
  max_dd   <- min(dd$pct)
  final_cor <- cor(df$ret_stock, df$ret_bond)
  
  tmp_result <- data.frame(start_date = start_date,
                           end_date = end_date,
                           rebal_months = rebal_months,
                           rebal_addition = rebal_addition,
                           final_value = final_value,
                           final_stock_pct = final_stock_pct,
                           max_dd = max_dd,
                           final_sd = final_sd,
                           final_cor = final_cor)
  
  return(tmp_result)
}

all_dates <- raw %>% filter(month(date) == 1) %>% select(date) %>% distinct()
total_months <- nrow(all_dates)
period_months <- n_years
rebal_sims <- data.frame(rebal_periods = c(12, 12, 9999, 9999),
                         rebal_additions = c(0, 100, 0, 100))

#Run all sims
final_results <- data.frame()

for(i in 1:nrow(rebal_sims)){
  
  r          <- rebal_sims[i, "rebal_periods"]
  r_addition <- rebal_sims[i, "rebal_additions"]
  
  sim <- data.frame(start_date = all_dates[1:(total_months - period_months), "date"],
                         end_date = all_dates[(period_months+1):total_months, "date"],
                         rebal_period = rep(r, total_months - period_months))
  
  if(r == 12){
    rebal_string <- "Rebalanced Annually"
  } else{
    rebal_string <- "Never Rebalanced"
  }
  
  # Build directory
  dir.create(file.path(paste0(out_path, "/rebal_", r, "_", r_addition)), showWarnings = FALSE)

  # Run sims for rebalance period
  for(s in 1:nrow(sim)){
    start        <- sim[s, "start_date"]
    end          <- sim[s, "end_date"]
    rebal_period <- sim[s, "rebal_period"]
    
    print(start)
    print(rebal_period)
    if(s == 1 & i == 1){
      final_results <- run_rebal(start, end, rebal_period, rebal_string, r_addition)
    } else{
      final_results <- final_results %>% bind_rows(run_rebal(start, end, rebal_period, rebal_string, r_addition))
    }
  }
  
  if(make_plots == 1){

    to_plot <- final_results %>%
      filter(rebal_months == r, rebal_addition == r_addition)
    
    file_path <- paste0(out_path, "/growth_of_dollar_", r, "_", r_addition, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = start_date, y = final_value)) +
      geom_bar(stat = "identity", fill = bw_colors[2]) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Growth of $100 For ",100*wt_stock, "/", 100*wt_bond," Portfolio\n", rebal_string, " Over ", n_years, " Years")) +
      labs(x = "Start Year" , y = "Growth of $100")
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

to_plot <- final_results %>%
              filter(rebal_addition == 100) %>%
              mutate(rebal_addition_string = ifelse(rebal_addition == 0, "Not Adding Funds", "Adding Funds"),
                     rebalance_string = ifelse(rebal_months == 12, "Rebalance Annually", "Never Rebalance"))

file_path <- paste0(out_path, "/max_dd_", r, "_", r_addition, ".jpeg")

plot <- ggplot(to_plot, aes(x = start_date, y = max_dd, col = rebalance_string)) +
  geom_line() +
  scale_color_manual(values = bw_colors) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-1, 0), breaks = seq(-1, 0, 0.1)) +
  of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
  ggtitle(paste0("Maximim Drawdown For  ",100*wt_stock, "/", 100*wt_bond," Portfolio\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Maximum Drawdown")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/stock_pct_", r, "_", r_addition, ".jpeg")

plot <- ggplot(to_plot, aes(x = start_date, y = final_stock_pct, col = rebalance_string)) +
  geom_line() +
  scale_color_manual(values = bw_colors) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Final Stock Percentage For  ",100*wt_stock, "/", 100*wt_bond," Portfolio\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Final Stock Percentage")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #