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

run_rebal <- function(start_date, end_date, rebal_months, rebal_string){
  
  df <- raw %>%
          filter(date >= start_date,
                 date < end_date)
  
  rebal_counter <- 0
  
  # Loop through rows
  for(i in 1:nrow(df)){
    if(i == 1){
      df[i, "value_stock"] <- wt_stock
      df[i, "value_bond"] <- wt_bond
      df[i, "value_port"] <- df[i, "value_stock"] + df[i, "value_bond"]
      df[i, "rebalance"] <- 0
      df[i, "ret_port"] <- 0
    } else{
      if(rebal_counter == rebal_months){
        rebal_counter <- 0
        
        df[i, "rebalance"] <- 1
        df[i, "value_stock"] <- (df[(i-1), "value_port"] * wt_stock) * (1 + df[i, "ret_stock"])
        df[i, "value_bond"] <- (df[(i-1), "value_port"] * wt_bond) * (1 + df[i, "ret_bond"])
      } else{
        df[i, "rebalance"] <- 0
        df[i, "value_stock"] <- df[(i-1), "value_stock"] * (1 + df[i, "ret_stock"])
        df[i, "value_bond"] <- df[(i-1), "value_bond"] * (1 + df[i, "ret_bond"])
      }
      df[i, "value_port"] <- df[i, "value_stock"] + df[i, "value_bond"]
      df[i, "ret_port"] <- df[i, "value_port"]/df[(i-1), "value_port"] - 1
      
    }
    rebal_counter <- rebal_counter + 1
  }
  
  to_plot <- df %>%
    select(date, contains("value_")) %>%
    mutate(`Stock %` = value_stock/value_port,
           `Bond %` = 1 - `Stock %`) %>%
    select(date, `Stock %`, `Bond %`) %>%
    gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/rebal_", rebal_months, "/", date_to_string(start_date), "_area.jpeg")
  
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
  
  final_value <- df[nrow(df), "value_port"]
  final_sd <- sd(df$ret_port) * sqrt(12)
  final_cor <- cor(df$ret_stock, df$ret_bond)
  
  tmp_result <- data.frame(start_date = start_date,
                           end_date = end_date,
                           rebal_months = rebal_months,
                           final_value = final_value,
                           final_sd = final_sd,
                           final_cor = final_cor)
  
  return(tmp_result)
}

all_dates <- raw %>% filter(month(date) == 1) %>% select(date) %>% distinct()
total_months <- nrow(all_dates)
period_months <- n_years
rebal_periods <- c(1, 12, 9999)

#Run all sims
final_results <- data.frame()

for(i in 1:length(rebal_periods)){
  
  r <- rebal_periods[i]
  
  sim <- data.frame(start_date = all_dates[1:(total_months - period_months), "date"],
                         end_date = all_dates[(period_months+1):total_months, "date"],
                         rebal_period = rep(r, total_months - period_months))
  
  if(r == 1){
    rebal_string <- "Rebalanced Monthly"
  } else if(r == 12){
    rebal_string <- "Rebalanced Annually"
  } else{
    rebal_string <- "Never Rebalanced"
  }
  
  # Build directory
  dir.create(file.path(paste0(out_path, "/rebal_", r)), showWarnings = FALSE)

  # Run sims for rebalance period
  for(s in 1:nrow(sim)){
    start        <- sim[s, "start_date"]
    end          <- sim[s, "end_date"]
    rebal_period <- sim[s, "rebal_period"]
    
    print(start)
    print(rebal_period)
    if(s == 1){
      final_results <- run_rebal(start, end, rebal_period, rebal_string)
    } else{
      final_results <- final_results %>% bind_rows(run_rebal(start, end, rebal_period, rebal_string))
    }
  }
  
  to_plot <- final_results %>%
    filter(rebal_months == r)
  
  file_path <- paste0(out_path, "/growth_of_dollar_", r, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = start_date, y = final_value)) +
    geom_bar(stat = "identity", fill = bw_colors[2]) +
    scale_y_continuous(label = dollar, limits = c(0, 40)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Growth of $1 For  ",100*wt_stock, "/", 100*wt_bond," Portfolio\n", rebal_string, " Over ", n_years, " Years")) +
    labs(x = "Start Year" , y = "Growth of $1")
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  file_path <- paste0(out_path, "/sd_", r, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = start_date, y = final_sd)) +
    geom_bar(stat = "identity", fill = bw_colors[2]) +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Standard Deviation For  ",100*wt_stock, "/", 100*wt_bond," Portfolio\n", rebal_string, " Over ", n_years, " Years")) +
    labs(x = "Start Year" , y = "Annualized Standard Deviation")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

to_plot <- final_results %>%
              select(date, final_cor) %>%
              distinct() %>%
              arrange(date)

file_path <- paste0(out_path, "/correlation_stock_bond.jpeg")

plot <- ggplot(to_plot, aes(x = start_date, y = final_cor)) +
  geom_bar(stat = "identity", fill = bw_colors[2]) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Rolling ", n_years, "-Year Correlation\nBetween Stocks and Bonds")) +
  labs(x = "Start Year" , y = "Correlation")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")




# ############################  End  ################################## #