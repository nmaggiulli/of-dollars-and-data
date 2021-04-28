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

make_plots <- 1
n_years <- 30
wt_stock <- 0.6
wt_bond <- 1 - wt_stock
bw_colors <- c("#969696", "#000000")

# Calculate median 1yr ret with dividends + inflation 
spx <- read.csv(paste0(importdir, "_jkb/0011_rebalance_stock_bond/spx_timeseries_4-27-2021.csv"), skip = 6) %>%
        rename(symbol = Symbol) %>%
        select(-Name, -Metric) %>%
        gather(-symbol, key=key, value = value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  filter(symbol != "", !is.na(value)) %>%
  arrange(date) %>%
  mutate(ret_1yr = lead(value, 12)/value - 1) %>%
  select(date, value, ret_1yr) %>%
  drop_na()

spx_median_1yr_ret <- quantile(spx$ret_1yr, probs = 0.5)

sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  select(date, price_plus_div) %>%
  mutate(ret_1yr = lead(price_plus_div, 12)/price_plus_div - 1) %>%
  filter(date >="1963-01-01") %>% 
  drop_na()

sp500_median_1yr_ret <- quantile(sp500$ret_1yr, probs = 0.5)
  
# Grab stock/bond data
raw <- read.csv(paste0(importdir, "_jkb/0011_rebalance_stock_bond/sp500_5yr_treasury.csv"), skip = 6) %>%
          filter(Date != "", `S.P.500.Index` != "") %>%
          mutate(date = as.Date(Date, format = "%m/%d/%Y"),
                 month = month(date),
                 ret_stock = as.numeric(`S.P.500.Index`),
                 ret_bond = as.numeric(`Five.Year.US.Treasury.Notes`)) %>%
          select(date, contains("ret_"), month) %>%
          filter(date < "2021-01-31")

run_rebal <- function(start_date, end_date, rebal_month, rebal_string, rebal_addition){
  
  df <- raw %>%
          filter(date >= start_date,
                 date < end_date)
  
  # Loop through rows
  for(i in 1:nrow(df)){
    mt <- df[i, "month"]
    
    if(i == 1){
      df[i, "value_stock"] <- wt_stock*100
      df[i, "value_bond"] <- wt_bond*100
      df[i, "value_port"] <- df[i, "value_stock"] + df[i, "value_bond"]
      df[i, "rebalance"] <- 0
      df[i, "ret_port"] <- 0
    } else{
      if(mt == rebal_month){
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
  }
  
  dd <- df %>%
          select(date, value_port) %>%
          drawdown_path()
  
  file_path <- paste0(out_path, "/rebal_", rebal_month, "_", rebal_addition, "/", date_to_string(start_date), "_area.jpeg")
  
  if(make_plots == 1 & start_date == "1930-01-31"){
  
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
                           rebal_month = rebal_month,
                           rebal_addition = rebal_addition,
                           final_value = final_value,
                           final_stock_pct = final_stock_pct,
                           max_dd = max_dd,
                           final_sd = final_sd,
                           final_cor = final_cor)
  
  return(tmp_result)
}

#Run all sims (if needed)
result_file <- paste0(localdir, "rebalance_stock_bond_all_sims.Rds")

if(!file.exists(result_file)){
  final_results <- data.frame()
  
  all_dates <- raw %>% filter(month(date) == 1) %>% select(date) %>% distinct()
  total_months <- nrow(all_dates)
  period_months <- n_years
  rebal_sims <- data.frame(rebal_month = c(rep(seq(1, 12, 1), 2), 99, 99),
                           rebal_additions = c(rep(0, 12), rep(100, 12), 0, 100))
  for(i in 1:nrow(rebal_sims)){
    
    r          <- rebal_sims[i, "rebal_month"]
    r_addition <- rebal_sims[i, "rebal_additions"]
    
    sim <- data.frame(start_date = all_dates[1:(total_months - period_months), "date"],
                           end_date = all_dates[(period_months+1):total_months, "date"],
                           rebal_month = rep(r, total_months - period_months))
    
    if(r  <= 12){
      rebal_string <- paste0("Rebalanced Each ", month.abb[r])
    } else{
      rebal_string <- "Never Rebalanced"
    }
    
    # Build directory
    dir.create(file.path(paste0(out_path, "/rebal_", r, "_", r_addition)), showWarnings = FALSE)
  
    # Run sims for rebalance period
    for(s in 1:nrow(sim)){
      start        <- sim[s, "start_date"]
      end          <- sim[s, "end_date"]
      rebal_month <- sim[s, "rebal_month"]
      
      print(start)
      print(rebal_month)
      if(s == 1 & i == 1){
        final_results <- run_rebal(start, end, rebal_month, rebal_string, r_addition)
      } else{
        final_results <- final_results %>% bind_rows(run_rebal(start, end, rebal_month, rebal_string, r_addition))
      }
    }
  }
  saveRDS(final_results, result_file)
} else{
  final_results <- readRDS(result_file)
}

summary <- final_results %>%
              filter(rebal_month <= 12) %>%
              group_by(start_date, end_date, rebal_addition) %>%
              summarise(min_value = min(final_value),
                        max_value = max(final_value),
                        value_pct_diff = max_value/min_value - 1,
                        value_diff_annualized = (1+value_pct_diff)^(1/n_years) - 1,
                        max_dd = mean(max_dd),
                        min_sd = min(final_sd),
                        max_sd = max(final_sd)) %>%
              ungroup() %>%
              mutate(rebal_addition_string = ifelse(rebal_addition == 0, "Not Adding Funds", "Adding Funds Monthly"))

# Do across all 12-month periods
to_plot <- summary %>%
  filter(rebal_addition == 0) %>%
  select(start_date, min_value, max_value) %>%
  gather(-start_date, key=key, value=value) %>%
  mutate(key = ifelse(key == "min_value", "Worst Month", "Best Month"))

no_additions_summary <- summary %>%
                      filter(rebal_addition == 0)

print(mean(no_additions_summary$value_diff_annualized))

file_path <- paste0(out_path, "/_growth_max_min.jpeg")

text_labels <- data.frame()
text_labels[1, "start_date"] <- as.Date("1956-01-01")
text_labels[1, "value"] <- 1900
text_labels[1, "label"] <- "Best Month"

text_labels[2, "start_date"] <- as.Date("1960-01-01")
text_labels[2, "value"] <- 800
text_labels[2, "label"] <- "Worst Month"

plot <- ggplot(to_plot, aes(x = start_date, y = value, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=start_date, y = value, col = label, label = label, family = "my_font")) +
  scale_color_manual(values = bw_colors, guide = FALSE) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Value For ",100*wt_stock, "/", 100*wt_bond," Portfolio\nRebalanced Annually\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Final Value of $100 Investment")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do max dd
to_plot <- summary %>%
            select(start_date, max_dd, rebal_addition_string)

file_path <- paste0(out_path, "/_max_dd_add_funds_vs_not.jpeg")

text_labels <- data.frame()
text_labels[1, "start_date"] <- as.Date("1961-01-01")
text_labels[1, "max_dd"] <- -0.05
text_labels[1, "label"] <- "Adding Funds Monthly"

text_labels[2, "start_date"] <- as.Date("1961-01-01")
text_labels[2, "max_dd"] <- -0.38
text_labels[2, "label"] <- "Not Adding Funds"

plot <- ggplot(to_plot, aes(x = start_date, y = max_dd, col = rebal_addition_string)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=start_date, y = max_dd, col = label, label = label, family = "my_font")) +
  scale_color_manual(values = bw_colors, guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.7, 0), breaks = seq(-0.7, 0, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Maximum Drawdown For ",100*wt_stock, "/", 100*wt_bond," Portfolio\nRebalanced Annually Over ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Maximum Drawdown")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Max dd by rebal
to_plot <- final_results %>%
  filter(rebal_addition == 0, rebal_month == 1 | rebal_month == 99) %>%
  mutate(rebalance_string = ifelse(rebal_month <= 12, "Rebalance Annually", "Never Rebalance"))

file_path <- paste0(out_path, "/_max_dd_rebalance_vs_not.jpeg")

text_labels <- data.frame()
text_labels[1, "start_date"] <- as.Date("1961-01-01")
text_labels[1, "max_dd"] <- -0.1
text_labels[1, "label"] <- "Rebalance Annually"

text_labels[2, "start_date"] <- as.Date("1961-01-01")
text_labels[2, "max_dd"] <- -0.45
text_labels[2, "label"] <- "Never Rebalance"

plot <- ggplot(to_plot, aes(x = start_date, y = max_dd, col = rebalance_string)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=start_date, y = max_dd, col = label, label = label, family = "my_font")) +
  scale_color_manual(values = bw_colors, guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.7, 0), breaks = seq(-0.7, 0, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Maximum Drawdown For ",100*wt_stock, "/", 100*wt_bond," Portfolio\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Maximum Drawdown")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot final stock percentage
to_plot <- final_results %>%
              filter(rebal_addition == 0, rebal_month == 1 | rebal_month == 99) %>%
              mutate(rebalance_string = ifelse(rebal_month <= 12, "Rebalance Annually", "Never Rebalance"))

file_path <- paste0(out_path, "/_stock_pct_rebalance_vs_not.jpeg")

text_labels <- data.frame()
text_labels[1, "start_date"] <- as.Date("1961-01-01")
text_labels[1, "final_stock_pct"] <- 1
text_labels[1, "label"] <- "Never Rebalance"

text_labels[2, "start_date"] <- as.Date("1961-01-01")
text_labels[2, "final_stock_pct"] <- 0.45
text_labels[2, "label"] <- "Rebalance Annually"

plot <- ggplot(to_plot, aes(x = start_date, y = final_stock_pct, col = rebalance_string)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=start_date, y = final_stock_pct, col = label, label = label, family = "my_font")) +
  scale_color_manual(values = bw_colors, guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Stock Percentage For ",100*wt_stock, "/", 100*wt_bond," Portfolio\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Final Stock Percentage")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do growth plot
file_path <- paste0(out_path, "/_growth_rebalance_vs_not.jpeg")

text_labels <- data.frame()
text_labels[1, "start_date"] <- as.Date("1940-01-01")
text_labels[1, "final_value"] <- 3000
text_labels[1, "label"] <- "Never Rebalance"

text_labels[2, "start_date"] <- as.Date("1970-01-01")
text_labels[2, "final_value"] <- 1000
text_labels[2, "label"] <- "Rebalance Annually"

plot <- ggplot(to_plot, aes(x = start_date, y = final_value, col = rebalance_string)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=start_date, y = final_value, col = label, label = label, family = "my_font")) +
  scale_color_manual(values = bw_colors, guide = FALSE) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Portfolio Value For ",100*wt_stock, "/", 100*wt_bond," Portfolio\nOver ", n_years, " Years")) +
  labs(x = "Start Year" , y = "Final Value of $100 Investment")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #