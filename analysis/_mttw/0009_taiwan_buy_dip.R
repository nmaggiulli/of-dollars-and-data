cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "_mttw/0009_taiwan_buy_dip"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_msci <- read.csv(paste0(importdir, "/", folder_name, "/GrowthOfWealth_20251113221715_tw.csv"),
                     col.names = c("date", "index_tw"), skip = 6) %>%
            filter(!(date == ""), !(index_tw == "")) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y") + days(1) - months(1),
                   index = as.numeric(index_tw)) %>%
            arrange(date) %>%
            select(date, index)

dd <- drawdown_path(raw_msci %>% select(date, index))

start_year <- year(min(dd$date))
end_year <- year(max(dd$date))

#Plot DD
file_path <- paste0(out_path, "/dd_msci_taiwan_", start_year, "_", end_year, ".jpeg")

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(position = "identity", fill = "red") + 
  scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle("台灣MSCI指數回檔") +
  labs(x = "日期" , y = "價值損失百分比")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now bring in S&P 500
raw_sp500 <- read.csv(paste0(importdir, "/", folder_name, "/GrowthOfWealth_20251113223116_sp500.csv"),
                      col.names = c("date", "index_sp500"), skip = 6) %>%
  filter(!(date == ""), !(index_sp500 == "")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         index = as.numeric(index_sp500)) %>%
  arrange(date) %>%
  select(date, index)

dd <- drawdown_path(raw_sp500 %>% select(date, index))

start_year <- year(min(dd$date))
end_year <- year(max(dd$date))

#Plot DD for S&P 500 now too
file_path <- paste0(out_path, "/dd_sp500_", start_year, "_", end_year, ".jpeg")

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(position = "identity", fill = "red") + 
  scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle("美國股市回檔") +
  labs(x = "日期" , y = "價值損失百分比")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

######## Define all functions first ########

create_full_dd <- function(dataset, start_date, end_date){
  
  tmp_dataset <- dataset %>%
                  filter(date >= start_date, date <= end_date)
  
  dd <- drawdown_path(tmp_dataset)
  
  # Find bottoms
  for (i in 1:nrow(dd)){
    if(dd[i, "pct"] == 0){
      dd[i, "ath"] <- 1
    } else{
      dd[i, "ath"] <- 0
    }
    
    if(dd[i, "ath"] == 1){
      local_min <- 0
    } else{
      local_min <- min(local_min, dd[i, "pct"])
    }
    
    dd[i, "min_dd"] <- local_min 
  }
  
  # Reverse sort to overwrite bottoms
  for (i in nrow(dd):1){
    if(dd[i, "ath"] == 1){
      local_min <- 0
    } else{
      local_min <- min(local_min, dd[i, "min_dd"])
    }
    
    dd[i, "min_dd"] <- local_min 
  }
  
  return(dd)
}

# Do a full DCA vs. Buy the Dip simulation
full_dca_dip <- function(dataset, n_month_delay, start_date, end_date){
  
  tmp_dataset  <-  dataset %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(ret = index/lag(index) - 1) %>%
    filter(!is.na(ret)) %>%
    select(date, index, ret)
  
  purchase_dates <- full_dd %>%
    filter(pct == min_dd, min_dd < 0) %>%
    mutate(date = date + months(n_month_delay + 1),
           bottom = 1) %>%
    select(date, bottom)
  
  df <- tmp_dataset %>%
    left_join(select(full_dd, date, pct, ath)) %>%
    left_join(purchase_dates) %>%
    mutate(bottom = ifelse(is.na(bottom), 0, 1))
  
  for(i in 1:nrow(df)){
    if (i == 1){
      df[i, "dca_value"] <- monthly_buy * (1 + df[i, "ret"])
      df[i, "dip_vested"] <- monthly_buy * (1 + df[i, "ret"])
      df[i, "dip_cash"] <- 0
    } else{
      df[i, "dca_value"] <- (df[(i-1), "dca_value"] + monthly_buy) * (1 + df[i, "ret"])
      
      if(df[i, "bottom"] == 0){
        df[i, "dip_vested"] <- df[(i-1), "dip_vested"] * (1 + df[i, "ret"])
        df[i, "dip_cash"] <- df[(i-1), "dip_cash"] + monthly_buy
        
      } else{
        df[i, "dip_vested"] <- (df[(i-1), "dip_vested"] + df[(i-1), "dip_cash"] + monthly_buy) * (1 + df[i, "ret"])
        df[i, "dip_cash"] <- 0
      }
    }
    
    if (i == nrow(df)){
      df[i, "dip_vested"] <- df[i, "dip_vested"] + df[i, "dip_cash"]
      df[i, "dip_cash"] <- 0
    }
    
    df[i, "dip_value"] <- df[i, "dip_vested"] + df[i, "dip_cash"]
  }
  
  first <- df[1, "index"]
  
  df <- df %>%
    mutate(lump_sum = index/first)
  
  return(df)
}

# Do more efficient simulation of bottom buying strategy
calculate_dca_dip_diff <- function(dataset, n_month_delay, start_date, end_date){
  
  tmp_dataset    <- dataset %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(lag_price = lag(index)) %>%
    filter(!is.na(lag_price))
  
  tmp_dd <- create_full_dd(dataset, start_date, end_date)
  
  purchase_dates <- tmp_dd %>%
    filter(pct == min_dd, min_dd < 0) %>%
    mutate(date = date + months(n_month_delay + 1)) %>%
    filter(date >= as.Date(start_date), 
           date <= as.Date(end_date)) %>%
    mutate(dip_amount = (interval(lag(date), date) %/% months(1)) * monthly_buy) %>%
    select(date, dip_amount)
  
    # Find first purchase amount
    purchase_dates[1, "dip_amount"] <- (interval(start_date, purchase_dates[1, "date"]) %/% months(1) + 1) * monthly_buy
    
    # Find the last purchase date
    last_purchase_date <- purchase_dates[nrow(purchase_dates), "date"]
  
  end <- tmp_dataset[nrow(tmp_dataset), "index"]
  
  n_periods <- nrow(tmp_dataset)
  
  df <- tmp_dataset %>%
    mutate(period_ret = ((end/lag_price)^(12/(n_periods - row_number() + 1))) - 1,
           dca_growth = (end/lag_price)*monthly_buy) %>%
    left_join(purchase_dates) %>%
    mutate(dip_amount = ifelse(is.na(dip_amount), 0, dip_amount),
           dip_growth = (end/lag_price)*dip_amount) %>%
    select(date, index, lag_price, period_ret, dca_growth,
           dip_amount, dip_growth)
  
  # Add last cash and "growth" for any remaining cash balances
  if(df[nrow(df), "date"] != last_purchase_date){
    df[nrow(df), "dip_amount"] <- (interval(last_purchase_date, end_date) %/% months(1)) * monthly_buy
    df[nrow(df), "dip_growth"] <- (interval(last_purchase_date, end_date) %/% months(1)) * monthly_buy
  }
  
  df <- df %>%
    mutate(cumulative_dca = cumsum(dca_growth),
           cumulative_dip = cumsum(dip_growth))
  
  return(df)
}

# Plot ATHs and Dips
plot_ath_dips <- function(dataset, start_date, end_date){
  
  # Calculate data
  full_df <- full_dca_dip(dataset, 0, start_date, end_date)
  
  to_plot <- full_df %>%
    select(date, lump_sum) %>%
    gather(key=key, value=value, -date)
  
  ath <- full_df %>%
    filter(ath == 1) %>%
    select(date, lump_sum) %>%
    gather(key=key, value=value, -date)
  
  bottom <- full_df %>%
    mutate(bottom = ifelse(lead(bottom) == 1, 1, 0)) %>%
    filter(bottom == 1) %>%
    select(date, lump_sum) %>%
    gather(key=key, value=value, -date)
  
  # Set general parameters before plotting
  start_date_string <- str_replace_all(paste0(start_date), "-", "_")
  
  #Create directory
  dir.create(file.path(paste0(out_path, "/", start_date_string)), showWarnings = FALSE)
  
  file_path <- paste0(out_path, "/", start_date_string, "/ath_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_line() +
    geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle("All-Time Highs") +
    labs(x = "Date", y = "Growth of $1")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # New plot
  file_path <- paste0(out_path, "/", start_date_string,"/ath_dip_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_line() +
    geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle("歷史新高與回落") +
    labs(x = "日期", y = "新台幣1元的成長")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Plot DCA v Cash
plot_dca_v_cash <- function(dataset, lag_length, start_date, end_date, text_date, cash_nudge, invest_nudge,
                            dca_nudge, btd_nudge){
  
  if(lag_length == 2){
    end_title <- paste0(" (延遲兩個月)")
  } else{
    end_title <- ""
  }
  
  to_plot <- full_dca_dip(dataset, lag_length, start_date, end_date) %>%
    select(date, dip_vested) %>%
    rename(`投資金額` = dip_vested) %>%
    gather(-date, key=key, value=value)
  
  cash <- full_dca_dip(dataset, lag_length, start_date, end_date) %>%
    select(date, dip_cash) %>%
    rename(`現金` = dip_cash) %>%
    gather(-date, key=key, value=value)
  
  bottom <- full_dca_dip(dataset, lag_length, start_date, end_date) %>%
    mutate(bottom = ifelse(lead(bottom) == 1, 1, 0)) %>%
    filter(bottom == 1) %>%
    select(date, dip_vested) %>%
    gather(key=key, value=value, -date)
  
  text_labels <- to_plot %>%
    bind_rows(cash) %>%
    filter(date == text_date)
  
  start_date_string <- str_replace_all(paste0(start_date), "-", "_")
  
  #Create directory
  dir.create(file.path(paste0(out_path, "/", start_date_string)), showWarnings = FALSE)
  
  # New plot
  file_path <- paste0(out_path, "/", start_date_string, "/dip_cash_lag_", lag_length, "_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_bar(data=cash, aes(x=date, y=value), fill = "green", col = "green", stat="identity") +
    geom_line(col = "black") +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("black", "green"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    segment.color = "transparent", 
                    nudge_y = ifelse(text_labels$key == "現金", cash_nudge, invest_nudge),
                    max.iter = 3000) +
    of_dollars_and_data_theme +
    ggtitle(paste0("逢低買進", end_title)) +
    labs(x = "日期", y = "金額")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Second plot
  to_plot <- full_dca_dip(dataset, lag_length, start_date, end_date) %>%
    select(date, dip_value, dca_value) %>%
    rename(`持續買進` = dca_value,
           `逢低買進` = dip_value) %>%
    gather(-date, key=key, value=value)
  
  assign("to_plot_tmp", to_plot, envir = .GlobalEnv)
  
  text_labels <- to_plot %>%
    filter(date == text_date)
  
  bottom <- full_dca_dip(dataset, lag_length, start_date, end_date) %>%
    mutate(bottom = ifelse(lead(bottom) == 1, 1, 0)) %>%
    filter(bottom == 1) %>%
    select(date, dip_value) %>%
    gather(key=key, value=value, -date)
  
  # New plot
  file_path <- paste0(out_path, "/", start_date_string, "/dca_vs_dip_lag_", lag_length, "_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("black", "#3182bd"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    nudge_y = ifelse(text_labels$key == "持續買進", dca_nudge, btd_nudge),
                    segment.color = "transparent") +
    of_dollars_and_data_theme +
    ggtitle(paste0("逢低買進 vs. 持續買進\n", end_title)) +
    labs(x = "日期", y = "金額")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Pull cumulative dollar growth
  cumulative_totals <- calculate_dca_dip_diff(dataset, lag_length, start_date, end_date)

  # DCA growth only
  to_plot <- cumulative_totals %>%
    select(date, dca_growth) %>%
    gather(-date, key=key, value=value)
  
  bottom <- cumulative_totals %>%
    filter(dip_amount != 0, row_number() != 1) %>%
    select(date, dca_growth) %>%
    gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/", start_date_string, "/dca_final_growth_btd_", lag_length, "_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_bar(stat="identity", position="dodge", col = "black", fill = "black") +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", alpha = 0.7)+
    geom_hline(yintercept = monthly_buy, linetype = "dashed", col="red") +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("#3182bd", "black"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Final Growth of Each DCA Payment\nAnd Buy the Dip Purchases")) +
    labs(x = "Date", y = "Amount")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do a separate one without the dips
  file_path <- paste0(out_path, "/", start_date_string, "/dca_final_growth_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_bar(stat="identity", position="dodge", col = "black", fill = "black") +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("#3182bd", "black"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Final Growth of Each DCA Payment")) +
    labs(x = "Date", y = "Amount")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

###### End function definition ############

# Define simulation parameters
monthly_buy <- 10000
analysis_start <- as.Date("1988-01-01")
analysis_end <- as.Date("2024-12-01")

# Parameters for all plots
dot_size <- 1.2

# Create full_dd dataset for use in other functions
full_dd <- create_full_dd(raw_msci, analysis_start, analysis_end)

# Do all plotting
plot_ath_dips(raw_msci, "2015-01-01", "2024-12-01")
plot_dca_v_cash(raw_msci, 0, "2015-01-01", "2024-12-01", "2022-01-01", 400000, 500000, -900000, 500000)
plot_dca_v_cash(raw_msci, 2, "2015-01-01", "2024-12-01", "2022-01-01", 400000, 500000, 500000, -900000)


run_all_years <- 1
n_years <- 10

if (run_all_years == 1){
  # Define final results data frame
  final_results <- data.frame()
  
  # Define list of dates
  all_dates <- seq.Date(analysis_start, analysis_end - years(n_years) + months(1), "year")
  
  # Loop through all dates to run DCA vs. Buy the Dip Strategy comparisons
  counter <- 1
  for (d in 1:length(all_dates)){
    
    st <- all_dates[d]
    print(st)
    
    final_results[counter, "start_date"] <- format.Date(st)
    final_results[counter, "end_date"] <- format.Date(st + years(n_years) - months(1))
    final_results[counter, "n_years"] <- n_years
    
    for(i in c(0, 2)){
      tmp <- calculate_dca_dip_diff(raw_msci, i, st, st + years(n_years) - months(1))
      
      dip_diff_name <- paste0("dip_pct_gt_lag_", i)
      dip_win_name <- paste0("dip_win_lag_", i)
      dca_name <- paste0("dca_final_amount_lag_", i)
      
      final_results[counter, dip_diff_name] <- sum(tmp$dip_growth)/sum(tmp$dca_growth) - 1
      final_results[counter, dip_win_name] <- ifelse(final_results[counter, dip_diff_name] > 0, 1, 0)
      final_results[counter, dca_name] <- sum(tmp$dca_growth)
    }
    
    counter <- counter + 1
  }
  
  # Plot the DCA outperformance by year
  to_plot <- final_results %>%
    select(start_date, dip_pct_gt_lag_2) %>%
    mutate(date = as.Date(start_date)) %>%
    select(-start_date) %>%
    rename(`Lag2` = dip_pct_gt_lag_2) %>%
    gather(-date, key=key, value=value) %>%
    mutate(key = factor(key, levels = c("Lag2")))
  
  file_path <- paste0(out_path, "/dip_buying_outperformance_lag2.jpeg")
  
  text_labels <- data.frame(date = c(as.Date("2008-01-01"), as.Date("2008-01-01")),
                            value = c(0.15, -0.15),
                            label = c("逢低買進表現較佳", "逢低買進表現較差"))
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text_repel(data=text_labels, aes(x=date, y=value), 
                    label = text_labels$label, 
                    col="black", 
                    family = "my_font",
                    max.iter = 1) +
    scale_y_continuous(label = percent) +
    scale_color_manual(values = c("#3182bd"), guide = FALSE) +
    scale_x_date(date_labels = "%Y",
                 breaks = c(
                   as.Date("1990-01-01"),
                   as.Date("2000-01-01"),
                   as.Date("2010-01-01")
                 )) +
    of_dollars_and_data_theme +
    ggtitle(paste0("逢低買進 vs. 持續買進 (延遲兩個月)\n所有", n_years, "年期間")) +
    labs(x = "日期", y = "逢低買進超額報酬率（%）")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Print summary stats
  print(mean(final_results$dip_win_lag_0))
  print(mean(final_results$dip_win_lag_2))
  
  print(mean(final_results$dip_pct_gt_lag_0))
  print(mean(final_results$dip_pct_gt_lag_2))
}

# ############################  End  ################################## #