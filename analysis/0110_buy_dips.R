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

folder_name <- "0110_buy_dips"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

######## Define all functions first ########

create_full_dd <- function(start_date, end_date){
  # Read in data for individual stocks and sp500 Shiller data
  sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(ret = price_plus_div/lag(price_plus_div) - 1) %>%
    filter(!is.na(ret)) %>%
    select(date, price_plus_div, ret)
  
  dd <- drawdown_path(select(sp500_ret_pe, date, price_plus_div))
  
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
full_dca_dip <- function(n_month_delay, start_date, end_date){

  sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(ret = price_plus_div/lag(price_plus_div) - 1) %>%
    filter(!is.na(ret)) %>%
    select(date, price_plus_div, ret)
  
  purchase_dates <- full_dd %>%
                      filter(pct == min_dd, min_dd < 0) %>%
                      mutate(date = date + months(n_month_delay + 1),
                             bottom = 1) %>%
                      select(date, bottom)
  
  df <- sp500_ret_pe %>%
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
  
  first <- pull(df[1, "price_plus_div"])
  
  df <- df %>%
          mutate(lump_sum = price_plus_div/first)
  
  return(df)
}

# Do more efficient simulation of bottom buying strategy
calculate_dca_dip_diff <- function(n_month_delay, start_date, end_date){
  sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(lag_price = lag(price_plus_div)) %>%
    filter(!is.na(lag_price))
  
  purchase_dates <- full_dd %>%
    filter(pct == min_dd, min_dd < 0, 
           date >= as.Date(start_date) - months(1) + months(n_month_delay), 
           date <= as.Date(end_date) + months(n_month_delay)
    ) %>%
    mutate(date = date + months(n_month_delay + 1)) %>%
    mutate(dip_amount = (interval(lag(date), date) %/% months(1)) * monthly_buy) %>%
    select(date, dip_amount)
  
  # Find first purchase amount
  purchase_dates[1, "dip_amount"] <- (interval(start_date, purchase_dates[1, "date"]) %/% months(1) + 1) * monthly_buy
  
  # Find the last purchase date
  last_purchase_date <- purchase_dates[nrow(purchase_dates), "date"]
  
  end <- pull(sp500_ret_pe[nrow(sp500_ret_pe), "price_plus_div"])
  
  n_periods <- nrow(sp500_ret_pe)
  
  df <- sp500_ret_pe %>%
    mutate(period_ret = ((end/lag_price)^(12/(n_periods - row_number() + 1))) - 1,
           dca_growth = (end/lag_price)*monthly_buy) %>%
    left_join(purchase_dates) %>%
    mutate(dip_amount = ifelse(is.na(dip_amount), 0, dip_amount),
           dip_growth = (end/lag_price)*dip_amount) %>%
    select(date, price_plus_div, lag_price, period_ret, dca_growth,
           dip_amount, dip_growth)
  
  # Add last cash and "growth" for any remaining cash balances
  if(pull(df[nrow(df), "date"]) != last_purchase_date){
    df[nrow(df), "dip_amount"] <- (interval(last_purchase_date, end_date) %/% months(1)) * monthly_buy
    df[nrow(df), "dip_growth"] <- (interval(last_purchase_date, end_date) %/% months(1)) * monthly_buy
  }
  
  df <- df %>%
    mutate(cumulative_dca = cumsum(dca_growth),
           cumulative_dip = cumsum(dip_growth))
  
  return(df)
}

# Plot ATHs and Dips
plot_ath_dips <- function(lag_length, start_date, end_date){

  # Calculate data
  full_df <- full_dca_dip(0, start_date, end_date)
  
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
  note_string <- paste0("Note:  Real return includes reinvested dividends.") 

  start_date_string <- str_replace_all(paste0(start_date), "-", "_")
  
  #Create directory
  dir.create(file.path(paste0(out_path, "/", start_date_string)), showWarnings = FALSE)
  
  file_path <- paste0(out_path, "/", start_date_string, "/ath_sp500_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_line() +
    geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle("All-Time Highs for the S&P 500") +
    labs(x = "Date", y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # New plot
  file_path <- paste0(out_path, "/", start_date_string,"/ath_dip_sp500_", start_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_line() +
    geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle("All-Time Highs & 'Dips'\nfor the S&P 500") +
    labs(x = "Date", y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Plot DCA v Cash
plot_dca_v_cash <- function(lag_length, start_date, end_date, text_date){
  
  if(lag_length > 0){
    end_title <- paste0(" (With ", lag_length, "-Month Lag)")
  } else{
    end_title <- ""
  }
  
  to_plot <- full_dca_dip(lag_length, start_date, end_date) %>%
              select(date, dip_vested) %>%
              rename(`Invested\nAmount` = dip_vested) %>%
              gather(-date, key=key, value=value)
  
  cash <- full_dca_dip(lag_length, start_date, end_date) %>%
            select(date, dip_cash) %>%
            rename(`Cash` = dip_cash) %>%
            gather(-date, key=key, value=value)
  
  bottom <- full_dca_dip(lag_length, start_date, end_date) %>%
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
  
  note_string <- str_wrap(paste0("Note: The Buy the Dip strategy accumulates cash and buys at 'dips' in the S&P 500.  ",
                                 "Real return includes reinvested dividends."), 
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_bar(data=cash, aes(x=date, y=value), col = "green", stat="identity") +
    geom_line(col = "black") +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("green", "black"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    segment.color = "transparent", 
                    nudge_y = ifelse(text_labels$key == "Cash", 10000, 15000),
                    max.iter = 3000) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Buy the Dip", end_title)) +
    labs(x = "Date", y = "Amount",
         caption = paste0(source_string, "\n", note_string))

  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Second plot
  to_plot <- full_dca_dip(lag_length, start_date, end_date) %>%
                select(date, dip_value, dca_value) %>%
                rename(`DCA` = dca_value,
                       `Buy the Dip` = dip_value) %>%
                gather(-date, key=key, value=value)
  
  text_labels <- to_plot %>%
                  filter(date == text_date)
  
  bottom <- full_dca_dip(lag_length, start_date, end_date) %>%
              mutate(bottom = ifelse(lead(bottom) == 1, 1, 0)) %>%
              filter(bottom == 1) %>%
              select(date, dip_value) %>%
              gather(key=key, value=value, -date)
  
  # New plot
  file_path <- paste0(out_path, "/", start_date_string, "/dca_vs_dip_lag_", lag_length, "_", start_date_string, ".jpeg")
  
  note_string <- str_wrap(paste0("Note: The Buy the Dip strategy accumulates cash and buys at 'dips' in the S&P 500.  ",
                                 "Real return includes reinvested dividends."), 
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("#3182bd", "black"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    nudge_y = ifelse(text_labels$key == "DCA", -15000, 15000),
                    segment.color = "transparent") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Buy the Dip", end_title, "\nvs. DCA")) +
    labs(x = "Date", y = "Amount",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Pull cumulative dollar growth
  cumulative_totals <- calculate_dca_dip_diff(lag_length, start_date, end_date)
  
  to_plot <- cumulative_totals %>%
                select(date, cumulative_dca, cumulative_dip) %>%
                rename(`DCA` = cumulative_dca,
                       `Buy the Dip` = cumulative_dip) %>%
                gather(-date, key=key, value=value)
  
  text_labels <- to_plot %>%
                  filter(date == text_date)
  
  # Plot Cumululative growth
  file_path <- paste0(out_path, "/", start_date_string, "/cumulative_growth_lag_", lag_length, "_", start_date_string, ".jpeg")
  
  note_string <- str_wrap(paste0("Note: The Buy the Dip strategy accumulates cash and buys at 'dips' in the S&P 500.  ",
                                 "Real return includes reinvested dividends."), 
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("#3182bd", "black"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    nudge_y = ifelse(text_labels$key == "DCA", -15000, 1000),
                    segment.color = "transparent") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Cumulative Growth of DCA and\nBuy the Dip", end_title)) +
    labs(x = "Date", y = "Amount",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # DCA growth only
  to_plot <- cumulative_totals %>%
              select(date, dca_growth) %>%
              gather(-date, key=key, value=value)
  
  bottom <- cumulative_totals %>%
              filter(dip_amount != 0) %>%
              select(date, dca_growth) %>%
              gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/", start_date_string, "/dca_final_growth_lag_", lag_length, "_", start_date_string, ".jpeg")
  
  note_string <- str_wrap(paste0("Note: Real return includes reinvested dividends.  ",  
                                 "Assumes a monthly payment of $", formatC(monthly_buy, digits=0, big.mark = ",", format = "f"),".  ",
                                 "Red dots represent when the Buy the Dip strategy makes purchases."), 
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_bar(stat="identity", position="dodge", col = "black") +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", alpha = 0.7)+
    geom_hline(yintercept = monthly_buy, linetype = "dashed", col="red") +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("#3182bd", "black"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Final Growth of Each DCA Payment\nAnd Buy the Dip Purchases")) +
    labs(x = "Date", y = "Amount",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

###### End function definition ############

# Define simulation parameters
monthly_buy <- 100
n_years <- 40
analysis_start <- as.Date("1920-01-01")
analysis_end <- as.Date("2018-12-01")

# Parameters for all plots
source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
dot_size <- 1.2

# Create full_dd dataset for use in other functions
full_dd <- create_full_dd(analysis_start, analysis_end)

testing <- 1

if(testing == 1){
  test_date <- "1932-07-01"
  test_end <- ""
  if(test_end == ""){
    t <- calculate_dca_dip_diff(0, test_date, as.Date(test_date) + years(n_years) - months(1))
    t_full <- full_dca_dip(0, test_date, as.Date(test_date) + years(n_years) - months(1))
  } else{
    t <- calculate_dca_dip_diff(0, test_date, test_end)
    t_full <- full_dca_dip(0, test_date, test_end)
  }
}

# Do all plotting
plot_ath_dips(0, "1995-01-01", "2018-12-01")
plot_dca_v_cash(0, "1995-01-01", "2018-12-01", "2014-12-01")
plot_dca_v_cash(2, "1995-01-01", "2018-12-01", "2014-12-01")

dt1 <- "1928-01-01"
plot_ath_dips(0, dt1, as.Date(dt1) + years(40) - months(1))
plot_dca_v_cash(0, dt1, as.Date(dt1) + years(40) - months(1), "1940-01-01")
plot_dca_v_cash(2, dt1, as.Date(dt1) + years(40) - months(1), "1940-01-01")

dt2 <- "1975-01-01"
plot_ath_dips(0, dt2, as.Date(dt2) + years(40) - months(1))
plot_dca_v_cash(0, dt2 , as.Date(dt2) + years(40) - months(1), "1980-01-01")

run_all_years <- 1

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
    
    for(i in 0:2){
      tmp <- calculate_dca_dip_diff(i, st, st + years(n_years) - months(1))
      
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
              select(start_date, dip_pct_gt_lag_0) %>%
              mutate(date = as.Date(start_date)) %>%
              select(-start_date) %>%
              rename(`No Lag` = dip_pct_gt_lag_0) %>%
              gather(-date, key=key, value=value) %>%
              mutate(key = factor(key, levels = c("No Lag")))
  
  file_path <- paste0(out_path, "/dip_buying_outperformance.jpeg")
  note_string <- str_wrap(paste0("Note:  The DCA strategy buys the S&P 500 every month and stays fully invested.  ",
                                 "The Buy the Dip strategy accumulates cash and buys at 'dips' in the S&P 500.  ",
                                 "The outperformance percentage is defined as how much more (or less) money that the Buy the Dip strategy has compared to",
                                  " the DCA strategy in the final period."), 
                          width = 85)
  
  text_labels <- data.frame(date = c(as.Date("1950-01-01"), as.Date("1950-01-01")),
                            value = c(0.15, -0.15),
                            label = c("Buy the Dip Outperforms", "Buy the Dip Underperforms"))
  
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
                   as.Date("1920-01-01"),
                   as.Date("1930-01-01"),
                   as.Date("1940-01-01"),
                   as.Date("1950-01-01"),
                   as.Date("1960-01-01"),
                   as.Date("1970-01-01"),
                   as.Date("1980-01-01")
                 )) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Buy the Dip vs. DCA\nAll ", n_years, "-Year Periods")) +
    labs(x = "Date", y = "Buy the Dip Outperformance (%)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Print summary stats
  print(mean(final_results$dip_win_lag_0))
  print(mean(final_results$dip_win_lag_1))
  print(mean(final_results$dip_win_lag_2))
  
  print(mean(final_results$dip_pct_gt_lag_0))
  print(mean(final_results$dip_pct_gt_lag_1))
  print(mean(final_results$dip_pct_gt_lag_2))
}

# ############################  End  ################################## #