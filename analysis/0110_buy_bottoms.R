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

folder_name <- "0110_buy_bottoms"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

monthly_buy <- 100

full_dca_bottom <- function(n_month_delay, start_date, end_date){

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
  
  purchase_dates <- dd %>%
                      filter(pct == min_dd, min_dd < 0) %>%
                      mutate(date = date + months(n_month_delay + 1),
                             bottom = 1) %>%
                      select(date, bottom)
  
  df <- sp500_ret_pe %>%
          left_join(select(dd, date, pct, ath)) %>%
          left_join(purchase_dates) %>%
          mutate(bottom = ifelse(is.na(bottom), 0, 1))
  
  for(i in 1:nrow(df)){
    if (i == 1){
      df[i, "dca_value"] <- monthly_buy * (1 + df[i, "ret"])
      df[i, "bottom_vested"] <- monthly_buy * (1 + df[i, "ret"])
      df[i, "bottom_cash"] <- 0
    } else{
      df[i, "dca_value"] <- (df[(i-1), "dca_value"] + monthly_buy) * (1 + df[i, "ret"])
      
      if(df[i, "bottom"] == 0){
          df[i, "bottom_vested"] <- df[(i-1), "bottom_vested"] * (1 + df[i, "ret"])
          df[i, "bottom_cash"] <- df[(i-1), "bottom_cash"] + monthly_buy
          
      } else{
        df[i, "bottom_vested"] <- (df[(i-1), "bottom_vested"] + df[(i-1), "bottom_cash"] + monthly_buy) * (1 + df[i, "ret"])
        df[i, "bottom_cash"] <- 0
      }
    }
    
    if (i == nrow(df)){
      df[i, "bottom_vested"] <- df[i, "bottom_vested"] + df[i, "bottom_cash"]
      df[i, "bottom_cash"] <- 0
    }
    
    df[i, "bottom_value"] <- df[i, "bottom_vested"] + df[i, "bottom_cash"]
  }
  
  first <- pull(df[1, "price_plus_div"])
  
  df <- df %>%
          mutate(lump_sum = price_plus_div/first)
  
  return(df)
}

start_date <- '1995-01-01'
end_date <- '2018-12-01'

# Calculate data
full_df <- full_dca_bottom(0, start_date, end_date)

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
source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
note_string <- paste0("Note:  Real return includes reinvested dividends.") 

dot_size <- 1.2

file_path <- paste0(out_path, "/ath_sp500.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value)) +
  geom_line() +
  geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%Y", breaks = c(
    as.Date("1995-01-01"),
    as.Date("2000-01-01"),
    as.Date("2005-01-01"),
    as.Date("2010-01-01"),
    as.Date("2015-01-01"),
    as.Date("2020-01-01")
  ),
  limits = c(as.Date("1995-01-01"), as.Date("2020-01-01"))) +
  of_dollars_and_data_theme +
  ggtitle("All-Time Highs for the S&P 500") +
  labs(x = "Date", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# New plot
file_path <- paste0(out_path, "/ath_bottom_sp500.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value)) +
  geom_line() +
  geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
  geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%Y", breaks = c(
    as.Date("1995-01-01"),
    as.Date("2000-01-01"),
    as.Date("2005-01-01"),
    as.Date("2010-01-01"),
    as.Date("2015-01-01"),
    as.Date("2020-01-01")
  ),
  limits = c(as.Date("1995-01-01"), as.Date("2020-01-01"))) +
  of_dollars_and_data_theme +
  ggtitle("All-Time Highs & Relative Bottoms\nfor the S&P 500") +
  labs(x = "Date", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

plot_dca_v_cash <- function(lag_length){
  
  if(lag_length > 0){
    end_title <- paste0(" (With ", lag_length, "-Month Lag)")
  } else{
    end_title <- ""
  }
  
  to_plot <- full_dca_bottom(lag_length, start_date, end_date) %>%
              select(date, bottom_vested, bottom_cash) %>%
              rename(`Invested\nAmount` = bottom_vested,
                     `Cash` = bottom_cash) %>%
              gather(-date, key=key, value=value)
  
  bottom <- full_dca_bottom(lag_length, start_date, end_date) %>%
              mutate(bottom = ifelse(lead(bottom) == 1, 1, 0)) %>%
              filter(bottom == 1) %>%
              select(date, bottom_vested) %>%
              gather(key=key, value=value, -date)
  
  text_labels <- to_plot %>%
                  filter(date == "2014-12-01")
  
  # New plot
  file_path <- paste0(out_path, "/dca_vs_bottom_lag_", lag_length, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_point(data=bottom, aes(x=date, y=value), col = "red", size = dot_size, alpha = 0.7) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("green", "black"), guide = FALSE) +
    geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                    label = text_labels$key,
                    family = "my_font",
                    nudge_y = ifelse(text_labels$key == "DCA", -9000, 9000),
                    segment.color = "transparent") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Bottom-Buying Strategy", end_title)) +
    labs(x = "Date", y = "Amount",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_dca_v_cash(0)

# Do more efficient simulations of bottom buying strategy
calculate_dca_bottom_diff <- function(n_month_delay, start_date, end_date){
  sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date >= as.Date(start_date) - months(1), date <= as.Date(end_date)) %>%
    mutate(lag_price = lag(price_plus_div)) %>%
    filter(!is.na(lag_price))
  
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
  
  purchase_dates <- dd %>%
    filter(pct == min_dd, min_dd < 0) %>%
    mutate(date = date + months(n_month_delay + 1)) %>%
    mutate(bottom_amount = (interval(lag(date), date) %/% months(1)) * monthly_buy) %>%
    select(date, bottom_amount)
  
  # Find first purchase amount
  purchase_dates[1, "bottom_amount"] <- (interval(start_date, purchase_dates[1, "date"]) %/% months(1) + 1) * monthly_buy
  
  # Fix last purchase amount
  last_purchase_date <- purchase_dates[nrow(purchase_dates), "date"]
  
  # Find the number of purchases
  n_purchases <- nrow(purchase_dates)
  
  if(last_purchase_date < end_date){
    purchase_dates[(n_purchases+1), "bottom_amount"] <- (interval(last_purchase_date, end_date) %/% months(1)) * monthly_buy
    purchase_dates[(n_purchases+1), "date"] <- end_date
  } else if(last_purchase_date > end_date){
    last2_purchase_date <- purchase_dates[(nrow(purchase_dates)-1), "date"]
    purchase_dates[n_purchases, "bottom_amount"] <- (interval(last2_purchase_date, end_date) %/% months(1)) * monthly_buy
    purchase_dates[n_purchases, "date"] <- end_date
  }
  
  end <- pull(sp500_ret_pe[nrow(sp500_ret_pe), "price_plus_div"])
  
  n_periods <- nrow(sp500_ret_pe)
  
  df <- sp500_ret_pe %>%
          mutate(period_ret = ((end/lag_price)^(12/(n_periods - row_number() + 1))) - 1,
                 dca_growth = (end/lag_price)*monthly_buy) %>%
          left_join(purchase_dates) %>%
          mutate(bottom_amount = ifelse(is.na(bottom_amount), 0, bottom_amount),
                 bottom_growth = (end/lag_price)*bottom_amount) %>%
          select(date, price_plus_div, lag_price, period_ret, dca_growth,
                 bottom_amount, bottom_growth)
  
  return(df)
}

# Define the number of years to look over for the full-period comparisons
n_years <- 40

testing <- 1
if(testing == 1){
  test_date <- "1960-01-01"
  t <- calculate_dca_bottom_diff(0, test_date, as.Date(test_date) + years(n_years) - months(1))
  t_full <- full_dca_bottom(0, test_date, as.Date(test_date) + years(n_years) - months(1))
  
  to_plot_tmp <- t_full %>% select(date, dca_value, bottom_value) %>% gather(-date, key=key, value=value)
  
  ggplot(to_plot_tmp, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(trans = log10_trans()) +
    of_dollars_and_data_theme
}

# Define final results data frame
final_results <- data.frame()

# Define list of dates
all_dates <- seq.Date(as.Date("1920-01-01"), as.Date("1979-01-01"), "year")

# Loop through all dates to run DCA vs. Bottom-Buying Strategy comparisons
counter <- 1
for (d in 1:length(all_dates)){

  st <- all_dates[d]
  print(st)

  final_results[counter, "date"] <- format.Date(st)
  final_results[counter, "n_years"] <- n_years
  
  for(i in 0:2){
    tmp <- calculate_dca_bottom_diff(i, st, st + years(n_years) - months(1))
    
    dca_diff_name <- paste0("dca_bottom_pct_gt_lag_", i)
    dca_win_name <- paste0("dca_win_lag_", i)
    
    final_results[counter, dca_diff_name] <- sum(tmp$dca_growth)/sum(tmp$bottom_growth) - 1
    final_results[counter, dca_win_name] <- ifelse(final_results[counter, dca_diff_name] > 0, 1, 0)
  }
  
  counter <- counter + 1
}

print(mean(final_results$dca_win_lag_0))
print(mean(final_results$dca_win_lag_1))
print(mean(final_results$dca_win_lag_2))

print(mean(final_results$dca_bottom_pct_gt_lag_0))
print(mean(final_results$dca_bottom_pct_gt_lag_1))
print(mean(final_results$dca_bottom_pct_gt_lag_2))

to_plot <- final_results %>%
            select(date, dca_bottom_pct_gt_lag_0) %>%
            mutate(date = as.Date(date)) %>%
            rename(`No Lag` = dca_bottom_pct_gt_lag_0) %>%
            gather(-date, key=key, value=value) %>%
            mutate(key = factor(key, levels = c("No Lag")))

file_path <- paste0(out_path, "/dca_outperformance.jpeg")
note_string <- str_wrap(paste0("Note:  The DCA strategy buys the S&P 500 every month and stays fully invested.  ",
                               "The Bottom-Buying strategy accumulates cash and buys at relative bottoms in the S&P 500.  ",
                               "The outperformance percentage is defined as how much more (or less) money that the DCA has compared to",
                                " the Bottom-Buying strategy in the terminal period."), 
                        width = 85)

text_labels <- data.frame(date = c(as.Date("1950-01-01"), as.Date("1951-01-01")),
                          value = c(0.1, -0.1),
                          label = c("DCA Outperforms", "DCA Underperforms"))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data=text_labels, aes(x=date, y=value), label = text_labels$label, col="black", family = "my_font") +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = c("#3182bd"), guide = FALSE) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("DCA vs. Bottom-Buying Strategy\nAll ", n_years, "-Year Periods")) +
  labs(x = "Date", y = "DCA Outperformance (%)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #