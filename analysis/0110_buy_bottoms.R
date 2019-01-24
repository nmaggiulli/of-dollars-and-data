cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "0110_buy_bottoms"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

monthly_buy <- 100

buy_bottoms_dca <- function(n_month_delay, start_date, end_date){

  # Read in data for individual stocks and sp500 Shiller data
  sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date)) %>%
    mutate(ret = price_plus_div/lag(price_plus_div) - 1) %>%
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
                      mutate(date = date + months(n_month_delay),
                             bottom = 1) %>%
                      select(date, bottom)
  
  df <- sp500_ret_pe %>%
          left_join(select(dd, date, pct, ath)) %>%
          left_join(purchase_dates) %>%
          mutate(bottom = ifelse(is.na(bottom), 0, 1))
  
  for(i in 1:nrow(df)){
    if (i == 1){
      df[i, "dca_value"] <- monthly_buy
      df[i, "bottom_vested"] <- monthly_buy
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

start_date <- '1979-01-01'
end_date <- '2018-12-01'

# Calculate data
full_df <- buy_bottoms_dca(0, start_date, end_date)

to_plot <- full_df %>%
            select(date, lump_sum) %>%
            gather(key=key, value=value, -date)

ath <- full_df %>%
      filter(ath == 1) %>%
      select(date, lump_sum) %>%
      gather(key=key, value=value, -date)

bottom <- full_df %>%
          filter(bottom == 1) %>%
          select(date, lump_sum) %>%
          gather(key=key, value=value, -date)

# Set general parameters before plotting
source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
note_string <- paste0("Note:  Real return includes reinvested dividends.") 

dot_size <- 1.5

file_path <- paste0(out_path, "/ath_sp500.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value)) +
  geom_line() +
  geom_point(data=ath, aes(x=date, y=value), col = "green", size = dot_size, alpha = 0.7) +
  scale_y_continuous(label = dollar) +
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
  of_dollars_and_data_theme +
  ggtitle("All-Time Highs & Relative Bottoms\nfor the S&P 500") +
  labs(x = "Date", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- full_df %>%
            select(date, dca_value, bottom_value) %>%
            rename(DCA = dca_value,
                   `Bottom\nBuying` = bottom_value) %>%
            gather(-date, key=key, value=value)

text_labels <- to_plot %>%
                filter(date == "2014-12-01")

# New plot
file_path <- paste0(out_path, "/dca_vs_bottom_lag_0.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                  label = text_labels$key,
                  nudge_y = ifelse(text_labels$key == "DCA", -9000, 9000),
                  segment.color = "transparent") +
  of_dollars_and_data_theme +
  ggtitle("DCA vs. Bottom Buying") +
  labs(x = "Date", y = "Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- buy_bottoms_dca(2, start_date, end_date) %>%
  select(date, dca_value, bottom_value) %>%
  rename(DCA = dca_value,
         `Bottom\nBuying` = bottom_value) %>%
  gather(-date, key=key, value=value)

text_labels <- to_plot %>%
  filter(date == "2014-12-01")

# New plot
file_path <- paste0(out_path, "/dca_vs_bottom_lag_2.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                  label = text_labels$key,
                  nudge_y = ifelse(text_labels$key == "DCA", -9000, 9000),
                  segment.color = "transparent") +
  of_dollars_and_data_theme +
  ggtitle("DCA vs. Bottom Buying (with 2-Month Lag)") +
  labs(x = "Date", y = "Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #