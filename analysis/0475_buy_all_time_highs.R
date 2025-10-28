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
library(quantmod)
library(FinCal)
library(tidyverse)

folder_name <- "0475_buy_all_time_highs"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

## Set DD ATH limit
limit_1 <- -0.05

getSymbols("GC%3DF", from = as.Date("2025-03-14"), to = Sys.Date()-1,
           src="yahoo", periodicity = "daily")

gld_latest <- data.frame(date=index(get("GC%3DF")), coredata(get("GC%3DF"))) %>%
  rename(index = `GC.3DF.Adjusted`) %>%
  select(date, index)

read_ycharts <- function(symbol){
  raw <- read.csv(paste0(importdir, "/", folder_name, "/", symbol, "_data.csv"),
                  skip = 1, col.names = c("date", "index")) %>%
          mutate(date = as.Date(date)) %>%
          arrange(date) 
  
  if(symbol == "GOLD"){
    raw <- raw %>%
            bind_rows(gld_latest)
    
    raw$index <- na.locf(raw$index)
  }
  
  dd <- drawdown_path(raw) %>%
    mutate(dd_group = case_when(
      pct >= limit_1 ~ "Near ATH",
      TRUE ~ "Off ATH"
    ))
  
  trading_days_1yr <- 250
  trading_days_3yr <- trading_days_1yr*3
  
  raw <- raw %>%
          left_join(dd) %>%
          mutate(symbol = symbol,
                 fwd_ret_1yr = lead(index, trading_days_1yr)/index - 1,
                 fwd_ret_3yr = (lead(index, trading_days_3yr)/index)^(1/3) - 1
                 )
  
  raw$dd_group <- factor(raw$dd_group, levels = c("Near ATH", "Off ATH"))
  
  
  for(i in 1:nrow(raw)){
    if(i == 1){
      raw[i, "days_between_ath"] <- 0
    } else{
      if(raw[i, "pct"] == 0){
        raw[i, "days_between_ath"] <- 0
      } else{
        raw[i, "days_between_ath"] <- raw[(i-1), "days_between_ath"] + 1
      }
    }
  }
  return(raw)
}

spx <- read_ycharts("SPX")
eafe <- read_ycharts("MSEAFE")
em <- read_ycharts("MSEM")
btc <- read_ycharts("BTC")
gold <- read_ycharts("GOLD")

stack <- spx %>%
          bind_rows(eafe, em, btc, gold)

stack_counts <- stack %>%
                  group_by(symbol) %>%
                  summarise(total_obs = n()) %>%
                  ungroup()

stats_by_dd_group <- stack %>%
                      group_by(symbol, dd_group) %>%
                      summarise(n_obs = n(),
                                mean_fwd_ret_1yr = mean(fwd_ret_1yr, na.rm= TRUE),
                                mean_fwd_ret_3yr = mean(fwd_ret_3yr, na.rm= TRUE)
                                ) %>%
                      ungroup() %>%
                      left_join(stack_counts) %>%
                      mutate(dd_group_pct = round(100*n_obs/total_obs, 2)) %>%
                      select(symbol, dd_group, dd_group_pct, contains("mean_"))

plot_symbol_ret <- function(sym, name, num){
  ret_col <- paste0("fwd_ret_", num, "yr")
  
  to_plot <- stack %>%
              filter(symbol == sym) %>%
              rename_(.dots = setNames(paste0(ret_col), "fwd_ret"))
  
  end_year <- year(max(stack$date))
  
  avg_near <- to_plot %>%
                filter(dd_group == "Near ATH") %>%
                summarise(ret = mean(fwd_ret, na.rm = TRUE)) %>%
                pull(ret)
  
  avg_off <- to_plot %>%
                filter(dd_group == "Off ATH") %>%
                summarise(ret = mean(fwd_ret, na.rm = TRUE)) %>%
                pull(ret)
  
  file_path <- paste0(out_path, "/fwd_ret_", sym, "_", num, "yr_", end_year, ".jpeg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: 'Near ATH' is anytime the index is less than ", 
                                 -100*limit_1,
                                 "% from its all-time high.  ",
                                 "'Off ATH' is anytime the index is more than ", 
                                 -100*limit_1, 
                                 "% from its all-time high.  ",
                                 "The average annualized return when 'Near ATH' is ", 
                                 round(100*avg_near, 1), 
                                 "% compared to ",
                                 round(100*avg_off, 1),
                                 "% when 'Off ATH'."),
                          width = 85)
  
  if(num == 1){
    year_string <- "Year"
  } else{
    year_string <- paste0(num, " Years")
  }
  
  plot <- ggplot(to_plot, aes(x = fwd_ret, fill = dd_group)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("red", "blue")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("Annualized Return Over Next ", year_string, "\n", name)) +
    labs(x = "Future Annualized Return" , y = paste0("Frequency"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do ATH plot
  if(num == 1){
    file_path <- paste0(out_path, "/days_btwn_ath_", sym, "_", end_year, ".jpeg")
    source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
    
    plot <- ggplot(to_plot, aes(x = date, y = days_between_ath)) +
      geom_area(fill = chart_standard_color) +
      scale_y_continuous(label = comma) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Days Between All-Time Highs\n", name)) +
      labs(x = "Date", y = "Number of Trading Days\nFrom Previous Peak",
           caption = paste0(source_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
    # Plot price with ATH
    file_path <- paste0(out_path, "/price_ath_", sym, "_", end_year, ".jpeg")
    source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
    
    if(sym == "BTC" | sym == "GOLD"){
      plot <- ggplot(to_plot, aes(x = date, y = index)) +
        geom_line(col = "black") +
        geom_point(data=filter(to_plot, pct == 0), aes(x=date, y=index), 
                   col = "red", 
                   size = 1, 
                   alpha = 0.5) +
        scale_y_continuous(label = dollar, trans = log10_trans()) +
        of_dollars_and_data_theme +
        ggtitle(paste0(name, " All-Time Highs")) +
        labs(x = "Date", y = "Price (in USD)",
             caption = paste0(source_string))
    } else{
      plot <- ggplot(to_plot, aes(x = date, y = index)) +
        geom_line(col = "black") +
        geom_point(data=filter(to_plot, pct == 0), aes(x=date, y=index), 
                   col = "red", 
                   size = 1, 
                   alpha = 0.5) +
        scale_y_continuous(label = comma, trans = log10_trans()) +
        of_dollars_and_data_theme +
        ggtitle(paste0(name, " All-Time Highs")) +
        labs(x = "Date", y = "Index Value",
             caption = paste0(source_string))
    }
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

yrs <- c(1, 3)

for(y in yrs){
  plot_symbol_ret("SPX", "S&P 500", y)
  plot_symbol_ret("MSEAFE", "EAFE", y)
  plot_symbol_ret("MSEM", "Emerging Markets", y)
  plot_symbol_ret("BTC", "Bitcoin", y)
  plot_symbol_ret("GOLD", "Gold", y)
}



# ############################  End  ################################## #