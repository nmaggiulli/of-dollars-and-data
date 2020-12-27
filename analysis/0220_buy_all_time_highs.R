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

folder_name <- "0220_buy_all_time_highs"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

read_ycharts <- function(symbol){
  raw <- read.csv(paste0(importdir, "/0220_ycharts_stock_indices/", symbol, "_data.csv"),
                  skip = 1, col.names = c("date", "index")) %>%
          mutate(date = as.Date(date)) %>%
          arrange(date) 
  
  limit_1 <- -0.05
  
  dd <- drawdown_path(raw) %>%
    mutate(dd_group = case_when(
      pct >= limit_1 ~ "Near ATH",
      TRUE ~ "Off ATH"
    ))
  
  trading_days_1yr <- 250
  trading_days_3yr <- trading_days_1yr*3
  trading_days_5yr <- trading_days_1yr*5
  
  first_index <- raw[1, "index"]
  
  raw <- raw %>%
          left_join(dd) %>%
          mutate(symbol = symbol,
                 fwd_ret_1yr = lead(index, trading_days_1yr)/index - 1,
                 fwd_ret_3yr = (lead(index, trading_days_3yr)/index)^(1/3) - 1,
                 fwd_ret_5yr = (lead(index, trading_days_5yr)/index)^(1/5) - 1,
                 fwd_dd_1yr = lead(pct, trading_days_1yr),
                 fwd_dd_3yr = lead(pct, trading_days_3yr),
                 fwd_dd_5yr = lead(pct, trading_days_5yr),
                 index = index/first_index
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

stack <- spx %>%
          bind_rows(eafe, em)

stack_counts <- stack %>%
                  group_by(symbol) %>%
                  summarize(total_obs = n()) %>%
                  ungroup()

stats_by_dd_group <- stack %>%
                      group_by(symbol, dd_group) %>%
                      summarize(n_obs = n(),
                                mean_fwd_ret_1yr = mean(fwd_ret_1yr, na.rm= TRUE),
                                mean_fwd_ret_3yr = mean(fwd_ret_3yr, na.rm= TRUE),
                                mean_fwd_ret_5yr = mean(fwd_ret_5yr, na.rm= TRUE)
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
  
  file_path <- paste0(out_path, "/fwd_ret_", sym, "_", num, "yr_.jpeg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  
  plot <- ggplot(to_plot, aes(x = fwd_ret, fill = dd_group)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("red", "blue")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("Forward 1-Year Returns Based on ATH Status\n", name)) +
    labs(x = "Forward Return" , y = paste0("Frequency"),
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do ATH plot
  if(num == 1){
    file_path <- paste0(out_path, "/days_btwn_ath_", sym, ".jpeg")
    source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
    
    plot <- ggplot(to_plot, aes(x = date, y = days_between_ath)) +
      geom_area(fill = chart_standard_color) +
      scale_y_continuous(label = comma) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Days Between All-Time Highs\n", name)) +
      labs(x = "Date", y = "Number of Trading Days From Previous Peak",
           caption = paste0(source_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

yrs <- c(1, 3, 5)

for(y in yrs){
  plot_symbol_ret("SPX", "S&P 500", y)
  plot_symbol_ret("MSEAFE", "EAFE", y)
  plot_symbol_ret("MSEM", "Emerging Markets", y)
}



# ############################  End  ################################## #