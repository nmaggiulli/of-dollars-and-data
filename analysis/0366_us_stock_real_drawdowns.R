cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyverse)

folder_name <- "0366_us_stock_real_drawdowns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    select(date, price_plus_div) %>%
                    filter(date >= "1926-01-01")

dd <- drawdown_path(sp500_ret_pe)

dd_counter <- 1
for(i in 1:nrow(dd)){
  if(i == 1){
    dd[i, "dd_counter"] <- dd_counter
    dd[i, "dd_month"] <- 0
  } else{
    if(dd[i, "pct"] == 0){
      dd_counter <- dd_counter + 1
      dd[i, "dd_counter"] <- dd_counter
      dd[i, "dd_month"] <- 0
    } else{
      dd[i, "dd_counter"] <- dd_counter
      dd[i, "dd_month"] <- dd[(i-1), "dd_month"] + 1
    }
  }
}

dd_lengths <- dd %>%
                group_by(dd_counter) %>%
                summarise(dd_start = min(date),
                          dd_end = max(date),
                  n_months = n(),
                          max_dd = min(pct)) %>%
                ungroup()

worst_10_dd<- dd_lengths %>%
                  arrange(desc(n_months)) %>%
                  head(10) %>%
                  inner_join(dd)

plot_dd_comparison<- function(n_months){
  
  n_years <- n_months/12
  
  n_month_filter <- as.numeric(n_months)

  to_plot <- worst_10_dd %>%
                filter(dd_month < n_month_filter) %>%
                mutate(dd_year = year(dd_start),
                       flag_2021 = case_when(
                         dd_year == 2021 ~ 1,
                         TRUE ~ 0
                       )) %>%
                select(dd_month, pct, dd_year, flag_2021)
  
  file_path <- paste0(out_path, "/us_stock_worst_real_drawdowns_", n_months, "_months.jpeg")
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  
  plot <- ggplot(data = to_plot, aes(x = dd_month/12, y = pct, col = as.factor(dd_year), size = as.factor(flag_2021))) +
    geom_line() +
    scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, -0.8, -0.1)) +
    scale_x_continuous(breaks = seq(0, n_months/12, 1), limits = c(0, n_months/12)) +
    scale_size_manual(values = c(0.5, 2), guide = "none") +
    ggtitle(paste0("Top 10 Inflation-Adjusted U.S. Stock Drawdowns\nOver ", n_years, " Years")) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Percentage Decline",
         caption = paste0(source_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(n_month_filter > 120){
    to_plot <- worst_10_dd %>%
      filter(dd_month < n_month_filter) %>%
      mutate(dd_year = year(dd_start),
             flag_2021 = case_when(
               dd_year == 2021 ~ 1,
               TRUE ~ 0
             )) %>%
      filter(flag_2021 == 1 | max_dd < -0.5) %>%
      select(dd_month, pct, dd_year, flag_2021)
    
    file_path <- paste0(out_path, "/select_drawdowns_", n_months, "_months.jpeg")
    source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
    
    plot <- ggplot(data = to_plot, aes(x = dd_month/12, y = pct, col = as.factor(dd_year), size = as.factor(flag_2021))) +
      geom_line() +
      scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, -0.8, -0.1)) +
      scale_x_continuous(breaks = seq(0, n_months/12, 1), limits = c(0, n_months/12)) +
      scale_size_manual(values = c(0.5, 2), guide = "none") +
      ggtitle(paste0("Select Inflation-Adjusted U.S. Stock Drawdowns\nOver ", n_years, " Years")) +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      of_dollars_and_data_theme +
      labs(x = "Year" , y = "Percentage Decline",
           caption = paste0(source_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
}

plot_dd_comparison(36)
plot_dd_comparison(13*12)

# Load in Russell 3000 mcaps
mcaps <- read.csv(paste0(importdir, "/0366_ycharts_2023_prices/timeseries_9-25-2023_mcap.csv"),
                   skip = 6) %>%
  select(-Metric) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, name, value) %>%
  drop_na() %>%
  arrange(symbol, date)

mcap_overall <- mcaps %>%
                  group_by(date) %>%
                  summarise(value = sum(value)) %>%
                  ungroup() %>%
                  mutate(symbol = "R3000")

df <- mcaps %>%
        bind_rows(mcap_overall)

first_mcap <- df %>%
              filter(date == min(mcaps$date)) %>%
              rename(first_mcap = value) %>%
              select(symbol, first_mcap)

df <- df %>%
        left_join(first_mcap) %>%
        mutate(mcap_change = value - first_mcap)

all_dates <- unique(df$date)
final_results <- data.frame()

for(i in 1:length(all_dates)){
  final_results[i, "date"] <- all_dates[i]
  
  tmp <- df %>%
            filter(date == all_dates[i])
  
  r3k <- tmp %>%
          filter(symbol == "R3000") %>%
          pull(mcap_change)
  
  tmp_ind <- tmp %>%
              filter(symbol != "R3000") %>%
              arrange(desc(mcap_change))
  
  value_add <- 0
  counter <- 1
  if(r3k > 0){
    while(r3k > value_add){
      value_add <- value_add + tmp_ind[counter, "mcap_change"]
      counter <- counter + 1
    }
    counter <- counter - 1
    final_results[i, "n_stocks_gain"] <- counter
  } else{
    final_results[i, "n_stocks_gain"] <- NA
  }
}

to_plot <- final_results

file_path <- paste0(out_path, "/russell_3000_stocks_rep_gain.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <- str_wrap("Note: When there is no gain in the Russell 3000, the number of stocks listed is not shown.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = date, y = n_stocks_gain)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  scale_x_date(date_labels = "%m/%y") +
  ggtitle("Number of Stocks Representing\nthe Gain in the Russell 3000\nin 2023") +
  of_dollars_and_data_theme +
  labs(x = "Date" , y = "Number of Stocks",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #