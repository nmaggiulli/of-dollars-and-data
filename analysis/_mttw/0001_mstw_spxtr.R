cat("\014") # Clear your console
rm(list = ls()) #clear your enviro01ent

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)
library(purrr)
library(ggrepel)

folder_name <- "_mttw/0001_mstw_spxtr"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

run_dca_loop <- 0

raw <- read.csv(paste0(importdir, "/", folder_name, "/MSTW_SPXTR_data_2000_2025_09_30.csv"),
                col.names = c("date", "index_msci_taiwan", "index_spx")) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(date) %>%
  rename(`MSCI Taiwan` = index_msci_taiwan,
         `S&P 500` = index_spx)

raw$`MSCI Taiwan` <- na.locf(raw$`MSCI Taiwan`)
raw$`S&P 500` <- na.locf(raw$`S&P 500`)

max_date <- max(raw$date)

plot_year <- function(start_year, tw_date, spx_date){
  
  filtered <- raw %>%
          filter(year(date) >= start_year)
  
  first_mstw <- filtered[1, "MSCI Taiwan"]
  first_spx <- filtered[1, "S&P 500"]
  
  to_plot <- filtered %>%
              gather(-date, key=key, value=value) %>%
              mutate(growth_of_dollar =  case_when(
                key == "MSCI Taiwan" ~ value/first_mstw,
                TRUE ~ value/first_spx
              )) %>%
            select(-value)

  to_plot$growth_of_dollar <- na.locf(to_plot$growth_of_dollar)
  
  file_path <- paste0(out_path, "/growth_of_dollar_msci_taiwan_vs_sp500_", start_year, ".jpeg")
  
  text_labels <- data.frame()
  
  max_msci_taiwan <- to_plot %>% filter(date == max_date, key == "MSCI Taiwan") %>% pull(growth_of_dollar)
  max_spx <- to_plot %>% filter(date == max_date, key == "S&P 500") %>% pull(growth_of_dollar)
  
  if(max_msci_taiwan > max_spx){
    msci_taiwan_multiplier <- 0.8
    spx_multiplier <- 0.3
  } else{
    msci_taiwan_multiplier <- 0.25
    spx_multiplier <- 0.8
  }
  
  text_labels[1, "date"] <- tw_date
  text_labels[1, "growth_of_dollar"] <- msci_taiwan_multiplier*max_msci_taiwan
  text_labels[1, "key"] <- "MSCI Taiwan"
  text_labels[1, "label"] <- paste0("MSCI 台灣\nNT", format_as_dollar(max_msci_taiwan, 2))
  
  text_labels[2, "date"] <- spx_date
  text_labels[2, "growth_of_dollar"] <- spx_multiplier*max_spx
  text_labels[2, "key"] <- "S&P 500"
  text_labels[2, "label"] <- paste0("S&P 500\nNT", format_as_dollar(max_spx, 2))
  
  # Plot the results
  plot <- ggplot(to_plot, aes(x = date, y = growth_of_dollar, col = key)) +
    geom_line() +
    geom_text(data = text_labels, aes(x=date, y=growth_of_dollar, label = label, col = key),
              family = "my_font") +
    scale_color_manual(guide = "none", values = c("blue", "black")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("新台幣 1 元的報酬成長\nMSCI 台灣 vs. S&P 500\n(", start_year, "-2025)")) +
    labs(x = "年份" , y = "1 元成長值")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do DCA analysis
  to_plot <- to_plot %>%
    group_by(key) %>%
    # if growth_of_dollar is a cumulative index, ret should be simple return:
    mutate(ret = (growth_of_dollar / lag(growth_of_dollar)) - 1,
           ret = coalesce(ret, 0)) %>%
    mutate(
      dca = {
        out <- accumulate(ret, ~ (.x * (1 + .y)) + 100, .init = 0) |> tail(-1)
        out[1] <- 100
        out
      }
    ) %>%
    ungroup()
  
  # Now plot it
  file_path <- paste0(out_path, "/dca_msci_taiwan_vs_sp500_", start_year, ".jpeg")
  
  text_labels <- data.frame()
  
  max_msci_taiwan <- to_plot %>% filter(date == max_date, key == "MSCI Taiwan") %>% pull(dca)
  text_labels[1, "date"] <- as.Date("2022-01-01")
  text_labels[1, "dca"] <- 0.9*max_msci_taiwan
  text_labels[1, "key"] <- "MSCI Taiwan"
  text_labels[1, "label"] <- paste0("MSCI 台灣 \nNT", format_as_dollar(max_msci_taiwan, 0))
  
  max_spx <- to_plot %>% filter(date == max_date, key == "S&P 500") %>% pull(dca)
  text_labels[2, "date"] <- as.Date("2023-01-01")
  text_labels[2, "dca"] <- 0.2*max_spx
  text_labels[2, "key"] <- "S&P 500"
  text_labels[2, "label"] <- paste0("S&P 500\nNT", format_as_dollar(max_spx, 0))
  
  # Plot the results
  plot <- ggplot(to_plot, aes(x = date, y = dca, col = key)) +
    geom_line() +
    geom_text(data = text_labels, aes(x=date, y=dca, label = label, col = key),
              family = "my_font") +
    scale_color_manual(guide = "none", values = c("blue", "black")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("每日投資新台幣 100 元的報酬成長\nMSCI 台灣 vs. S&P 500\n(", start_year, "-2025)")) +
    labs(x = "年份" , y = "價值")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_year(2000, as.Date("2024-01-01"), as.Date("2022-01-01"))
plot_year(2005, as.Date("2023-01-01"), as.Date("2023-01-01"))
plot_year(2010, as.Date("2024-01-01"), as.Date("2022-01-01"))
plot_year(2015, as.Date("2023-01-01"), as.Date("2023-01-01"))

run_dca_compare <- function(start_date){
  filtered <- raw %>%
    filter(date >= start_date)
  
  first_mstw <- filtered[1, "MSCI Taiwan"]
  first_spx <- filtered[1, "S&P 500"]
  
  tmp_df <- filtered %>%
    gather(-date, key=key, value=value) %>%
    mutate(growth_of_dollar =  case_when(
      key == "MSCI Taiwan" ~ value/first_mstw,
      TRUE ~ value/first_spx
    )) %>%
    group_by(key) %>%
    mutate(ret = (growth_of_dollar / lag(growth_of_dollar)) - 1,
           ret = coalesce(ret, 0)) %>%
    mutate(
      dca = {
        out <- accumulate(ret, ~ (.x * (1 + .y)) + 100, .init = 0) |> tail(-1)
        out[1] <- 100
        out
      }
    ) %>%
    ungroup()
  
  return(tmp_df)
}

all_dates <- unique(raw$date)
final_results <- data.frame()
counter <- 1

if(run_dca_loop == 1){
  
  for(i in 1:length(all_dates)){
    print(i)
    start_dt <- all_dates[i]
    
    tmp_df <- run_dca_compare(start_dt)
    
    final_results[counter, "start_date"] <- start_dt
    final_results[counter, "final_dca_tw"] <- tmp_df %>% filter(date == max_date, key == "MSCI Taiwan") %>% pull(dca)
    final_results[counter, "final_dca_spx"] <- tmp_df %>% filter(date == max_date, key == "S&P 500") %>% pull(dca)
    counter <- counter + 1
  }
  
  final_results <- final_results %>%
                    mutate(tw_win = ifelse(final_dca_tw>final_dca_spx, 1, 0),
                           tw_win_pct = final_dca_tw/final_dca_spx - 1)
  
  print(mean(final_results$tw_win))
  print(mean(final_results$tw_win_pct))
}

# ############################  End  ################################## #