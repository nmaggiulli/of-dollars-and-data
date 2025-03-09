cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(jsonlite)
library(ggrepel)
library(slackr)
library(zoo)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "0443_spx_200day_ma"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "xxxx_spx_ycharts/SPX_data.csv"),
                col.names = c("date", "index_spx")) %>%
          mutate(date = as.Date(date)) %>%
          arrange(date) %>%
          filter(date <= "2024-12-31")

run_ma_analysis <- function(start_date, end_date, log_scale){

  start_year <- year(start_date)
  end_year <- year(end_date)
  
  df <- raw %>%
          filter(date >= start_date,
                 date <= end_date) %>%
          mutate(ma_200day = rollmean(x = index_spx, 200, align = "right", fill = NA),
                 in_out = case_when(
                   index_spx < ma_200day ~ 0,
                   TRUE ~ 1
                 ),
                 in_out_status = case_when(
                   in_out == 1 & lag(in_out, 1) == 1 ~ "Stay Invested",
                   in_out == 0 & lag(in_out, 1) == 0 ~ "Stay Out",
                   in_out == 1 & lag(in_out, 1) == 0 ~ "Enter",
                   in_out == 0 & lag(in_out, 1) == 1 ~ "Exit",
                   TRUE ~ NA
                 ),
                 ret_fwd_250 = lead(index_spx, 250)/index_spx - 1,
                 ret_bh = case_when(
                   row_number() == 1 ~ 1,
                   TRUE ~ index_spx/lag(index_spx, 1)
                 ),
                 ret_ma = case_when(
                   row_number() == 1 ~ 1,
                   in_out_status %in% c("Stay Out", "Enter") ~ 1,
                   TRUE ~ index_spx/lag(index_spx, 1)
                 ),
                 port_bh = cumprod(ret_bh),
                 port_ma = cumprod(ret_ma),  
                 ) %>%
                filter(!is.na(in_out_status))
  
  analysis_rows <- df %>%
                filter(!is.na(ret_fwd_250))
  
  summary_by_status <- analysis_rows %>%
    group_by(in_out_status) %>%
    summarise(n_days = n(),
              pct_days = n_days/nrow(analysis_rows),
              pct25_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.25),
              median_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.5),
              pct75_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.75)) %>%
    ungroup()
  
  export_to_excel(df = summary_by_status,
                  outfile = paste0(out_path, "/spx_200day_ma_summary_", start_year, "_", end_year, ".xlsx"),
                  sheetname = "summary",
                  new_file = 1,
                  fancy_formatting = 0)
  
  export_to_excel(df = df,
                  outfile = paste0(out_path, "/spx_200day_ma_summary_", start_year, "_", end_year, ".xlsx"),
                  sheetname = "all_data",
                  new_file = 0,
                  fancy_formatting = 0)

  to_plot <- df %>%
              select(date, port_bh, port_ma) %>%
              rename(`Buy & Hold` = port_bh,
                     `200-Day MA` = port_ma) %>%
              gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/spx_200day_ma_growth_of_dollar_", start_year, "_", end_year, ".jpeg")
  source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: The 200-Day Moving Average strategy fully invested in U.S. stocks anytime the S&P 500 index is above its 200-day moving average and ",
                                 "moves to 100% cash anytime its below its 200-day moving average."),
                                width = 85)
  
  if(log_scale == 1){
    plot <- ggplot(to_plot, aes(x= date, y = value, col = key)) +
      geom_line() +
      scale_color_manual(values = c("green", "black")) +
      scale_y_continuous(label = dollar, trans = log10_trans()) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("Growth of $1\nBuy & Hold vs. 200-Day Moving Average\n", start_year, " - ", end_year)) +
      labs(x="Year", y="Portfolio Value (Log Scale)",
           caption = paste0(source_string, "\n", note_string))
  } else{
    plot <- ggplot(to_plot, aes(x= date, y = value, col = key)) +
      geom_line() +
      scale_color_manual(values = c("green", "black")) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("Growth of $1\nBuy & Hold vs. 200-Day MA\n", start_year, " - ", end_year)) +
      labs(x="Year", y="Portfolio Value",
           caption = paste0(source_string, "\n", note_string))
  }
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(log_scale == 0){
    #Plot entry exit
    to_plot <- df %>%
                select(date, in_out_status, port_ma)
    
    points <- to_plot %>%
                filter(in_out_status %in% c("Enter", "Exit"))
    
    file_path <- paste0(out_path, "/spx_200day_ma_entry_exit_", start_year, "_", end_year, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = date, y = port_ma)) +
      geom_line(col = "black") +
      geom_point(data = points, aes(x = date, y = port_ma, col = in_out_status),
                 alpha = 0.5) +
      scale_color_manual(values = c("green", "red")) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("200-Day Moving Average Strategy\nEntry and Exit Points\n", start_year, " - ", end_year)) +
      labs(x="Year", y="Portfolio Value",
           caption = paste0(source_string, "\n", note_string))
  
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

run_ma_analysis("1950-01-01", "2024-12-31", 1)
run_ma_analysis("1995-01-01", "2003-12-31", 0)
run_ma_analysis("2000-01-01", "2010-12-31", 0)
run_ma_analysis("2019-01-01", "2024-12-31", 0)

# ############################  End  ################################## #
