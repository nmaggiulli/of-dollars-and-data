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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0162_epidemics_vs_dow"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_fwd_ret <- function(n_days, flu_name, flu_title, flu_label, df_in, ret_data, source_in1, source_in2){
  
  file_path <- paste0(out_path, "/", ret_data, "_", flu_name, "_", n_days, "_day_fwd_ret.jpeg")
  source_string <- str_wrap(paste0("Source:  ", source_in1, ", ", source_in2, " (OfDollarsAndData.com)"),
                            width = 85)
  note_string <-  str_wrap(paste0("Note:  Days with missing data were filled using linear extrapolation."), 
                           width = 85)
  
  to_plot <- df_in
  
  plot <- ggplot(to_plot, aes(x=epi_count, y=fwd_ret)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(label = percent) +
    scale_x_continuous(label = comma) +
    of_dollars_and_data_theme +
    ggtitle(paste0(flu_title, " vs.\n", n_days, "-Day ", ret_data, " Forward Return")) +
    labs(x=paste0(flu_label), y=paste0(n_days, "-Day ", ret_data, " Forward Return"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_flu_data <- function(flu_name, flu_title, flu_label, ret_data, source_in1, source_in2){

  epi_data <- read.csv(paste0(importdir, "0162_epidemics_vs_dow_data/", flu_name, "_wiki_webplotdigi.csv"),
                       col.names = c("date", "epi_count")) %>%
                  mutate(date = as.Date(date, "%Y/%m/%d")) %>%
                  arrange(date) %>%
                  mutate(daily_diff = (lead(epi_count,1) - epi_count)/as.numeric(lead(date,1) - date))
  
  min_date <- min(epi_data$date)
  max_date <- max(epi_data$date)
  
  all_dates <- data.frame(date = seq.Date(min_date, max_date + days(365), 1))
  
  if(ret_data == "Dow"){
  ret_daily <- read_excel(paste0(importdir, "0162_epidemics_vs_dow_data/daily_dow_bloomberg.xlsx"), sheet="Sheet1",
                          col_names = c("date", "index")) %>%
                mutate(date = as.Date(date)) %>%
                filter(date >= min_date, date <= max_date + days(365))
  } else if(ret_data == "EAFE"){
    ret_daily <- read.csv(paste0(importdir, "0162_epidemics_vs_dow_data/daily_em_eafe_ycharts.csv"),
                          col.names = c("date", "eafe", "em")) %>%
                  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
                  rename(index = eafe) %>%
                  select(date, index)
  } else if (ret_data == "EM"){
    ret_daily <- read.csv(paste0(importdir, "0162_epidemics_vs_dow_data/daily_em_eafe_ycharts.csv"),
                          col.names = c("date", "eafe", "em")) %>%
                  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
                  rename(index = em) %>%
                  select(date, index)
  }
  
  df <- all_dates %>%
          left_join(ret_daily) %>%
          left_join(epi_data) %>%
          arrange(date) %>%
          mutate(index = ifelse(is.na(index), lead(index), index))
  
  for(i in 2:nrow(df)){
    if(is.na(df[i, "daily_diff"])){
      df[i, "daily_diff"] <- df[(i-1), "daily_diff"]
    }
    if(is.na(df[i, "index"])){
      df[i, "index"] <- df[(i-1), "index"]
    }
    
    if(df[i, "date"] <= max_date){
      df[i, "epi_count"] <-  df[(i-1), "epi_count"] + df[i, "daily_diff"]
    } else{
      df[i, "epi_count"] <- NA
      df[i, "daily_diff"] <- NA
    }
  }
  
  # Plot deaths per 1,000 over time
  file_path <- paste0(out_path, "/incidents_time_", flu_name, ".jpeg")
  source_string <- paste0("Source:   ", source_in1, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Days with missing data were filled using linear extrapolation."), 
                           width = 85)
  
  to_plot <- df %>%
    filter(date <= max_date)
  
  plot <- ggplot(to_plot, aes(x=date, y=epi_count)) +
    geom_line() +
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%m/%d/%Y") +
    of_dollars_and_data_theme +
    ggtitle(paste0(flu_title)) +
    labs(x=paste0("Date"), y=paste0(flu_label),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  ret_days <- c(7, 30, 60, 90, 180, 365)
  
  for(r in ret_days){
    
    filtered_df <- df %>%
      mutate(fwd_ret = lead(index, r)/index - 1) %>%
      filter(date <= max_date) 
    
    plot_fwd_ret(r, flu_name, flu_title, flu_label, filtered_df, ret_data, source_in1, source_in2)
  }
}

all_epidemics <- c("spanish_flu", "swine_flu", "ebola")
for(epi in all_epidemics){
  
  if(epi == "spanish_flu"){
    title <- "Spanish Flu Per-Capita Deaths in UK"
    label <- "Deaths Per 1,000 People"
    source_epi <- "Wikimedia Commons"
  } else if(epi == "swine_flu"){
    title <- "Swine Flu Cases in UK"
    label <- "Number of Cases"
    source_epi <- "Wikimedia Commons"
  } else if(epi == "ebola"){
    title <- "Total West Africa Ebola Cases"
    label <- "Number of Cases"
    source_epi <- "Shultz, James & Espinel, Zelde & Espinola, Maria & Rechkemmer, Andreas"
  } 
  
  ret_data_source <- c("Dow", "EAFE", "EM")
  
  for(rds in ret_data_source){
    if(rds == "Dow"){
      source_ret <- "Bloomberg"
      plot_flu_data(epi, title, label, rds, source_epi, source_ret)
    } else{
      source_ret <- "YCharts"
      if(epi != "spanish_flu"){
        plot_flu_data(epi, title, label, rds, source_epi, source_ret)
      }
    }
  }
}










# ############################  End  ################################## #