cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(stringr)
library(lubridate)
library(tidyverse)

folder_name <- "/_fl/0002_big_tech_recessions"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

last_date <- "2020-03-12"

mcap_dow_biggest <- read.csv(paste0(importdir, folder_name, "/dow_stock_market_cap_2001.csv"), skip = 5) %>%
          rename(symbol = Symbol,
                 name = Name) %>%
          select(-Metric) %>%
          gather(-symbol, -name, key=key, value=value) %>%
          mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
                 month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
                 day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
                 date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
          select(date, symbol, name, value) %>%
          filter(date <= last_date)

mcap_faamg <- read.csv(paste0(importdir, folder_name, "/faamg_market_cap_2007_12_01.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, name, value)  %>%
  filter(date <= last_date)

r_3000 <- read.csv(paste0(importdir, folder_name, "/RUATR_data.csv"),
                   col.names = c("date", "value")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  filter(date <= last_date) %>%
  mutate(name = "Russell 3000") 

plot_vs_r_3000 <- function(start_date, end_date, dow_5, time_period_title){
  if(dow_5 == 1){
    big_5 <- mcap_dow_biggest %>%
                      filter(date == start_date) %>%
                      arrange(-value) %>%
                      head(5) %>%
                      select(name, value) %>%
                      rename(first_mcap = value)
    
    big_5_names <- print(paste(big_5$name, collapse = ", "))
    
    big_5_index <- mcap_dow_biggest %>%
                    drop_na() %>%
                    filter(date >= start_date, date <= end_date) %>%
                    inner_join(big_5) %>%
                    mutate(value = value/first_mcap) %>%
                    group_by(date) %>%
                    summarize(value = mean(value, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(name = "Dow Top 5 Stocks")
    
    title_string <- "5 Biggest Stocks in the Dow"
    file_stub <- "dow_big_5"
    additional_note <- paste0("The five largest stocks in the Dow at the start of the period were: ",
                              big_5_names, ".")
  } else{
    big_5 <- mcap_faamg %>%
      filter(date == start_date) %>%
      arrange(-value) %>%
      head(5) %>%
      select(symbol, value) %>%
      rename(first_mcap = value)
    
    big_5_index <- mcap_faamg %>%
      drop_na() %>%
      filter(date >= start_date, date <= end_date) %>%
      inner_join(big_5) %>%
      mutate(value = value/first_mcap) %>%
      group_by(date) %>%
      summarize(value = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(name = "FAAMG Index")
    
    title_string <- "FAAMG Index"
    file_stub <- "faamg"
    if(year(end_date) < 2012){
      additional_note <- "FAAMG index is an equal-weighted index that excludes Facebook, who was not yet a public company."
    } else{
      additional_note <- "FAAMG index is an equal-weighted index."
    }
  }
  
  r_3000_first <- r_3000 %>%
                    filter(date == start_date) %>%
                    pull(value)
  
  r_3000_index <- r_3000 %>%
    filter(date >= start_date, date <= end_date) %>%
    mutate(value = value/r_3000_first)
  
  to_plot <- r_3000_index %>%
              bind_rows(big_5_index)
  
  text_labels <- to_plot %>%
                  filter(date == end_date) %>%
                  mutate(label = ifelse(value > 1, paste0("+", round(100*(value-1), 0), "%"),
                                        paste0(round(100*(value-1), 0), "%")))
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  file_path <- paste0(out_path, "/growth_", 
                      start_date_string, 
                      "_", 
                      end_date_string, 
                      "_", 
                      file_stub, 
                      ".jpeg")
  source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  All performance data shown is total return.  ",
                        additional_note),
                        width = 85)
  
  if(time_period_title == "2020 YTD"){
    vjust_val <- 1
  } else{
    vjust_val <- -1
  }
  
  
  plot <- ggplot(to_plot, aes(x=date, y=(value-1), col = name)) +
            geom_line() +
            geom_text(data=text_labels, aes(x=date, y=(value-1), 
                                            col = name, label = label),
                            vjust = vjust_val,
                      show.legend = FALSE) +
            scale_color_manual(values = c("red", chart_standard_color)) +
            scale_y_continuous(label = percent) +
            scale_x_date(date_labels = "%m/%y") +
            of_dollars_and_data_theme +
            theme(legend.position = "bottom",
                  legend.title = element_blank()) +
            ggtitle(paste0("Russell 3000 vs. ", title_string, "\n", 
                           time_period_title)) +
            labs(x = "Date", y = "Percentage Return",
                 caption = paste0(source_string, "\n", note_string))

  ggsave(file_path, plot, width = 15, height = 12, units = "cm")  
}

### Plot 5 Biggest Dow Stocks vs. Russell 3000 (2001-2001)
plot_vs_r_3000("2001-02-28", "2001-11-30", 1, "2001 Recession")

### Plot 5 Biggest Dow Stocks vs. Russell 3000 (2007-2009)
plot_vs_r_3000("2007-11-30", "2009-06-30", 1, "2008 Recession")

### Plot FAAMG vs. Russell 3000 (2007-2009)
plot_vs_r_3000("2007-11-30", "2009-06-30", 0, "2008 Recession")

### Plot FAAMG vs. Russell 3000 (YTD)
plot_vs_r_3000("2019-12-31", "2020-03-11", 0, "2020 YTD")


# ############################  End  ################################## #

  
