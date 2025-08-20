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
library(tidylog)
library(zoo)
library(ggjoy)
library(tidyverse)

folder_name <- "0269_ps_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in 2022 data
ps_2022 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_1-8-2022_ps.csv"),
                   skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=ps) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, ps) %>%
  drop_na()

#Bring in 2024 data
ps_2024 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_08-29-2024_ps.csv"),
               skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=ps) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, ps) %>%
  drop_na() %>%
  filter(date > as.Date("2022-01-31"))

ps_2025 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_8-20-2025_ps.csv"),
                    skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=ps) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, ps) %>%
  drop_na() %>%
  filter(date > as.Date("2024-08-31"))

ps <- ps_2022 %>%
        bind_rows(ps_2024) %>%
        bind_rows(ps_2025) %>%
        arrange(symbol, date)

#Now do mcap
mcap_2022 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_1-8-2022_mcap.csv"),
               skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=mcap) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"), 
         mcap = mcap/1000) %>%
  select(date, symbol, mcap) %>%
  drop_na()

mcap_2024 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_08-29-2024_mcap.csv"),
                      skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=mcap) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"), 
         mcap = mcap/1000) %>%
  select(date, symbol, mcap) %>%
  drop_na()



mcap_2025 <- read.csv(paste0(importdir, "/0269_ycharts_market_cap_ps/timeseries_8-20-2025_mcap.csv"),
                      skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=mcap) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"), 
         mcap = mcap/1000) %>%
  select(date, symbol, mcap) %>%
  drop_na()

mcap <- mcap_2022 %>%
  bind_rows(mcap_2024) %>%
  bind_rows(mcap_2025) %>%
  arrange(symbol, date)

df <- ps %>%
        full_join(mcap) %>%
        drop_na() %>%
        mutate(ps_above_20 = ifelse(ps > 20, "P/S > 20", "P/S <= 20"))

all_dates <- df %>%
              group_by(date) %>%
              summarise(n_symbols = n()) %>%
              ungroup() %>%
              filter(n_symbols > 1800) %>%
              select(date)
              
df <- df %>%
        inner_join(all_dates)

dates_to_run <- c("2017-11-30", "2018-11-30", "2019-11-30", "2020-11-30")

source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: U.S. stocks are represented by the Russell 3000 index. ",
                               "Stocks with missing data have been excluded."),
                        width = 85)

for(dt in dates_to_run){
    
  ps_stocks <- df %>%
                filter(date == as.Date(dt)) %>%
                select(symbol, ps_above_20, mcap)
  
  starting_mcap <- ps_stocks %>%
                    group_by(ps_above_20) %>%
                    summarise(starting_mcap = sum(mcap)) %>%
                    ungroup()
  
  to_plot <- df %>%
              filter(date >= as.Date(dt)) %>%
              inner_join(select(ps_stocks, -mcap)) %>%
              group_by(date, ps_above_20) %>%
              summarise(mcap = sum(mcap)) %>%
              ungroup() %>%
              left_join(starting_mcap) %>%
              mutate(mcap_final = mcap/starting_mcap) 
  
  date_string <- date_to_string(dt)
  
  file_path <- paste0(out_path, "/ps_gt_20_mcap_", date_string, ".jpeg")
  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = to_plot, aes(x = date, y = mcap_final, col = ps_above_20)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_x_date(date_labels = "%m/%y") +
    scale_color_manual(values = c("black", "red")) +
    ggtitle(paste0("Growth of $1 for U.S. Stocks\nby P/S Ratio")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Date" , y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

to_plot <- df %>%
                  group_by(date, ps_above_20) %>%
                  summarise(total_mcap = sum(mcap),
                            n_companies = n()) %>%
                  ungroup() %>%
                  filter(ps_above_20 == "P/S > 20")

file_path <- paste0(out_path, "/ps_above_20_mcap.jpeg")

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = date, y = total_mcap)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%m/%y") +
  ggtitle(paste0("Total Market Capitalization of U.S. Stocks\nwith P/S Ratio > 20")) +
  of_dollars_and_data_theme +
  labs(x = "Date" , y = "Market Capitalization (in millions)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #