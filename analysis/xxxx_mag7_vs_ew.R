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
library(tidylog)

folder_name <- "xxxx_mag7_vs_ew"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

start_date <- "2017-01-03"
end_date <- "2024-02-23"

raw <- read.csv(paste0(importdir, "xxxx_mag7_data/timeseries_2-25-2024.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, value) %>%
  arrange(symbol, date) %>%
  mutate(mag7 = case_when(
    symbol %in% c("AAPL", "GOOG", "META", "AMZN", "TSLA", "NVDA", "MSFT") ~ 1,
    TRUE ~ 0
  )) %>%
  drop_na() %>%
  filter(symbol != "GOOGL")

first_date_stocks <- raw %>%
              filter(date == start_date) %>%
              select(symbol, value) %>%
              rename(first_value = value)

df <- raw %>%
        filter(date >= start_date) %>%
        inner_join(first_date_stocks) %>%
        mutate(growth = value/first_value)

mag7_value <- df %>%
          filter(mag7 == 1, date == end_date) %>%
          mutate(final_value = growth*1/7) %>%
          summarise(final_value = sum(final_value)) %>%
          pull(final_value)

n_stocks <- 2

non_mag7 <- df %>%
              filter(mag7 != 1, date == end_date) %>%
              mutate(final_value = growth*1/n_stocks) %>%
              arrange(desc(final_value)) %>%
              mutate(lead =  rollsum(x = final_value, n_stocks, align = "right", fill = NA))

stocks_to_loop <- non_mag7 %>%
                    filter(lead > mag7_value) %>%
                    select(symbol)

counter <- 472                  
for(s in 1:nrow(stocks_to_loop)){
  stock <- stocks_to_loop[s, "symbol"]
  
  s_value <- non_mag7 %>%
                  filter(symbol == stock) %>%
                  pull(final_value)
  
  value_limit <- mag7_value - s_value
  
  count_s <- non_mag7 %>%
              filter(row_number() > s, final_value > value_limit) %>%
              nrow() - 1
  
  print(count_s)
  
  counter <- counter + count_s
}

# ############################  End  ################################## #