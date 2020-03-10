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

mcap <- read.csv(paste0(importdir, folder_name, "/dow_stock_market_cap_1999.csv"), skip = 5) %>%
          rename(symbol = Symbol,
                 name = Name) %>%
          select(-Metric) %>%
          gather(-symbol, -name, key=key, value=value) %>%
          mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
                 month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
                 day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
                 date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d") + days(1)) %>%
          select(date, symbol, name, value) %>%
          filter(date < "2020-01-01")

raw <- read.csv(paste0(importdir, folder_name, "/russell_3k_dow_tr_level.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d") + days(1)) %>%
  select(date, symbol, name, value)

recession_dates <- data.frame(start = c(as.Date("2001-03-01"), as.Date("2007-12-01")),
                              end = c(as.Date("2001-11-01"), as.Date("2009-06-01")))



# ############################  End  ################################## #

  
