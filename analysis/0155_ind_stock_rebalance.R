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
library(tidyverse)

folder_name <- "0155_ind_stock_rebalance"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0155_ind_stocks_rebalance/ycharts.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%y-%m-%d")) %>%
  filter(symbol != "") %>%
  select(date, symbol, name, value) %>%
  arrange(symbol, date)

dates <- raw %>%
          group_by(date) %>%
          summarize(n_stocks = n()) %>%
          ungroup()

# ############################  End  ################################## #