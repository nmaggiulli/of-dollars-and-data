cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Bitcoin data
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/import/0027_import_quandl_bitcoin.R")))
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Start Program Here ######################### #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(quadprog)
library(lubridate)
library(fTrading)
library(quantmod)
library(tidyr)
detach("package:dplyr", unload=TRUE)
library(dplyr)

# ############################  End  ################################## #

# Load in Bitcoin data and then scrape the other data from Yahoo Finance
bcoin <- readRDS(paste0(localdir, "0027_quandl_bitcoin.Rds")) %>%
          mutate(day = day(date),
                 day_of_week = weekdays(date),
                 asset = "Bitcoin") %>%
          select(date, day, day_of_week, value, asset)

calculate_dca <- function(start_date, end_date, frequency, dca_amount){
  
  
  df <- filter(bcoin, date >= start_date, date <= end_date) 
  
  if(frequency == "weekly"){
    df <- df %>%
            filter(day_of_week == "Monday")
  } else if (frequency == "monthly"){
    df <- df %>%
            filter(day == 1)
  }

  last_price <- df[nrow(df), "value"]
  
  df <- df %>%
          mutate(dca_ret = last_price/value - 1,
                 dca_amount = (1 + dca_ret) * dca_amount)
  
  return(df)
}

weekly_one_year_ago <- calculate_dca("2018-11-25", "2019-11-25", "weekly", 10)

summarized_weekly <- weekly_one_year_ago %>%
  summarize(dca_ret = mean(dca_ret),
            dca_amount = sum(dca_amount))
