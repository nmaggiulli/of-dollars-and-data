cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(stringr)
library(readxl)
library(slackr)
library(tidyverse)

folder_name <- "0155_ind_stocks_rebalance"

########################## Start Program Here ######################### #

years <- seq(2010, 2011, 1)
for(y in years){
  print(y)
  
  if(y == 2011 | y == 2016){
    day <- 30
  } else if (y == 2017){
    day <- 29
  } else{
    day <- 31
  }
  
  if(y == 2019){
    month <- 11
    day <- 29
  } else{
    month <- 12
  }
  
  dt_string <- paste0("`", month, "/", day, "/", y, "`")
  
  tmp <- read_excel(paste0(importdir, "0155_ind_stocks_rebalance/SP500_tickers.xlsx"), skip = 6) %>%
    select_(.dots = c("TICKER", dt_string)) 
  
  colnames(tmp) <- c("ticker", "ret")
  
  tmp <- tmp %>%
          filter(!is.na(ret), ret != 0) %>%
          select(ticker) %>%
          arrange(ticker)
  
  print(head(tmp))
  
  if(y == min(years)){
    final <- tmp
  } else{
    final <- bind_rows(final, tmp) %>%
                distinct() %>%
                arrange(ticker)
  }
}

write.csv(final, paste0(exportdir, "0160_ycharts_mcap_sp500_stocks/tickers_for_mcap_2010_2019.csv"), row.names = FALSE)


# ############################  End  ################################## #