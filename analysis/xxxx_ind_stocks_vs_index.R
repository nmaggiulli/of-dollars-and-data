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
library(ggjoy)
library(tidylog)
library(tidyverse)

folder_name <- "xxxx_ind_stocks_vs_index"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "0157_ind_stocks_ycharts.Rds")) %>%
        select(-ret)

first_day_symbols <- raw %>%
                      filter(date == min(raw$date)) %>%
                      select(symbol) %>%
                      distinct()

tickers_2005 <-   tmp <- read_excel(paste0(importdir, "0157_ind_stocks_rebalance/SP500_tickers.xlsx"), skip = 6) %>%
  select_(.dots = c("TICKER", "`12/30/2005`")) 

colnames(tickers_2005) <- c("symbol", "ret")

tickers_2005 <- tickers_2005 %>%
  filter(!is.na(ret), ret != 0) %>%
  select(symbol) %>%
  arrange(symbol)

full_symbols <- raw %>%
                  group_by(symbol) %>%
                  summarize(n_days = n()) %>%
                  ungroup() %>%
                  inner_join(first_day_symbols)

full_symbols <- full_symbols %>%
                  filter(n_days >= quantile(full_symbols$n_days, probs = 0.5)) %>%
                  select(symbol) %>%
                  inner_join(tickers_2005)

spxtr_daily <- read.csv(paste0(importdir, "/0161_spxtr_daily/SPXTR_data.csv"), col.names = c("date", "index_sp500")) %>%
                mutate(date = as.Date(substr(date, 1, 10), format = "%Y-%m-%d")) %>%
                arrange(date)

df <- raw %>%
        inner_join(full_symbols) %>%
        inner_join(spxtr_daily)

trading_days_list <- c(1, 5, 20, 60, 250, 750, 1250)

final_results <- data.frame()

for(t in trading_days_list){
  tmp <- df %>%
          mutate(ret_sp500 = index_sp500/lag(index_sp500, t) - 1,
                 ret = ifelse(symbol == lag(symbol, t), index/lag(index, t) - 1, NA)) %>%
          filter(!is.na(ret)) %>%
          mutate(ret_diff = ret-ret_sp500,
            above = ifelse(ret_diff > 0, 1, 0)) %>%
          summarize(avg_ret_diff = mean(ret_diff),
                    ret_diff_p25 = quantile(ret_diff, probs = 0.25),
                    ret_diff_p50 = quantile(ret_diff, probs = 0.5),
                    ret_diff_p75 = quantile(ret_diff, probs = 0.75),
                    pct_above = mean(above),
                    n_trading_days = t) %>%
          select(n_trading_days, pct_above, avg_ret_diff, ret_diff_p25, ret_diff_p50, ret_diff_p75)
  
  if(t == min(trading_days_list)){
    final_results <- tmp
  } else{
    final_results <- bind_rows(final_results, tmp)
  }
}


# ############################  End  ################################## #