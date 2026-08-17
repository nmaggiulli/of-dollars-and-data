cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
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
library(tidyverse)

folder_name <- "/_jkb/0020_sp500_period_comparison"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

period_length_months <- 50
start_period <- as.Date("2022-04-01")

start_period + months(period_length_months)

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                      filter(date <= start_period + months(period_length_months)) %>%
                      select(date, price_plus_div)
                      
df <-  sp500_ret_pe %>%
          mutate(ret_fwd = (lead(price_plus_div, period_length_months)/price_plus_div)^(12/period_length_months) - 1,
                 ret_fwd_pctile = percent_rank(ret_fwd) 
          )

start_row <- df %>% filter(date == start_period)

print(paste0(
  format(start_period, "%Y-%m"), ": ret_fwd = ", round(100 * start_row$ret_fwd, 2), "% | ",
  "percentile rank = ", round(100 * start_row$ret_fwd_pctile, 1), "th ",
  "(out of ", sum(!is.na(df$ret_fwd)), " non-NA ", period_length_months, "-month periods)"
))
