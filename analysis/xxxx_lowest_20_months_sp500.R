cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(stats)
library(lubridate)
library(tidyverse)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    mutate(date = as.Date(paste0(
                      substring(as.character(date), 1, 4),
                      "-", 
                      ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
                      "-01", 
                      "%Y-%m-%d")))

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
}


# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, price_plus_div) %>%
  mutate(
    ret_1m = 100*(price_plus_div/lag(price_plus_div, 1) - 1),
    ret_3m = 100*(price_plus_div/lag(price_plus_div, 3) - 1),
    ret_6m = 100*(price_plus_div/lag(price_plus_div, 6) - 1),
    ret_12m = 100*(price_plus_div/lag(price_plus_div, 12) - 1)) %>%
    select(date, contains("ret_")) %>%
    filter(!is.na(ret_12m), date >= "1946-01-01")

worst_20 <- sp500_ret_pe %>%
              arrange(ret_1m) %>%
              select(date, ret_1m) %>%
              head(20)

add_months <- function(df, n_months){
  df <- df %>%
          mutate(date = date %m+% months(n_months)) %>%
          select(date)
  return(df)
}

for (j in c(3, 6, 12)){
  ret_name <- paste0("ret_", j, "m")
  dates <- add_months(worst_20, j)
  
  merged <- dates %>%
              inner_join(sp500_ret_pe) %>%
              rename(ret = ret_name)
  
  print(paste0("The average return for the next ", j, " months is: ", round(mean(merged$ret), 1), "%."))
}


# ############################  End  ################################## #