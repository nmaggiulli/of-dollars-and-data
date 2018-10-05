cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                    filter(date >= 1972.11)

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

# Purchases for the world's market timer
purchases <- data.frame(date = c("1972-12-01", "1987-08-01", "1999-12-01", "2007-10-01"),
                        amount = c(6000, 46000, 68000, 64000)) %>%
  mutate(date = as.Date(date))

# Change the Date to a Date type for plotting the S&P data
df <- select(sp500_ret_pe, date, price_plus_div) %>%
                  mutate(date = as.Date(paste0(
                    substring(as.character(date), 1, 4),
                    "-", 
                    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
                    "-01", 
                    "%Y-%m-%d"))) %>%
                  left_join(purchases) %>%
                  mutate(amount = ifelse(is.na(amount), 0, amount),
                         ret_sp500 = price_plus_div/lag(price_plus_div, 1) - 1) %>%
                  filter(!is.na(ret_sp500))
              
for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "value"] <- df[i, "amount"] * (1 + df[i, "ret_sp500"])
  } else {
    df[i, "value"] <- (df[(i-1), "value"] + df[i, "amount"]) * (1 + df[i, "ret_sp500"])
  }
}


# ############################  End  ################################## #

  
