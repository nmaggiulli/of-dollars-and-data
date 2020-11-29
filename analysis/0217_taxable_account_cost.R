cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0217_taxable_account_cost"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

ret_high <- 0.07
ret_low <- 0.02
cap_gains <- 0.15
starting_amount <- 10000

df <- data.frame(years = seq(1, 30, 1))

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "port_bh_high"] <- starting_amount
    df[i, "port_bh_low"] <- starting_amount
    df[i, "port_bh_high_liq"] <- starting_amount
    df[i, "port_bh_low_liq"] <- starting_amount
    df[i, "port_high_sell_ann"] <- starting_amount
    df[i, "port_low_sell_ann"] <- starting_amount
  } else{
    df[i, "port_bh_high"] <- df[(i-1), "port_bh_high"] * (1 + ret_high)
    df[i, "port_bh_low"] <- df[(i-1), "port_bh_low"] * (1 + ret_low)
    
    df[i, "port_bh_high_liq"] <-  df[i, "port_bh_high"] - ((df[i, "port_bh_high"] - starting_amount)*(cap_gains))
    df[i, "port_bh_low_liq"] <-  df[i, "port_bh_low"] - ((df[i, "port_bh_low"] - starting_amount)*(cap_gains))
    
    df[i, "port_high_sell_ann"] <- df[(i-1), "port_high_sell_ann"] * (1 + (ret_high*(1-cap_gains)))
    df[i, "port_low_sell_ann"] <- df[(i-1), "port_low_sell_ann"] * (1 + (ret_low*(1-cap_gains)))
  }
  
  df[i, "port_high_tax_low_nontax"] <- df[i, "port_bh_high_liq"] + df[i, "port_bh_low"]
  df[i, "port_low_tax_high_nontax"] <- df[i, "port_low_sell_ann"] + df[i, "port_bh_high"]
  
  df[i, "port_bh_high_premium"] <- df[i, "port_bh_high_liq"]/df[i, "port_high_sell_ann"]
  df[i, "bh_high_ann_premium"] <- df[i, "port_bh_high_premium"]^(1/i) - 1
}




# ############################  End  ################################## #