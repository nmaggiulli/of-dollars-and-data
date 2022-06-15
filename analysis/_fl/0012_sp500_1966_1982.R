cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

folder_name <- "_fl/0012_sp500_1966_1982"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

monthly_payment <- 100

# Load data
raw <- read.csv(paste0(importdir, "/_fl/0012_stock_bond_cpi/DFA_GrowthOfWealth_20220613095224.csv"), skip = 7,
               col.names = c("date", "index_bond", "index_sp500_nom", "index_cpi")) %>%
  filter(!is.na(index_bond)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y") + days(1) - months(1),
         index_sp500_real = index_sp500_nom/index_cpi) %>%
  select(date, index_sp500_nom, index_sp500_real, index_cpi) %>%
  mutate(ret_sp500_nom = index_sp500_nom/lag(index_sp500_nom, 1) - 1,
         ret_sp500_real = index_sp500_real/lag(index_sp500_real, 1) - 1,
         cpi_rate = index_cpi/lag(index_cpi, 1) - 1)

calculate_dca <- function(start_date, end_date, pay_real, ret_real){
  if(ret_real == 1){
    return_var <- "ret_sp500_real"
  } else if (ret_real == 0){
    return_var <- "ret_sp500_nom"
  }
  
  start_dt <- as.Date(start_date)
  end_dt <- as.Date(end_date)
  
  tmp <- raw %>%
          filter(date >= start_dt, date <= end_dt)
  
  if(pay_real == 1){
    for(i in 1:nrow(tmp)){
      if(i == 1){
        tmp[i, "payment"] <- monthly_payment
        tmp[i, "port"] <- monthly_payment * (1 + tmp[i, return_var])
        tmp[i, "cost_basis"] <- monthly_payment
      } else{
        tmp[i, "payment"] <- tmp[(i-1), "payment"]/(1 - tmp[i, "cpi_rate"])
        tmp[i, "port"] <- (tmp[(i-1), "port"] +  tmp[i, "payment"]) * (1 + tmp[i, return_var])
        tmp[i, "cost_basis"] <- tmp[(i-1), "cost_basis"] + tmp[i, "payment"]
      }
    }
  } else{
    tmp$payment <- monthly_payment
    for(i in 1:nrow(tmp)){
      if(i == 1){
        tmp[i, "port"] <- monthly_payment * (1 + tmp[i, return_var])
        tmp[i, "cost_basis"] <- monthly_payment
      } else{
        tmp[i, "port"] <- (tmp[(i-1), "port"] +  tmp[i, "payment"]) * (1 + tmp[i, return_var])
        tmp[i, "cost_basis"] <- tmp[(i-1), "cost_basis"] + monthly_payment
      }
    }
  }
  
  last_real <- tmp[nrow(tmp), "index_sp500_real"]
  last_nom <- tmp[nrow(tmp), "index_sp500_nom"]
  last_cpi <- tmp[nrow(tmp), "index_cpi"]
  
  to_export <- tmp %>%
                  select(date, payment, cost_basis, port, index_sp500_real, index_sp500_nom, index_cpi) %>%
                  mutate(real_cumulative_ret = last_real/index_sp500_real,
                         nom_cumulative_ret = last_nom/index_sp500_nom,
                         real_payment = payment*1/(last_cpi/index_cpi))
  return(to_export)
}
     
port_66_82_real <- calculate_dca("1966-02-01", "1982-08-01", 0, 1)
port_66_82_nom <- calculate_dca("1966-02-01", "1982-08-01", 0, 0)

for(i in 1:nrow(port_66_82_real)){
  row <- nrow(port_66_82_real) - i + 1
  
  port_66_82_real[i, "nom_ann_ret"] <- port_66_82_real[i, "nom_cumulative_ret"]^(1/(row/12)) - 1
  port_66_82_real[i, "real_ann_ret"] <- port_66_82_real[i, "real_cumulative_ret"]^(1/(row/12)) - 1
}

print(mean(port_66_82_real$nom_ann_ret))
print(mean(port_66_82_real$real_ann_ret))
print(sum(port_66_82_real$real_payment))

# ############################  End  ################################## #