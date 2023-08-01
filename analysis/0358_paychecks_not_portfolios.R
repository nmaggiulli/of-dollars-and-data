cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(dplyr)

folder_name <- "0358_paychecks_not_portfolios"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  filter(date >= "1946-01-01", date <= "2014-06-01") %>%
                  select(date, price_plus_div, cpi)

retire_bal <- 8000000

cont <- sp500_ret_pe

orig_monthly_cont <- 10
current_bal <- 0

while(current_bal < retire_bal){
  current_bal <- 0
  orig_monthly_cont <- orig_monthly_cont + 10
  monthly_cont <- orig_monthly_cont
  print(monthly_cont)
  for(i in 1:nrow(cont)){
    if(i == 1){
      cont[i, "monthly_cont"] <- monthly_cont
      cont[i, "balance"] <- monthly_cont
    } else{
      ret <- pull(cont[i, "price_plus_div"])/pull(cont[(i-1), "price_plus_div"]) - 1
      cpi <- pull(cont[i, "cpi"])/pull(cont[(i-1), "cpi"]) - 1
      
      monthly_cont <- monthly_cont * (1 + cpi)
      
      cont[i, "balance"] <- cont[(i-1), "balance"] * (1 + ret) + monthly_cont
      cont[i, "monthly_cont"] <- monthly_cont
    }
  }
  current_bal <- pull(cont[nrow(cont), "balance"])
  
  if(orig_monthly_cont == 50){
    print(current_bal)
  }
}


# ############################  End  ################################## #