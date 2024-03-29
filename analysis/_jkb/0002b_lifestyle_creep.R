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

folder_name <- "_jkb/0002_lifestyle_creep"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

inc <- 100000
raise <- 100000
withdrawal_pct <- 0.04
annual_ret <- 0.04

initial_savings <- 0.1

savings_rates <- c(0.1)

for(initial_savings_rate in savings_rates){
  annual_savings <- inc*initial_savings_rate
  annual_expenditure <- inc*(1-initial_savings_rate)
  retirement_target <- annual_expenditure/withdrawal_pct
  
  n_periods_baseline <- log(1 + (retirement_target/annual_savings)*annual_ret)/log(1 + annual_ret)
  print(n_periods_baseline)
  
  raise_saved_pcts <- c(0.1)
  
  tmp <- data.frame(
    annual_ret = rep(annual_ret, length(raise_saved_pcts)),
    raise_saved_pct = raise_saved_pcts
  )
  
  counter_f <- 1
  for(r in raise_saved_pcts){
    df <- data.frame(year = c(),
                     saving_amount = c(),
                     total_saved = c(),
                     retirement_target = c(),
                     income = c(),
                     pct_total_retirement = c())
    
    counter <- 1
    retire_pct <- 0
    while(retire_pct < 1){
      if(counter == 1){
        df[counter, "year"] <- counter
        df[counter, "saving_amount"] <- annual_savings
        df[counter, "total_saved"] <- annual_savings
        df[counter, "retirement_target"] <- retirement_target
        df[counter, "income"] <- inc
      } else{ 
        if(counter <= 10){
          df[counter, "income"] <- inc
          df[counter, "saving_amount"] <- annual_savings
        } else{
          df[counter, "income"] <- inc + raise
          df[counter, "saving_amount"] <- (inc*initial_savings_rate) + (raise*r)
        }
        df[counter, "year"] <- counter
        df[counter, "total_saved"] <- (df[(counter-1), "total_saved"] * (1 + annual_ret)) + df[counter, "saving_amount"]
        df[counter, "retirement_target"] <- (df[counter, "income"] - df[counter, "saving_amount"])/withdrawal_pct
      }
      
      df[counter, "pct_total_retirement"] <- df[counter, "total_saved"]/df[counter, "retirement_target"]
      retire_pct <- df[counter, "pct_total_retirement"]
      counter <- counter + 1
    }
    tmp[counter_f, "n_periods"] <- counter - 1
    tmp[counter_f, "better_than_baseline"] <- ifelse(tmp[counter_f, "n_periods"] < n_periods_baseline, 1, 0)
    
    if(r == initial_savings_rate & initial_savings_rate == 0.6){
      assign("investigate", df, envir = .GlobalEnv)
    }
    
    counter_f <- counter_f + 1
  }
  
  tmp2 <- tmp %>%
    filter(better_than_baseline == 1) %>%
    head(1) %>%
    select(-better_than_baseline) %>% 
    mutate(savings_rate = initial_savings_rate)
  
  if(initial_savings_rate == savings_rates[1]){
    final_results <- tmp2
  } else{
    final_results <- final_results %>% bind_rows(tmp2) %>%
      select(savings_rate, raise_saved_pct, n_periods)
  }
}

# ############################  End  ################################## #