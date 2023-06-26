cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(tidyverse)

folder_name <- "0354_discretionary_sims"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

run_sims <- 0

run_retirement_sim <- function(n_years, withdrawal_rate, discretionary_spend_pct){
  start_year <- year(min(raw$date))
  end_year <- year(max(raw$date)) - n_years + 1
  all_years <- seq(start_year, end_year)
  
  final_results <- data.frame()
  counter <- 1
  for(year in all_years){
    end_yr <- year+ n_years - 1
    
    df <- raw %>%
      filter(yr >= year, yr <= end_yr) %>%
      select(date, yr, change_in_cpi, ret_port)
    
    dd_years <- dd %>%
      filter(yr >= year, yr <= end_yr)
    
    start_port <- 1* 10^6
    for(i in 1:nrow(df)){
      ret_port <- df[i, "ret_port"]
      
      if(i == 1){
        current_year <- df[i, "yr"]
        
        current_discretionary_pct <- dd_years %>%
          filter(yr == current_year) %>%
          pull(discretionary_pct)
        
        required_spend <- start_port * (withdrawal_rate - withdrawal_rate*discretionary_spend_pct)
        disc_spend <- start_port * (withdrawal_rate*discretionary_spend_pct) * (current_discretionary_pct)
        monthly_spend <- (required_spend + disc_spend)/12
        
        df[i, "port"] <- (start_port - monthly_spend) * (1 + ret_port)
      } else{
        mt <- month(df[i, "date"])
        
        if(mt == 1){
          current_year <- df[i, "yr"]
          
          current_discretionary_pct <- dd_years %>%
            filter(yr == current_year) %>%
            pull(discretionary_pct)
          
          change_in_cpi <- df[i, "change_in_cpi"]
          
          required_spend <- required_spend * (1 + change_in_cpi)
          disc_spend <- start_port * (withdrawal_rate*discretionary_spend_pct) * (current_discretionary_pct)
          monthly_spend <- (required_spend + disc_spend)/12
          
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        } else{
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        }
      }
      if(df[i, "port"] < 0){
        df[i, "port"] <- 0
      }
    }
    final_results[counter, "withdrawal_rate"] <- withdrawal_rate
    final_results[counter, "discretionary_pct"] <- discretionary_spend_pct
    final_results[counter, "n_years"] <- n_years
    final_results[counter, "start_year"] <- year
    final_results[counter, "end_year"] <- end_yr
    final_results[counter, "final_port"] <- df[nrow(df), "port"]
    
    counter <- counter + 1
  }
  return(final_results)
}

run_full_sim <- function(n_yrs, s_weight){

  # Do some data analysis to establish a long-term growth rate
  raw <- read.csv(paste0(importdir, "/_fl/0014_discretionary_sims/GrowthOfWealth_20230206173453.csv"),
                       skip = 7, 
                       row.names = NULL,
                       col.names = c("date", "index_bond",	"index_sp500", "cpi"))  %>%
    filter(!is.na(index_sp500)) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           yr = year(date),
           change_in_cpi = cpi/lag(cpi, 12) - 1)
  
  for(i in 1:nrow(raw)){
    if(i == 1){
      raw[i, "value_bond"] <- 1 - s_weight
      raw[i, "value_stock"] <- s_weight
      raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
      raw[i, "ret_port"] <- 0
    } else{
      mt <- month(raw[i, "date"])
      if(mt == 1){
        raw[i, "value_bond"] <- raw[(i-1), "value_port"] * (1 - s_weight) * (raw[i, "index_bond"]/raw[(i-1), "index_bond"])
        raw[i, "value_stock"] <- raw[(i-1), "value_port"] * s_weight * (raw[i, "index_sp500"]/raw[(i-1), "index_sp500"])
      } else{
        raw[i, "value_bond"] <- raw[(i-1), "value_bond"] * raw[i, "index_bond"]/raw[(i-1), "index_bond"]
        raw[i, "value_stock"] <- raw[(i-1), "value_stock"] * raw[i, "index_sp500"]/raw[(i-1), "index_sp500"]
      }
      raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
      raw[i, "ret_port"] <- raw[i, "value_port"]/raw[(i-1), "value_port"] - 1
    }
  }
  
  assign("raw", raw, envir = .GlobalEnv)
  
  dd <- raw %>%
          select(date, index_sp500) %>%
          drawdown_path() %>%
          mutate(mt = month(date),
                 yr = year(date) + 1) %>%
          filter(mt == 12) %>%
          mutate(discretionary_pct = case_when(
            pct > -0.1 ~ 1,
            pct > -0.2 ~ 0.5,
            TRUE ~ 0
          )) %>%
          select(yr, discretionary_pct) %>%
          bind_rows(data.frame(yr = 1926, discretionary_pct = 1)) %>%
          arrange(yr)
  
  assign("dd", dd, envir = .GlobalEnv)
  
  discretionary_pcts <- seq(0, 0.7, 0.1)
  withdrawal_rates <- seq(0.04, 0.07, 0.0025)
  
  for(d in discretionary_pcts){
    for(w in withdrawal_rates){
      print(paste0("Discretionary = ", d, ", Withdrawal = ", w))
      fr <- run_retirement_sim(n_yrs, w, d)
      
      if(w == min(withdrawal_rates) & d == min(discretionary_pcts)){
        final_results_disc <- fr
      } else{
        final_results_disc <- final_results_disc %>% bind_rows(fr)
      }
    }
  }
  
  summary <- final_results_disc %>%
                mutate(survival = ifelse(final_port > 0, 1, 0)) %>%
                group_by(withdrawal_rate, discretionary_pct, n_years) %>%
                summarise(n_simulations = n(),
                  survival_pct = mean(survival)) %>%
                rename(withdrawal_pct = withdrawal_rate) %>%
                ungroup()
  return(summary)
}

if(run_sims == 1){
  results_80_base <- run_full_sim(30, 0.8) %>%
    rename(stock_80_pct = survival_pct)
  
  summary <- results_80_base
  
  export_to_excel(
    df = summary,
    outfile = paste0(out_path, "/all_discretionary_sims_qtr_pcts.xlsx"),
    sheetname = "results",
    new_file = 1,
    fancy_formatting = 1
  )
} else{
  summary <- read_excel(paste0(out_path, "/all_discretionary_sims_qtr_pcts.xlsx"),
                        sheet = "results")
  
  wide <- summary %>%
            select(-n_years, -n_simulations) %>%
            spread(key = discretionary_pct, value = stock_80_pct)
  
  export_to_excel(
    df = wide,
    outfile = paste0(out_path, "/all_discretionary_sims_qtr_pcts.xlsx"),
    sheetname = "heatmap",
    new_file = 0,
    fancy_formatting = 0
  )
}




# ############################  End  ################################## #