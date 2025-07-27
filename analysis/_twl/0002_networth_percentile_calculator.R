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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "_twl/0001_asset_breakdown_by_networth_tier"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
                mutate(wealth_level = case_when(
                  networth < 10000 ~ "L1 (<$10k)",
                  floor(log10(networth)) == 4 ~ "L2 ($10k)",
                  floor(log10(networth)) == 5 ~ "L3 ($100k)",
                  floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                  floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                  floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                  TRUE ~ "ERROR"
                ),
                liquid_assets = asset - reteq - nfin,
                liquid_networth = liquid_assets - debt
                ) %>%
                select(networth, liquid_networth, age, income, wealth_level, wgt)

# Calculate net worth percentiles
find_percentile <- function(amount, var, varname){
  
  if(amount < 10^6){
    p_change <- 0.001
    p_guess <- 0.5
    log_reduce <- 1
  } else if(amount < 10^7){
    p_change <- 0.0001
    p_guess <- 0.8
    log_reduce <- 2
  } else if (amount < 10^8){
    p_change <- 0.0001
    p_guess <- 0.97
    log_reduce <- 2
  } else{
    p_change <- 0.0001
    p_guess <- 0.99
    log_reduce <- 2
  }
  
  solved <- 0
  while(solved == 0){
    guess <- scf_stack %>%
            rename(summary_col = !!sym(var)) %>%
            summarise(nw_percentile = wtd.quantile(summary_col, weights = wgt, probs = p_guess)) %>%
            pull(nw_percentile)
  
    diff_allowed <- 10^(floor(log10(amount) - log_reduce))
    
    if(guess - amount > diff_allowed){
      p_guess <- p_guess - p_change
    } else if (amount - guess > diff_allowed){
      p_guess <- p_guess + p_change
    } else{
      solved <- 1
    }
  }
  
  print(paste0(varname, " Percentile for ", format_as_dollar(amount), " = ", 100*p_guess, "%"))
}

find_percentile(10000, "networth", "Net Worth")
find_percentile(10^5, "networth", "Net Worth")
find_percentile(10^6, "networth", "Net Worth")
find_percentile(10^7, "networth", "Net Worth")
find_percentile(10^8, "networth", "Net Worth")

find_percentile(35000, "income", "Income")
find_percentile(70000, "income", "Income")
find_percentile(140000, "income", "Income")

find_percentile(10000, "liquid_networth", "Liquid Net Worth")
find_percentile(100000, "liquid_networth", "Liquid Net Worth")
find_percentile(10^6, "liquid_networth", "Liquid Net Worth")
find_percentile(10^7, "liquid_networth", "Liquid Net Worth")

# ############################  End  ################################## #`