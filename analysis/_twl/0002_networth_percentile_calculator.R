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
              filter(year == data_year, age < 35) %>%
                select(networth,wgt)

# Calculate net worth percentiles
find_percentile <- function(amount){
  
  if(amount < 10^6){
    p_change <- 0.01
    p_guess <- 0.5
  } else if(amount < 10^7){
    p_change <- 0.001
    p_guess <- 0.7
  } else if (amount < 10^8){
    p_change <- 0.001
    p_guess <- 0.95
  } else{
    p_change <- 0.0001
    p_guess <- 0.99
  }
  
  solved <- 0
  while(solved == 0){
    guess <- scf_stack %>%
            summarise(nw_percentile = wtd.quantile(networth, weights = wgt, probs = p_guess)) %>%
            pull(nw_percentile)
  
    diff_allowed <- 10^(floor(log10(amount)) - 1)
    
    if(guess - amount > diff_allowed){
      p_guess <- p_guess - p_change
    } else if (amount - guess > diff_allowed){
      p_guess <- p_guess + p_change
    } else{
      solved <- 1
    }
  }
  
  print(paste0("NW Percentile for ", format_as_dollar(amount), " = ", 100*p_guess, "%"))
}

find_percentile(10000)
find_percentile(10^5)
find_percentile(10^6)
find_percentile(10^7)
find_percentile(10^8)


# ############################  End  ################################## #