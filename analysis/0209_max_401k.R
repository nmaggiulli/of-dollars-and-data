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

folder_name <- "0209_max_401k"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

annual_savings <- 10000
cap_gains <- 0.15
annual_dividend <- 0.02
annual_growth <- 0.02

# Pretax parameters
inc_tax <- 0.24
tax_rate_retire <- 0.22

n_years <- 40

df <- data.frame()

for(i in 1:n_years){
  df[i, "year"] <- i
  if(i == 1){
    df[i, "value_roth401k"] <- annual_savings
    df[i, "value_trad401k"] <- annual_savings/(1-inc_tax)
    df[i, "post_tax_dividend"] <- 0
    df[i, "unrealized_gains"] <- 0
    df[i, "cum_unrealized_gains"] <- 0
    df[i, "value_taxable"] <- annual_savings
    df[i, "liquid_taxable"] <- annual_savings
  } else{
    df[i, "post_tax_dividend"] <- (df[(i-1), "value_taxable"] * annual_dividend) * (1 - cap_gains)
    df[i, "unrealized_gains"] <- df[(i-1), "value_taxable"] * annual_growth
    df[i, "cum_unrealized_gains"] <- df[(i-1), "unrealized_gains"] + df[i, "unrealized_gains"]
    df[i, "value_taxable"] <- df[(i-1), "value_taxable"] + df[i, "unrealized_gains"] + df[i, "post_tax_dividend"] + annual_savings
    df[i, "liquid_taxable"] <- df[i, "value_taxable"] - (cap_gains*df[i, "cum_unrealized_gains"]) 
    
    df[i, "value_roth401k"] <- (df[(i-1), "value_roth401k"] * (1+annual_growth+annual_dividend)) + annual_savings
    df[i, "value_trad401k"] <- (df[(i-1), "value_trad401k"] * (1+annual_growth+annual_dividend)) + (annual_savings/(1-inc_tax))
  }
  df[i, "liquid_trad401k"] <- df[i, "value_trad401k"] * (1-tax_rate_retire)
  
  df[i, "roth401k_tot_premium"] <- df[i, "value_roth401k"]/df[i, "liquid_taxable"]
  df[i, "trad401k_tot_premium"] <- df[i, "liquid_trad401k"]/df[i, "liquid_taxable"]
  
  df[i, "roth401k_ann_premium"] <- df[i, "roth401k_tot_premium"]^(1/i) - 1
  df[i, "trad401k_ann_premium"] <- df[i, "trad401k_tot_premium"]^(1/i) - 1
}

# ############################  End  ################################## #