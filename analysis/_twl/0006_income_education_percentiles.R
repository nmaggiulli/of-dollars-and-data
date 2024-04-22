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
                  floor(log10(networth)) == 4 ~ "L2 ($100k)",
                  floor(log10(networth)) == 5 ~ "L3 ($1M)",
                  floor(log10(networth)) == 6 ~ "L4 ($10M)",  
                  floor(log10(networth)) == 7 ~ "L5 ($100M)",  
                  floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                  TRUE ~ "ERROR"
                )) %>%
                select(networth, income, wealth_level, edcl, agecl, wgt)

income_by_edcl <- scf_stack %>%
                    group_by(wealth_level, edcl) %>%
                    summarise(income = wtd.quantile(income, weights = wgt, probs = 0.5)) %>%
                    ungroup() 

# ############################  End  ################################## #