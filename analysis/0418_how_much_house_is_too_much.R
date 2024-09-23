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

folder_name <- "0418_how_much_house_is_too_much"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
              mutate(`Primary Residence` = houses/asset,
                     owns_home = ifelse(houses > 0, 1, 0),
                    ) %>%
                select(networth, `Primary Residence`, owns_home, asset,
                       wgt)

# Calculate homeownership rate
homeown_rate <- scf_stack %>%
  summarise(
    owns_home = wtd.mean(owns_home, weights = wgt),
  ) %>%
  ungroup()

# Do home percentage of total assets
homeowner_stats <- scf_stack %>%
  filter(owns_home == 1) %>%
  summarise(
    pct10_primary_residence = wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.1),
    pct25_primary_residence = wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.25),
    median_primary_residence = wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.5),
    pct75_primary_residence = wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.75),
    pct90_primary_residence = wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.9),
    median_assets = wtd.quantile(asset, weights = wgt, probs = 0.5),
  ) %>%
  ungroup() 

# ############################  End  ################################## #