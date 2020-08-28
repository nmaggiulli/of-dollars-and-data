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

folder_name <- "xxxx_zillow_data"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

start_date <- "2020-01-01"

rents <- read.csv(paste0(importdir, "/xxxx_zillow_data/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv")) %>%
          clean_cols() %>%
          rename(region_id = regionid,
                 region = regionname) %>%
          select(-sizerank) %>%
          gather(-region_id, -region, key=key, value=value) %>%
          mutate(yr = gsub("x(\\d+)_(\\d+)", "\\1", key, perl = TRUE),
                 mt = gsub("x(\\d+)_(\\d+)", "\\2", key, perl = TRUE),
                 date = as.Date(paste0(yr, "-", mt, "-01"))) %>%
          select(region_id, region, date, value) %>%
          drop_na() %>%
          filter(date >= start_date)

import_zillow <- function(filename){
  tmp <- read.csv(paste0(importdir, "/xxxx_zillow_data/", filename, ".csv")) %>%
    clean_cols() %>%
    rename(region_id = regionid,
           region = regionname) %>%
    select(-sizerank, -regiontype, -statename) %>%
    gather(-region_id, -region, key=key, value=value) %>%
    mutate(yr = gsub("x(\\d+)_(\\d+)_(\\d+)", "\\1", key, perl = TRUE),
           mt = gsub("x(\\d+)_(\\d+)_(\\d+)", "\\2", key, perl = TRUE),
           date = as.Date(paste0(yr, "-", mt, "-01"))) %>%
    select(region_id, region, date, value) %>%
    drop_na() %>%
    filter(date >= start_date)
  
  return(tmp)
}

msp <- import_zillow("Metro_median_sale_price_uc_SFR_sm_sa_month")
fsi <- import_zillow("Metro_invt_fs_uc_sfr_smoothed_month")
spc <- import_zillow("Metro_perc_listings_price_cut_uc_sfr_smoothed_month")

calc_pct <- function(df){
  first_df <- df %>%
                filter(date == start_date) %>%
                rename(first_value = value) %>%
                select(-date)
  
  out_df <- df %>%
                left_join(first_df) %>%
                mutate(pct = value/first_value) %>%
                select(-first_value) %>%
                filter(date == max(df$date))
  
  return(out_df)
}

rents_pct <- calc_pct(rents)
msp_pct <- calc_pct(msp)
fsi_pct <- calc_pct(fsi)
spc_pct <- calc_pct(spc)

# ############################  End  ################################## #