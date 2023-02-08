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

folder_name <- "_fl/0014_discretionary_sims"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Do some data analysis to establish a long-term growth rate
raw <- read.csv(paste0(importdir, "/", folder_name, "/GrowthOfWealth_20230206173453.csv"),
                     skip = 7, 
                     row.names = NULL,
                     col.names = c("date", "index_bond",	"index_sp500", "cpi"))  %>%
  filter(!is.na(index_sp500)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         yr = year(date))

run_retirement_sim <- function(n_years, withdrawal_rate, discretionary_spend_pct){
  start_year <- year(min(raw$date))
  end_year <- year(max(raw$date)) - n_years + 1
  
  n_months <- n_years*12
  
  df <- raw %>%
          filter(yr >= start_year, yr <= end_year)
  
  start_port <- 1* 10^6
  start_spend_monthly <- start_port*withdrawal_rate/12
  
  for(i in 1:n_months){
    if(i == 1){
      df[i, "port"] <- start_port
    } else{
      
    }
  }
}




# ############################  End  ################################## #