cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "xxxx_debasement_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in sp500 Shiller 
sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  filter(date >= as.Date("2020-01-01"))

first <- pull(sp500_ret_pe[1, "price_plus_div"])
first_p <- pull(sp500_ret_pe[1, "price"])
first_cpi <- pull(sp500_ret_pe[1, "cpi"])

df <- sp500_ret_pe %>%
          mutate(growth_of_dollar_real = price_plus_div/first,
                 price_growth = price/first_p,
                 cpi_change = cpi/first_cpi) %>%
          select(date, growth_of_dollar_real, price_growth, cpi_change)

# ############################  End  ################################## #