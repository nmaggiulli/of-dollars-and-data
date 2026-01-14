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
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "0489_scf_net_worth_regressions"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

aaii_raw <- read_excel(paste0(importdir, "0491_aaii_data/aaii.xls"))

# Create a date list to use for subsetting
end_month <- floor_date(Sys.Date(), "month")
all_dates <- seq.Date(as.Date("1987-12-01"), end_month, "month")-1

# Subset based on known properties of file
aaii <- aaii_raw[3:(length(all_dates)+2), 2:6]

colnames(aaii) <- c("stock_funds", "stocks", "bond_funds", "bonds", "cash")  

aaii <- aaii %>%
  mutate(date = as.POSIXct(all_dates),
         stock_allocation = as.numeric(stocks) + as.numeric(stock_funds),
         bond_allocation = as.numeric(bonds) + as.numeric(bond_funds),
         cash_allocation = as.numeric(cash)) %>%
  select(date, stock_allocation, bond_allocation, cash_allocation)


# ############################  End  ################################## #