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
library(tidyverse)

folder_name <- "0345_rich_vs_wealthy"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2019

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

rich_income_limit <- 200000
wealthy_networth_limit <- 1.2 * 10^6

df <- scf_stack %>%
      select(hh_id, imp_id, age, agecl,
             networth,
             income, wgt) %>%
      mutate(high_income = ifelse(income > rich_income_limit, 1, 0),
             high_wealth = ifelse(networth > wealthy_networth_limit, 1, 0)) %>%
      arrange(hh_id, imp_id)

high_inc <- df %>%
            filter(high_income == 1)

n_hhs <- high_inc %>%
            select(hh_id) %>%
            distinct()

# ############################  End  ################################## #