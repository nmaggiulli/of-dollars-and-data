cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "xxxx_sp500_cape"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in all data
ret_yr <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  rename(index = price_plus_div) %>%
                  mutate(yr = year(date),
                         mt = month(date)) %>%
                  filter(mt == 1) %>%
                  mutate(ret = index/lag(index) - 1) %>%
                  select(date, ret, yr, index) %>%
                  filter(!is.na(ret), yr > 1940)


# ############################  End  ################################## #