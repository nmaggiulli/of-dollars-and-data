cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "xxxx_spx_25day_drawdowns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read_excel(paste0(importdir, "0097_spx_daily/spx_daily.xlsx")) %>%
        rename(date = Period,
               index = `S&P 500 Level`) %>%
        select(date, index) %>%
        mutate(mt_ret = index/lag(index,25) - 1,
               gt_8pt8 = ifelse(mt_ret < -0.088, 1, 0)) %>%
        filter(!is.na(mt_ret))




# ############################  End  ################################## #
