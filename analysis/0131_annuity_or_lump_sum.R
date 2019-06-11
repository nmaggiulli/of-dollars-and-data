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
library(FinCal)
library(tidyverse)

folder_name <- "0131_annuity_or_lump_sum"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

annuity_monthly <- 1800
ret_retire <- 0.04/12
n_months_retire <- 30*12

lump_sum <- 155000

ret_work <- 0.06/12
n_months_work <- 17*12

fv <- fv(r = ret_work, n = n_months_work, pv = lump_sum, pmt = 0, type = 0)


# ############################  End  ################################## #