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

# Define working and retirement parameters
n_years_work <- 17
n_years_retire <- 25

pmt_annuity <- 1800*12

ret_work <- 0.05
ret_retire <- 0.05

# Calculate value of annuity at time of retirement
pv_annuity <- pv(r = ret_retire,
                 n = n_years_retire,
                 fv = 0,
                 pmt = -pmt_annuity)

# Include lump sum v
lump_sum <- -135000



fv_lump_sum <- fv(r = ret_work, n = n_years_work, pv = lump_sum, pmt = 0, type = 0)

ret_eq <- discount.rate(n = n_years_work, 
                        pv = lump_sum,
                        fv = -pv_annuity,
                        pmt = 0,
                        type=0
                        )

print(paste0("Future Value of Lump Sum is: $", formatC(fv_lump_sum, digits = 0, big.mark = ",", format = "f")))
print(paste0("Present Value of Annuity is: $", formatC(-pv_annuity, digits = 0, big.mark = ",", format = "f")))

# ############################  End  ################################## #