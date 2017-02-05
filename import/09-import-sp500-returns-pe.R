cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(readxl)
library(dplyr)

########################## Start Program Here ######################### #

# Bring in the data Shiller uses on his website
sp500_ret_pe <- read_excel(paste0(importdir, "09-sp500-returns-pe/ie_data.xls"),
                          sheet = "cleaned") %>%
                select(Date, cape, price_plus_div)

# Save down the data
saveRDS(sp500_ret_pe, paste0(localdir, "09-sp500-ret-pe.Rds"))

# ############################  End  ################################## #