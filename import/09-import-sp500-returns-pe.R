cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(readxl)
library(dplyr)

########################## Start Program Here ######################### #

# Bring in the data Shiller uses on his website
# Build the data in this step
# Also drop the 2017 data as the dividends are missing
sp500_ret_pe <- read_excel(paste0(importdir, "09-sp500-returns-pe/ie_data.xls"),
                          sheet = "cleaned") %>%
                  filter(!is.na(real_div)) %>%
                select(Date, real_price, real_div, cape)

# Save down the data
saveRDS(sp500_ret_pe, paste0(localdir, "09-sp500-ret-pe.Rds"))

# ############################  End  ################################## #