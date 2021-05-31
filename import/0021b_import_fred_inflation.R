cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(quantmod)
library(lubridate)
library(dplyr)

########################## Start Program Here ######################### #

# Get the inflation data
getSymbols('CPIAUCNS',src='FRED')

cpi <- data.frame(date = index(CPIAUCNS), 
                  CPIAUCNS, row.names=NULL)

saveRDS(cpi, paste0(localdir, "0021_FRED_cpi_monthly.Rds"))

cpi_annual <- cpi %>%
          mutate(year = year(date),
                 month = month(date)) %>%
          filter(month == 12) %>%
          mutate(rate_cpi = CPIAUCNS/lag(CPIAUCNS) - 1,
                 index_cpi = CPIAUCNS) %>%
          select(year, index_cpi, rate_cpi)

saveRDS(cpi_annual, paste0(localdir, "0021_FRED_cpi.Rds"))



# ############################  End  ################################## #