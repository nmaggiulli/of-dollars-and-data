cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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

cpi <- cpi %>%
          mutate(year = year(date),
                 month = month(date)) %>%
          filter(month == 12) %>%
          mutate(rate_cpi = CPIAUCNS/lag(CPIAUCNS) - 1) %>%
          select(year, rate_cpi)

saveRDS(cpi, paste0(localdir, "21-FRED-cpi.Rds"))



# ############################  End  ################################## #