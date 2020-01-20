cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

Quandl.api_key("PNidzaHfNknqXchLKsRT")

# Import bitcoin price in USD (note that my API key is imported in the header.R file)
bcoin       <- Quandl("BITFINEX/BTCUSD")  %>%
                  filter(Last > 0) %>%
                  mutate(value = Last,
                         date = Date) %>%
                  arrange(date) %>%
                  select(date, value) 

saveRDS(bcoin, paste0(localdir, "0027_quandl_bitcoin.Rds"))

# ############################  End  ################################## #