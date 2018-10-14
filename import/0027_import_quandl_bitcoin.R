cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

# Import bitcoin price in USD (note that my API key is imported in the header.R file)
bcoin       <- Quandl("BCHAIN/MKPRU")  %>%
                  filter(Value > 0) %>%
                  mutate(value = Value,
                         date = Date) %>%
                  arrange(date) %>%
                  select(date, value) 

saveRDS(bcoin, paste0(localdir, "27-quandl-bitcoin.Rds"))

# ############################  End  ################################## #