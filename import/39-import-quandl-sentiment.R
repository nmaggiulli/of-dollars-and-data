cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

# Import bitcoin price in USD (note that my API key is imported in the header.R file)
aaii       <- Quandl("AAII/AAII_SENTIMENT")  %>%
                  mutate(date = Date,
                         bullish = Bullish,
                         bearish = Bearish) %>%
                  arrange(date) %>%
                  select(date, bullish, bearish) 

saveRDS(aaii, paste0(localdir, "39-quandl-aaii-sentiment.Rds"))

# ############################  End  ################################## #