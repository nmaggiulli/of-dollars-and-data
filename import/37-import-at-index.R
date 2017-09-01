cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

import_and_index <- function(filename, savename){
  temp <- read.csv(paste0(importdir, "37-at-index/", filename)) %>%
            arrange(year)
  for (i in 2:nrow(temp)){
    temp[i, "ret"] <- temp[(i), "price"]/temp[(i-1), "price"] - 1
  }
  saveRDS(temp, paste0(localdir, savename))
}

import_and_index("wheat-prices-farmdoc.csv", "37-wheat-prices-1966-2016.Rds")
import_and_index("avocado-prices-usda-ycharts.csv", "37-avocado-prices-1966-2016.Rds")




# ############################  End  ################################## #