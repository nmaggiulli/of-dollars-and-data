cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)

########################## Start Program Here ######################### #

folder_name <- "15-zillow-home-price/"

build_data <- function(file_name){
  temp       <- readRDS(paste0(importdir, folder_name, file_name,".Rds"))
  temp2      <- gather(temp, "year", "price", 4:ncol(temp))
  temp2$year <- as.numeric(gsub("X","", temp2$year))
  saveRDS(temp2, paste0(localdir, "15_", file_name, ".Rds"))
}

build_data("state_mediansold_sqft_all")
build_data("state_mediansold_all")
build_data("state_zhvi_toptier")
build_data("state_zhvi_bottomtier")
build_data("state_zhvi_middletier")



# ############################  End  ################################## #