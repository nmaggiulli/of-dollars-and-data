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
  temp2$year <- as.Date(paste0(gsub("X","", temp2$year), ".01"), format = "%Y.%m.%d")
  saveRDS(temp2, paste0(localdir, "15_", file_name, ".Rds"))
}

type <- "metro"

build_data(paste0(type,"_mediansold_sqft_all"))
build_data(paste0(type,"_mediansold_all"))
build_data(paste0(type,"_zhvi_toptier"))
build_data(paste0(type,"_zhvi_bottomtier"))
build_data(paste0(type,"_zhvi_middletier"))



# ############################  End  ################################## #