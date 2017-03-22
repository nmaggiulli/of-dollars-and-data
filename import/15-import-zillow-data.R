cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Create tempfile to download zip data into
temp <- tempfile()
download.file("http://files.zillowstatic.com/research/public/State.zip",temp)

# List the files before selecting which ones to keep
unzip(temp, list = TRUE)

import_file <- function(path, name){
  test <- read.table(unz(temp, paste0("State/", path)), sep = ",", header = TRUE)
  saveRDS(test, paste0(importdir, "15-zillow-home-price/", name, ".Rds"))
}

# Keep median sold price per square foot and median rental price per square foot
import_file("State_MedianSoldPricePerSqft_AllHomes.csv", "state_mediansold_sqft_all")
import_file("State_MedianSoldPrice_AllHomes.csv", "state_mediansold_all")
import_file("State_Zhvi_TopTier.csv", "state_zhvi_toptier")
import_file("State_Zhvi_BottomTier.csv", "state_zhvi_bottomtier")
import_file("State_Zhvi_MiddleTier.csv", "state_zhvi_middletier")

unlink(temp)


# ############################  End  ################################## #