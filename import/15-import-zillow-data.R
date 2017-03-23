cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Create tempfile to download zip data into
temp <- tempfile()
download.file("http://files.zillowstatic.com/research/public/Metro.zip",temp)

# List the files before selecting which ones to keep
unzip(temp, list = TRUE)

import_file <- function(path, name){
  test <- read.table(unz(temp, paste0("Metro/Metro_", path)), sep = ",", header = TRUE)
  saveRDS(test, paste0(importdir, "15-zillow-home-price/metro_", name, ".Rds"))
}

# Keep median sold price per square foot and median rental price per square foot
import_file("MedianSoldPricePerSqft_AllHomes.csv", "mediansold_sqft_all")
import_file("MedianSoldPrice_AllHomes.csv", "mediansold_all")
import_file("Zhvi_TopTier.csv", "zhvi_toptier")
import_file("Zhvi_BottomTier.csv", "zhvi_bottomtier")
import_file("Zhvi_MiddleTier.csv", "zhvi_middletier")

unlink(temp)


# ############################  End  ################################## #