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

# Keep median sold price per square foot and median rental price per square foot
state_mediansold_sqft_all <- read.table(unz(temp, "State/State_MedianSoldPricePerSqft_AllHomes.csv"))
state_mediansold_all      <- read.table(unz(temp, "State/State_MedianSoldPrice_AllHomes.csv"))
state_rentalprice_all     <- read.table(unz(temp, "State/State_MedianRentalPrice_AllHomes.csv"))
state_rentalprice_1bed    <- read.table(unz(temp, "State/State_MedianRentalPrice_1Bedroom.csv"))

unlink(temp)


# ############################  End  ################################## #