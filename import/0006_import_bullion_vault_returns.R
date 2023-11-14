cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(readr)

########################## Start Program Here ######################### #

# Returns are taken from this locaton by BuillionVault
# http://banners.bullionvault.com/en/us-annual-asset-performance-comparison-1976-2015.pdf
#

bv_returns <- read_csv(paste0(importdir, "0006_bullion_vault_asset_returns/asset-returns-bullion-vault-1976.csv"))

# Make all of the returns into real returns
# Create a function
convert_to_real <- function(var){
  var_string <- var
  bv_returns[, var_string] <- bv_returns[, var_string] - bv_returns[, "CPI"]
  assign("bv_returns", bv_returns, envir = .GlobalEnv)
}

# Get a var_list to loop through
var_list <- colnames(bv_returns)

# Remove year and CPI from the looping list
var_list <- var_list[var_list != "year"]
var_list <- var_list[var_list != "CPI"] 

# Loop through the list of vars to convert the returns to real returns
for (x in var_list){
  convert_to_real(x)
}

# Remove CPI as it is not an asset
bv_returns[, "CPI"] <- NULL

bv_returns$year <- as.Date(bv_returns$year, "%d/%m/%y")

# Save down the data
saveRDS(bv_returns, paste0(localdir, "0006_bv_returns.Rds"))



# ############################  End  ################################## #