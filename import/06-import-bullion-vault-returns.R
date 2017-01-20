cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(readxl)

########################## Start Program Here ######################### #

bv_returns <- read_excel(paste0(importdir, "06-bullion-vault-asset-returns/asset-returns-bullion-vault-1976-2015.xlsx"))

# Make all of the returns into real returns
# Create a function
convert_to_real <- function(var){
  var_string <- var
  bv_returns[, var_string] <- bv_returns[, var_string] - bv_returns[, "us_cpi"]
  assign("bv_returns", bv_returns, envir = .GlobalEnv)
}

# Get a var_list to loop through
var_list <- colnames(bv_returns)

# Remove year and CPI from the looping list
var_list <- var_list[var_list != "year"]
var_list <- var_list[var_list != "us_cpi"] 

# Loop through the list of vars to convert the returns to real returns
for (x in var_list){
  convert_to_real(x)
}

# Remove CPI as it is not an asset
bv_returns[, "us_cpi"] <- NULL

# Save down the data
saveRDS(bv_returns, paste0(localdir, "06-bv-returns.Rds"))



# ############################  End  ################################## #