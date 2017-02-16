cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

# Load in the data
load(paste0(importdir, "10-psid-cross-year/ind_ind2013er.rda"))



# ############################  End  ################################## #