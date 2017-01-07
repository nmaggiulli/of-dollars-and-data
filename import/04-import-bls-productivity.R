cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Data was found here:  https://download.bls.gov/pub/time.series/pr/

# Read in productivity data from tab-delimited text file
bls_productivity <- read.table(paste0(importdir, "04-bls-productivity/pr.data.1.AllData.txt"), 
                               header = TRUE,
                               sep = "\t")

saveRDS(bls_productivity, paste0(localdir, "04-raw-bls-productivity"))

# ############################  End  ################################## #