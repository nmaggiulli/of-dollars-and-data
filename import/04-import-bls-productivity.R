cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Read in productivity data from tab-delimited text file on the web
bls_productivity <- read.csv("https://download.bls.gov/pub/time.series/pr/pr.data.1.AllData", 
                             header = TRUE, 
                             sep = "\t")

saveRDS(bls_productivity, paste0(importdir, "04-bls-productivity/bls_productivity.Rds"))

# ############################  End  ################################## #