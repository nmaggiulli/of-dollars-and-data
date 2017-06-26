cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

temp <- read.csv(paste0(importdir, "29-market-capitalization-world/triump-of-the-optimists-dimson-market-cap.csv"))

temp$region <- as.character(temp$region)

saveRDS(temp, paste0(localdir, "29-dimson-world-market-cap.Rds"))

# ############################  End  ################################## #