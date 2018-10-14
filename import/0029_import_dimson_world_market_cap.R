cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

temp <- read.csv(paste0(importdir, "29-market-capitalization-world/triump-of-the-optimists-dimson-market-cap.csv"))

temp$region <- as.character(temp$region)

saveRDS(temp, paste0(localdir, "29-dimson-world-market-cap.Rds"))

# ############################  End  ################################## #