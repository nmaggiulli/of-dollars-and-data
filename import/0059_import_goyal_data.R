cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(readxl)
library(httr)

########################## Start Program Here ######################### #

GET('http://www.hec.unil.ch/agoyal/docs/PredictorData2016.xlsx', write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf, sheet="Monthly")

saveRDS(df, paste0(importdir, "59-goyal-data/58-goyal-stock-bond-data.Rds"))

# ############################  End  ################################## #