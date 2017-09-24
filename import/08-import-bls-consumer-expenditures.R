cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

# Read in productivity data from tab-delimited text file on the web
# Use function to make this dynamic
read_in_bls <- function(string){
  name      <- deparse(substitute(string))
  temp_name <- read.csv(paste0("https://download.bls.gov/pub/time.series/cx/cx.", name), 
                             header = TRUE, 
                             sep = "\t", row.names = NULL)
  
  saveRDS(temp_name, paste0(importdir, "08-bls-consumer-expenditures/bls_cx_", name, ".Rds"))
}


read_in_bls(data.1.AllData)
read_in_bls(category)
read_in_bls(demographics)
read_in_bls(subcategory)
read_in_bls(characteristics)
read_in_bls(item)

# ############################  End  ################################## #