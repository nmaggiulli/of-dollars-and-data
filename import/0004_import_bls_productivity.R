cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Read in productivity data from tab-delimited text file on the web
# Use function to make this dynamic
read_in_bls <- function(string){
  name      <- deparse(substitute(string))
  temp_name <- read.csv(paste0("https://download.bls.gov/pub/time.series/pr/pr.", name), 
                             header = TRUE, 
                             sep = "\t")
  
  saveRDS(temp_name, paste0(importdir, "0004_bls_productivity/bls_productivity_", name, ".Rds"))
}

read_in_bls(data.1.AllData)
read_in_bls(class)
read_in_bls(measure)
read_in_bls(sector)
read_in_bls(series)
read_in_bls(duration)





# ############################  End  ################################## #