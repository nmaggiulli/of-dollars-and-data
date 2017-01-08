cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Read in productivity data from tab-delimited text file on the web
# Use function to make this dynamic
read_in_bls <- function(string){
  name      <- deparse(substitute(string))
  temp_name <- read.csv(paste0("https://download.bls.gov/pub/time.series/pr/pr.", name), 
                             header = TRUE, 
                             sep = "\t")
  
  saveRDS(temp_name, paste0(importdir, "04-bls-productivity/bls_productivity_", name, ".Rds"))
}

read_in_bls(data.1.AllData)
read_in_bls(class)
read_in_bls(measure)
read_in_bls(sector)
read_in_bls(series)
read_in_bls(duration)





# ############################  End  ################################## #