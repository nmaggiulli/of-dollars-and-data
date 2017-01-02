cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Load 2013 SCF data into memory
load(paste0(importdir,"03-scf-data/scf2013.rda"))

# Subset to only the variables we care about in the data
vars_to_keep <- c( 'y1' , 'yy1' , 'wgt' , 'one' , 'five' , 'networth' , 'checking' , 'hdebt' , 'agecl' , 'hhsex' , 'race' )

# Write a function to subset our data
subset_data <- function(name){
  data <- get(name)
  data <- data[ , vars_to_keep]
  assign(name, data, envir = .GlobalEnv)
}

# Loop through each of the imp datasets to subset them
for (i in 1:5){
  string <- paste0("imp", i)
  subset_data(string)
}




# ############################  End  ################################## #