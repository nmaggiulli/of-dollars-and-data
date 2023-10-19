cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lodown)

########################## Start Program Here ######################### #

#library(devtools)
#install_github( "ajdamico/lodown" , dependencies = TRUE )
#Program taken from here:  http://asdfree.com/survey-of-consumer-finances-scf.html

in_path <- paste0(importdir, "0003_scf_data")

download_data <- 1
if(download_data == 1){
  lodown( "scf" , output_dir = file.path(in_path , "SCF" ) )
}

# ############################  End  ################################## #