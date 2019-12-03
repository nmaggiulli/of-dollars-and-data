cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/rwm_analytics")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(rsconnect)

########################## Start Program Here ######################### #

# Connect to Shiny Apps
rsconnect::setAccountInfo(name='nmaggiulli',
                          token='13CDD50150436D65438B61CC5EB0F318',
                          secret='XXX')

# Set paths
shiny_path <- paste0("/Users/nickmaggiulli/git/of_dollars_and_data/analysis/_shiny_apps/0001_shiny_yahoo_cef_pull")
rsconnect_path <- paste0(shiny_path, "/rsconnect")

# Terminate app and delete rsconnect library
terminateApp("0001_shiny_yahoo_cef_pull", account = "nmaggiulli")

if(dir.exists(rsconnect_path)){
  unlink(rsconnect_path, recursive = TRUE)
}

# Redeploy app and then delete rsconnect folder
rsconnect::deployApp(shiny_path, account = "nmaggiulli")
Sys.sleep(5)
unlink(rsconnect_path, recursive = TRUE)

# ############################  End  ################################## #