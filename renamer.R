cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(tidyverse)

########################## Start Program Here ######################### #

renamer <- function(folder){

root_folder <- paste0(programroot, folder)  
  
list <- data.frame(file_name = list.files(path = root_folder)) %>%
  mutate(lead_number = gsub("(\\d+[a-z]?)-.*", "\\1", file_name),
    new_file_name = paste0(root_folder, "/00", str_replace_all(file_name, "-", "_")),
    file_name = paste0(root_folder, "/", file_name)) 

for (i in 1:nrow(list)){
  orig <- list[i, "file_name"]
  new <- list[i, "new_file_name"]
  
  file.rename(orig, new)
}

}

renamer("build")


# ############################  End  ################################## #