cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(stringr)
library(readxl)
library(slackr)
library(tidyverse)

folder_name <- "87-employee-rev-ycharts"

########################## Start Program Here ######################### #

df <- read.csv(paste0(importdir, folder_name, "/raw.csv"))


# ############################  End  ################################## #