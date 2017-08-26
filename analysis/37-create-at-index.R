cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(magrittr)
library(dplyr)

########################## Start Program Here ######################### #

# Read in AT data
avc <- readRDS(paste0(localdir, "37-avocado-prices-1966-2016.Rds"))
wheat <- readRDS(paste0(localdir, "37-wheat-prices-1966-2016.Rds"))

# ############################  End  ################################## #