cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(ggmap)

########################## Start Program Here ######################### #

# Load in data
ue_stack <- readRDS(paste0(localdir, "11-bls-ue.Rds"))

# Filter the data to be only for annual unemployment rates and for states
ue_stack <- filter(ue_stack, period == "M13", area_type_code == "A", measure_code == "03")



# ############################  End  ################################## #