cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)

########################## Start Program Here ######################### #

# Load data fom local library
hf_results <- readRDS(paste0(localdir, "13-hf-correlation-results.Rds"))

# ############################  End  ################################## #
