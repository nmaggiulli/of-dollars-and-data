cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

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

# ############################  End  ################################## #

# Load in BV returns
bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds"))

# Define the number of years
n_years <- nrow(bv_returns)

# Define the number of simulations
n_simulations <- 10000

# Create a simulation vector
sim_vec <- seq(1, n_years, 1)