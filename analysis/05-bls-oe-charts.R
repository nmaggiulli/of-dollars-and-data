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

########################## Start Program Here ######################### #

# Load data fom local library
bls_oe <- readRDS(paste0(localdir, "05-bls-oe.Rds"))

# Filter the data to the national level and for hourly pay percentiles
bls_oe_filtered <- filter(bls_oe, areatype_name == "National",
                        datatype_name %in% c("Hourly 10th percentile wage", 
                                             "Hourly 25th percentile wage",
                                             "Hourly 50th percentile wage",
                                             "Hourly 75th percentile wage",
                                             "Hourly 90th percentile wage"),
                        str_trim(value) !=  "-")


# ############################  End  ################################## #