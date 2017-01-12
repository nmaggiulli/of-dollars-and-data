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
                                             "Hourly median wage",
                                             "Hourly 75th percentile wage",
                                             "Hourly 90th percentile wage"),
                        str_trim(value) !=  "-") %>%
                        select(year, value, occupation_name, datatype_name, footnote_codes)

# Treat the datatype variable as a factor
bls_oe_filtered$datatype_name <- factor(bls_oe_filtered$datatype_name,
                                         levels = c("Hourly 10th percentile wage", 
                                                    "Hourly 25th percentile wage", 
                                                    "Hourly median wage",
                                                    "Hourly 75th percentile wage",
                                                    "Hourly 90th percentile wage"))

# bls_oe_filtered2 <- filter(bls_oe,
#                           datatype_name %in% c("Employment",
#                                                "Employment per 1,000 jobs"),
#                           str_trim(value) !=  "-") 


# ############################  End  ################################## #