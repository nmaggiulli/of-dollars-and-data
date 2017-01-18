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
                        industry_name == "Cross-industry, Private Ownership Only",
                        str_trim(value) !=  "-") %>%
                        select(year, value, footnote_codes, area_name, areatype_name,
                               industry_name, occupation_name, datatype_name)

# Treat the datatype variable as a factor
bls_oe_filtered$datatype_name <- factor(bls_oe_filtered$datatype_name,
                                         levels = c("Hourly 10th percentile wage", 
                                                    "Hourly 25th percentile wage", 
                                                    "Hourly median wage",
                                                    "Hourly 75th percentile wage",
                                                    "Hourly 90th percentile wage"))

bls_oe_filtered$value <- as.numeric(as.character(bls_oe_filtered$value))

# Remove pure duplicates
bls_oe_long <- distinct(bls_oe_filtered) %>%
                    unite(group, industry_name, occupation_name, datatype_name)

# Choose the lower "value" for any duplicates remaining             
bls_oe_long <- bls_oe_long[order(bls_oe_long$group, abs(bls_oe_long$value)), ]
bls_oe_long     <- bls_oe_long[!duplicated(bls_oe_long$group),]

# Recreate the original vars before dedupping
bls_oe_long     <- separate(bls_oe_long, sep = "_", group, c("industry_name", "occupation_name", "datatype_name"))

# Turn the long dataset into a wide dataset
# Also calculate the percentage diff between the top and bottom percentiles
bls_oe_wide <-  spread(bls_oe_long, datatype_name, value) %>%
                mutate(pct75_10_diff = `Hourly 75th percentile wage` / `Hourly 10th percentile wage` - 1) %>%
                arrange(pct75_10_diff)



# ############################  End  ################################## #