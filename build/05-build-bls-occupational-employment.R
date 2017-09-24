cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(stringr)

########################## Start Program Here ######################### #

# Load in raw BLS productivity data
bls_oe <-readRDS(paste0(importdir, "05-bls-occupational-employment/bls_oe_data.1.AllData.Rds"))

# Load in other datasets and create a code based on their row number
# Will use these datasets to merge to the main productivity dataset
create_index <- function(string){
  name            <- deparse(substitute(string))
  temp            <- readRDS(paste0(importdir, "05-bls-occupational-employment/bls_oe_", name, ".Rds"))
  new_col         <- paste0(name, "_name")
  old_col         <- paste0(name, "_code")
  temp[, new_col] <-  temp[, old_col]
  temp[, old_col] <- rownames(temp)
  temp            <- temp[, c(old_col, new_col)]
  return(temp)
}

areatype    <- create_index(areatype)
area        <- create_index(area)
industry    <- create_index(industry)
occupation  <- create_index(occupation)
datatype    <- create_index(datatype)

# Parse the series ID based on the "pr.txt" file here:  https://download.bls.gov/pub/time.series/oe/
bls_oe <- mutate(bls_oe, 
                 areatype_code    = substr(series_id, 4, 4),
                 area_code        = substr(series_id, 5, 11),
                 industry_code    = substr(series_id, 12, 17),
                 occupation_code  = substr(series_id, 18, 23),
                 datatype_code    = substr(series_id, 24, 25))

# Pad the area data with leading zeroes on area_code
area$area_code <- str_pad(area$area_code, 7, pad = "0")

# Merge on the area, areatype, industry, occupation, and datatype information
bls_oe <- bls_oe                %>%
          left_join(areatype)   %>%
          left_join(area)       %>%
          left_join(industry)   %>%
          left_join(occupation) %>%
          left_join(datatype)   %>%
            select(series_id, year, value, footnote_codes, area_name,
                  areatype_name, industry_name, occupation_name, datatype_name)

# Save down final build before doing analysis
saveRDS(bls_oe, paste0(localdir, "05-bls-oe.Rds"))


# ############################  End  ################################## #