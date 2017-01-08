cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(zoo)

########################## Start Program Here ######################### #

# Load in raw BLS productivity data
bls_productivity <-readRDS(paste0(importdir, "04-bls-productivity/bls_productivity_data.1.AllData.Rds"))

# Load in other datasets and create a code based on their row number
# Will use these datasets to merge to the main productivity dataset
create_index <- function(string){
  name            <- deparse(substitute(string))
  temp            <- readRDS(paste0(importdir, "04-bls-productivity/bls_productivity_", name, ".Rds"))
  new_col         <- paste0(name, "_name")
  old_col         <- paste0(name, "_code")
  temp[, new_col] <-  temp[, old_col]
  temp[, old_col] <- rownames(temp)
  temp            <- temp[, c(old_col, new_col)]
  return(temp)
}

sector      <- create_index(sector)
measure     <- create_index(measure)
class       <- create_index(class)
duration    <- create_index(duration)

# Parse the series ID based on the "pr.txt" file here:  https://download.bls.gov/pub/time.series/pr/
bls_productivity <- mutate(bls_productivity, sector_code = substr(series_id, 4, 7),
                           class_code = substr(series_id, 8, 8),
                           measure_code = substr(series_id, 9, 10),
                           duration_code = substr(series_id, 11, 11),
                           year_period = paste0(year, "-", period))

# Convert the year_period to year_quarter using the zoo package
# Those with a NA year_quarter (period = Q05) are annual averages
bls_productivity$year_quarter <- as.Date(as.yearqtr(bls_productivity$year_period, format = "%Y-Q%q"))

# Merge on the sector, measure, and class information
bls_productivity <- bls_productivity %>%
                  left_join(sector) %>%
                    left_join(measure) %>%
                      left_join(class) %>%
                        left_join(duration) %>%
                    select(series_id, year_quarter, year, value, 
                           sector_name, measure_name, class_name, duration_name)

# Save down final build before doing analysis
saveRDS(bls_productivity, paste0(localdir, "04-bls-productivity.Rds"))


# ############################  End  ################################## #