cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(stringr)

########################## Start Program Here ######################### #

# Create list of datasets to loop through
loop_list <- c("data.0.CurrentU90-94",
               "data.0.CurrentU95-99", 
               "data.0.CurrentU00-04",
               "data.0.CurrentU05-09",
               "data.0.CurrentU10-14",
               "data.0.CurrentU15-19")

# Load in raw BLS UE data and stack it
for (i in loop_list){
  if (i == "data.0.CurrentU90-94"){
    ue_stack <- readRDS(paste0(importdir, "12-bls-unemployment/bls_unemployment_", i, ".Rds"))
  } else{
    temp <- readRDS(paste0(importdir, "12-bls-unemployment/bls_unemployment_", i, ".Rds"))
    ue_stack <- rbind(ue_stack, temp)
  }
}

# Load in the area and area_type
area <- readRDS(paste0(importdir, "12-bls-unemployment/bls_unemployment_area.Rds"))

area <- select(area, area_type_code, area_code, row.names) %>%
          mutate(area_text = area_code, 
                 area_code = area_type_code,
                 area_type_code = row.names) %>%
            select(area_code, area_text, area_type_code)

area_type <- readRDS(paste0(importdir, "12-bls-unemployment/bls_unemployment_area_type.Rds"))

area_type <- mutate(area_type, area_type_text = area_type_code,
                    area_type_code = rownames(area_type)) %>%
            select(area_type_text, area_type_code)

areas <- area %>%
          left_join(area_type) %>%
          select(area_code, area_text, area_type_text, area_type_code)

# Load in measure data as well
measure <- readRDS(paste0(importdir, "12-bls-unemployment/bls_unemployment_measure.Rds"))
measure <- mutate(measure, 
                  measure_text = measure_code,
                  measure_code = rownames(measure))

# Define the area on the ue stack
ue_stack <- mutate(ue_stack, 
                   area_code = substr(series_id, 4, 18),
                   measure_code = substr(series_id, 19, 20))

# Join the area data to the stack
ue_stack <- ue_stack %>%
              left_join(areas) %>%
              left_join(measure)

# Save down RDS
saveRDS(ue_stack, paste0(localdir, "12-bls-ue.Rds"))

# ############################  End  ################################## #