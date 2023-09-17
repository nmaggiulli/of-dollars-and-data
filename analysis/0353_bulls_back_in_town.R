cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0353_bulls_back_in_town"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Data is only through May 31, 2023 NOT 12/31/23
raw <- read.csv(paste0(importdir, "/", folder_name, "/sp500_mcap_2017_2023_9-17-2023.csv"),
                skip = 6, col.names = c("symbol", "name", "metric", "value_2023_12_31", "value_2022_12_31", "value_2021_12_31",
                                        "value_2020_12_31", "value_2019_12_31", 
                                        "value_2018_12_31", "value_2017_12_31", "value_2016_12_31")) %>%
              mutate(value_add_2023 = value_2023_12_31 - value_2022_12_31,
                     value_add_2021 = value_2021_12_31 - value_2020_12_31,
                     value_add_2020 = value_2020_12_31 - value_2019_12_31,
                     value_add_2019 = value_2019_12_31 - value_2018_12_31,
                     value_add_2017 = value_2017_12_31 - value_2016_12_31) %>%
                filter(symbol != "GOOGL",
                       !is.na(value_2022_12_31))

overall_by_year <- raw %>%
                    summarize(value_add_2023 = sum(value_add_2023, na.rm = TRUE),
                              value_add_2021 = sum(value_add_2021, na.rm = TRUE),
                              value_add_2020 = sum(value_add_2020, na.rm = TRUE),
                              value_add_2019 = sum(value_add_2019, na.rm = TRUE),
                              value_add_2017 = sum(value_add_2017, na.rm = TRUE))

# You can't do 2018 or 2022 (cause the change was negative)
# Do 2023 for checking purposes
years <- c(2017, 2019, 2020, 2021, 2023)

for(y in years){
  colname <- paste0("value_add_", y)
  
  value_add <- overall_by_year[1, colname]
  
  df <- raw %>%
          rename_(.dots = setNames(paste0(colname), "value_add")) %>%
          arrange(desc(value_add))
  
  sum_total <- 0
  n_companies <- 1
  while(sum_total < value_add){
    new_value_add <- df[n_companies, "value_add"]
    if(n_companies == 1){
      sum_total <- new_value_add
    } else{
      sum_total <- sum_total + new_value_add
    }
    
    n_companies <- n_companies + 1
  }
  
  print(paste0("In ", y, ", ", n_companies, " companies represented the entire gain in the S&P 500."))
}


# ############################  End  ################################## #