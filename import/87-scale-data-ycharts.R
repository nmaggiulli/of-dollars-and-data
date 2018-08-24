cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(stringr)
library(readxl)
library(slackr)
library(tidyverse)

folder_name <- "87-scale-data-ycharts"

########################## Start Program Here ######################### #

read_in_ycharts <- function(filename){
df <- read.csv(paste0(importdir, folder_name, "/", filename)) %>%
      gather(key=key, value=value, -Period) %>%
        mutate(year = substr(Period, 1, 4),
               company = gsub("(.*?)(\\.Inc|\\.Corp).*", "\\1", key),
               measure = gsub(".*?(Inc\\.|Corp\\.)(.*?)\\.\\.Annual\\.", "\\2", key)
        ) %>%
        filter(!is.na(value)) %>%
        group_by(year, company, measure) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        spread(key=measure, value=value) %>%
        ungroup()
  return(df)
}

er <- read_in_ycharts("employees_rev.csv")
ai <- read_in_ycharts("assets_netinc.csv")

df <- er %>% full_join(ai)

saveRDS(df, paste0(localdir, "87-employee-rev-ycharts.Rds"))


# ############################  End  ################################## #