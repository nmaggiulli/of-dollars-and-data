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
df <- read_excel(paste0(importdir, folder_name, "/", filename)) %>%
        gather(key=key, value=value, -year) %>%
        filter(!is.na(value)) %>%
        group_by(year, key) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        separate(key, sep= "_", into = c("company", "measure")) %>%
        spread(key=measure, value=value) %>%
        ungroup()
  return(df)
}

er <- read_in_ycharts("employees_rev.xlsx")
ai <- read_in_ycharts("assets_netinc.xlsx")

df <- er %>% full_join(ai)

saveRDS(df, paste0(localdir, "87-employee-rev-ycharts.Rds"))


# ############################  End  ################################## #