cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0242_levered_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow_pre_2020 <- read_excel(paste0(importdir, "/0242_levered_returns/Dow daily 2020.xlsx"),
                           col_names = c("date", "index")) %>%
                  mutate(date = as.Date(date)) %>%
                  filter(year(date) < 2020)

dow_2020 <- read.csv(paste0(importdir, "/0242_levered_returns/DJI_data.csv"),
                       col.names = c("date", "index")) %>%
              mutate(date = as.Date(date)) %>%
              filter(year(date) == 2020)

df <- dow_pre_2020 %>%
        bind_rows(dow_2020) %>%
        arrange(date) %>%
        mutate(ret = index/lag(index, 1) - 1,
               ret_2x = ret*2,
               ret_3x = ret*3)

for(i in 1:nrow(df)){
  
}

# ############################  End  ################################## #