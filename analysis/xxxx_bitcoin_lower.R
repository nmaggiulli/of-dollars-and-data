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
library(tidyverse)

folder_name <- "xxxx_bitcoin_lower"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bcoin <- readRDS(paste0(localdir, "0027_quandl_bitcoin.Rds")) %>%
  mutate(day = day(date)) %>%
  select(date, value)

df <- bcoin %>%
      mutate(lower_30 = ifelse(value > lead(value, 30), 1, 0),
             lower_90 = ifelse(value > lead(value, 90), 1, 0),
             lower_365 = ifelse(value > lead(value, 365), 1, 0)) %>%
      filter(date >= "2018-01-01")

summary <- df %>%
            summarize(lower_30 = mean(lower_30, na.rm = TRUE),
                      lower_90 = mean(lower_90, na.rm = TRUE),
                      lower_365 = mean(lower_365, na.rm = TRUE))
  

# ############################  End  ################################## #