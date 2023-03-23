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
library(ipumsr)
library(tidyverse)

########################## Start Program Here ######################### #

ddi <- read_ipums_ddi(paste0(importdir, "xxxx_ipums_data/cps_00002.xml"))
ipums <- read_ipums_micro(ddi) %>%
            clean_cols() %>%
            filter(inctot != 999999999,
                   pernum == 1) %>%
            select(year, serial, asecwt, inctot, pernum) %>%
            distinct()

summary <- ipums %>%
  summarise(pct = wtd.quantile(inctot, weights = asecwt, probs=c(0.95)))

              