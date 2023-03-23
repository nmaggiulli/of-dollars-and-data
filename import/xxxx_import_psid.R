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
library(lodown)
library(tidyverse)

########################## Start Program Here ######################### #

pw <- read_excel(paste0(importdir, "/0000_credentials/psid_creds.xlsx"))

psid_cat <- get_catalog( "psid" ,
                         output_dir = file.path( path.expand( "~" ) , "PSID" ) , 
                         your_email = pw$email, 
                         your_password = pw$pw)

to_download <- psid_cat %>%
            filter(table_name == "Cross-year Individual: 1968-2019")

lodown( "psid" , to_download , 
                    your_email = pw$email, 
                    your_password = pw$pw)

raw <- readRDS(paste0("/Users/nmaggiulli/PSID/family files/cross-year individual 1968-2019.Rds"))

head <- raw %>%
          head(2) %>%
          select(er77451)
              