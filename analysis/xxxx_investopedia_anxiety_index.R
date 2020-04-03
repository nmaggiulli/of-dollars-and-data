cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(readxl)
library(jsonlite)
library(tidyverse)

folder_name <- "xxxx_investopedia_anxiety_index"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

test <- fromJSON("https://inv-anxiety-index.s3.amazonaws.com/iai.json")

iai <- data.frame(as.list(test[[2]])) %>%
        gather(key=key, value=value) %>%
        rename(index_iai = value) %>%
        mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
              month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
              day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
              date = as.Date(paste0(year, "-", month, "-", day))) %>%
        select(date, index_iai)

spxtr <- read.csv(paste0(importdir, "/0177_ycharts_spxtr_daily/SPXTR_data.csv"),
                col.names = c("date","index_sp500")) %>%
              mutate(date = as.Date(date)) %>%
              arrange(date) 

vix <- read.csv(paste0(importdir, "/0177_ycharts_spxtr_daily/VIX_data.csv"),
                  col.names = c("date","index_vix")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) 

df <- iai %>%
        left_join(spxtr) %>%
        left_join(vix) %>%
        drop_na() 
            
ggplot(df, aes(x=date, y=index_iai)) +
  geom_line()

# ############################  End  ################################## #