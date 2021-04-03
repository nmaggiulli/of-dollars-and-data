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

folder_name <- "_jkb/0014_grandfather_retirement"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")

birth_year <- 1938
start_year <- 1993
end_date <- as.Date("2019-05-13")
ss_age <- 62
payment <- 500

raw <- read.csv(paste0(importdir, "_jkb/0014_grandfather_retirement/SPXTR_data.csv")) %>%
          rename(index_sp500 = `S.P.500.Total.Return.Level`) %>%
          mutate(date = as.Date(Period)) %>%
          select(date, index_sp500) %>%
        arrange(date) %>%
        mutate(ret_sp500 = index_sp500/lag(index_sp500, 1) - 1) %>%
        filter(year(date) >= start_year, date < end_date)

df <- raw

for(i in 1:nrow(df)){
  ret <- df[i, "ret_sp500"]
  yr <- year(df[i, "date"])
  mt <- month(df[i, "date"])
  
  if(i == 1){
    df[i, "value_portfolio"] <- payment
  } else{
    lag_mt <- month(df[(i-1), "date"])
    
    if(mt != lag_mt & yr >= (ss_age + birth_year)){
      new_payment <- payment*2
    } else if(mt != lag_mt){
      new_payment <- payment
    }else{
      new_payment <- 0
    }
    
    df[i, "value_portfolio"] <- df[(i-1), "value_portfolio"] * (1 + ret) + new_payment
  }
}
# ############################  End  ################################## #