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

folder_name <- "0155_ind_stocks_rebalance"

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir,folder_name, "/SPX 2005 Returns Data.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  filter(symbol != "", !is.na(value)) %>%
  select(date, year, month, symbol, name, value) %>%
  arrange(symbol, date)

mat <- raw %>%
          select(symbol, value) %>%
          as.matrix() 

mat <- cbind(mat, c(0))

for(i in 1:nrow(mat)){
  print(i)
  if(i == 1){
    mat[i, 3] <- 1
  } else{
    if(mat[i, 1] == mat[(i-1), 1]){
      mat[i, 3] <- as.numeric(mat[(i-1), 3]) * (1 + as.numeric(mat[i, 2]))
    } else{
      mat[i, 3] <- 1
    }
  }
}

mat <- cbind(mat, raw[, "date"])

df <- raw %>%
        cbind(mat[,3]) %>%
        mutate(index = as.numeric(`mat[, 3]`),
               year = as.numeric(year),
               month = as.numeric(month)) %>%
        rename(ret = value) %>%
        select(date, year, month, symbol, name, index, ret)

saveRDS(df, paste0(localdir, "0155_ind_stocks_ycharts.Rds"))


# ############################  End  ################################## #