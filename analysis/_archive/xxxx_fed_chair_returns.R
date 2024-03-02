cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Bitcoin data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/import/0027_import_quandl_bitcoin.R")))
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(quantmod)
library(ggjoy)
library(tidyr)
library(readxl)
library(dplyr)

folder_name <- "xxxx_fed_chair_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

fed_chairs <- read_excel(paste0(importdir, "/xxxx_fed_chairs/fed_chair_history.xlsx"))

dow_daily <- read_excel(paste0(importdir, "/xxxx_fed_chairs/Dow daily 2020.xlsx"), col_names = c("date", "index_dow")) %>%
                mutate(ret = index_dow/lag(index_dow) - 1) %>%
                drop_na()

final_results <- data.frame()

for(i in 1:nrow(fed_chairs)){
  s_date <- fed_chairs[i, "start_date"]
  e_date <- fed_chairs[i, "end_date"]
  name <- fed_chairs[i, "name"]
  
  mean_ret <- dow_daily %>%
          filter(date >= s_date, date <= e_date) %>%
          summarise(mean_ret = mean(ret)) %>%
          ungroup() %>%
          pull(mean_ret)
  
  final_results[i, "name"] <- name
  final_results[i, "start_date"] <- s_date
  final_results[i, "end_date"] <- e_date
  final_results[i, "mean_ret"] <- mean_ret
}

export_to_excel(df = final_results,
                outfile = paste0(out_path, "/fed_chair_mean_daily_return.xlsx"),
                sheetname = "results",
                new_file = 1,
                fancy_formatting = 1)

# ############################  End  ################################## #