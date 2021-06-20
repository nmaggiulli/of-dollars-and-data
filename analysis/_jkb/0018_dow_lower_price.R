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

folder_name <- "_jkb/0018_dow_lower_price"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "_jkb/0018_ycharts_dow_lower_price/ycharts_dji_data.csv"), skip = 1,
                col.names = c("date","index_dow")) %>%
  mutate(date = as.Date(substr(date, 1, 10), format = "%Y-%m-%d")) %>%
  select(date, index_dow) %>%
  arrange(date) %>%
  filter(date <= "2020-03-23")

for(i in 1:nrow(raw)-1){
  val <- raw[i, "index_dow"]
  rest <- raw[(i+1):nrow(raw), "index_dow"] 
  
  lg <- val > rest
  first_below <- ifelse(min(which(lg == TRUE)) > 9999, 0, min(which(lg == TRUE)))
  raw[i, "days_to_lower"] <- first_below
  print(i)
}

all_lower_in_future <- raw %>%
                        filter(days_to_lower != 0)



# ############################  End  ################################## #