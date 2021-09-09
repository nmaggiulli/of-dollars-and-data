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
library(tidylog)
library(tidyverse)

folder_name <- "0259_tsa_passenger_throughput"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bts_data <- read_excel(paste0(importdir, "/0259_never_forget/bts_gov_Passengers_9_9_2021 10_59_06 AM.xlsx")) %>%
              clean_cols() %>%
              filter(month != "TOTAL") %>%
              mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
              select(date, domestic)

start_date <- as.Date("2001-10-01")
end_date <- as.Date("2021-08-01")

df <- data.frame(date = seq.Date(start_date, end_date, "month")) %>%
          left_join(bts_data) %>%
          fill(domestic, .direction = c("updown"))

format_as_dollar(sum(df$domestic)*15*5, 1)

# ############################  End  ################################## #