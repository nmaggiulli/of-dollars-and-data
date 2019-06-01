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
library(tidyverse)

folder_name <- "0129_amzn_nasdaq"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0129_ycharts_amzn_nasdaq/AMZN_IXIC_data.csv"))

colnames(raw) <- c("date", "index_amzn", "index_nq")

raw <- raw %>%
        filter(!is.na(index_amzn), !is.na(index_nq)) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
        filter(date >= "1999-05-28") %>%
        arrange(date)

dd_amzn <- drawdown_path(select(raw, date, index_amzn))
dd_nq <- drawdown_path(select(raw, date, index_nq))

# ############################  End  ################################## #