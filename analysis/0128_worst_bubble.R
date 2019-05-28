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

folder_name <- "0128_worst_bubble"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

fred_jpy_mcap_to_gdp <- read_excel(paste0(importdir, "0128_bubble_data/fred_mcap_to_gdp_DDDM01JPA156NWDB.xls"),
                                   skip = 10)

colnames(fred_jpy_mcap_to_gdp) <- c("date", "mcap_to_gdp")

fred_jpy_real_gdp <- read_excel(paste0(importdir, "0128_bubble_data/fred_japan_realgdp_JPNRGDPR.xls"),
                                   skip = 10)

colnames(fred_jpy_real_gdp) <- c("date", "real_gdp")

jpy <- fred_jpy_mcap_to_gdp %>%
          left_join(fred_jpy_real_gdp) %>%
          mutate(mcap_billions = mcap_to_gdp/100*real_gdp/1000) %>%
          filter(!is.na(mcap_billions)) %>%
          select(date, mcap_billions)

bcoin_mcap <- read_excel(paste0(importdir, "0128_bubble_data/bitcoin_coin_market_cap.xlsx")) %>%
                mutate(mcap_billions = mcap/(10^9)) %>%
                select(date, mcap_billions) 




# ############################  End  ################################## #