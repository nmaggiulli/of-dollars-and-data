cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

spy <- read.csv(paste0(importdir, "0077_spy_volatility/spy_vol_tot_return.csv")) %>%
          arrange(Period) %>%
          mutate(date = as.Date(Period, format = "%Y-%m-%d"),
                 ret_spy = `SPDR.S.P.500.ETF.Total.Return.Price`/lag(`SPDR.S.P.500.ETF.Total.Return.Price`) - 1,
                 vol_spy = `SPDR.S.P.500.ETF.30.Day.Rolling.Volatility`) %>%
          select(date, ret_spy, vol_spy) %>%
          filter(!is.na(vol_spy)) %>%
          mutate(vol_bucket = case_when(
            vol_spy <= 8 ~ "<=8",
            vol_spy < 16 ~ "8-16",
            vol_spy < 24 ~ "16-24",
            vol_spy < 32 ~ "24-32",
            vol_spy < 40 ~ "32-40",
            vol_spy < 48 ~ "40-48",
            vol_spy >= 48 ~ ">=48"
          ),
          ret_pos = ifelse(ret_spy > 0, "Positive Return", "Negative Return")) %>%
          mutate(vol_bucket = factor(vol_bucket, levels = c("<=8", "8-16", "16-24", "24-32", "32-40", "40-48", ">=48")))

saveRDS(spy, paste0(localdir, "0077_spy_volatility.Rds"))

# ############################  End  ################################## #