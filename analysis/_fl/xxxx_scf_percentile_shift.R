cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(Hmisc)
library(tidyverse)

########################## Start Program Here ######################### #

nw_target <- 800000

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# 1. Percentile of $800k for ages 30-34 in 2022 (weighted)
df_2022 <- scf_stack %>%
  filter(year == 2022, age >= 30, age <= 34)

pct <- sum(df_2022$wgt[df_2022$networth <= nw_target]) / sum(df_2022$wgt)

print(paste0("2022, age 30-34: $800k is the ",
             round(100 * pct, 1), "th percentile"))

# 2. That same percentile for ages 33-37 in 2025
df_2025 <- scf_stack %>%
  filter(year == 2025, age >= 33, age <= 37)

nw_2025 <- as.numeric(wtd.quantile(df_2025$networth, weights = df_2025$wgt,
                                   probs = pct))

print(paste0("2025, age 33-37: ", round(100 * pct, 1),
             "th percentile net worth is ", format_as_dollar(nw_2025)))