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

folder_name <- "_twl/0004_asset_breakdown_by_wealth_level"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# asset = value of all assets (fin + nfin)

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
              mutate(`Business Interests` = bus/asset,
                     `Real Estate` = (oresre+nnresre)/asset,
                     `Primary Residence` = houses/asset,
                     `Vehicles` = vehic/asset,
                     `Retirement` =  retqliq/asset,
                     `Stocks & Mutual Funds` = (nmmf + stocks)/asset, 
                     `Cash` = liq/asset,
                     `Other` = (savbnd + othfin + othnfin + cashli + othma + bond + cds)/asset,
                     liquid_assets = asset - reteq - nfin,
                     liquid_networth = networth - reteq - nfin,
                     owns_home = ifelse(houses > 0, 1, 0),
                     wealth_level = case_when(
                       networth < 10000 ~ "L1 (<$10k)",
                       floor(log10(networth)) == 4 ~ "L2 ($10k)",
                       floor(log10(networth)) == 5 ~ "L3 ($100k)",
                       floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                       floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                       floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                             TRUE ~ "ERROR"
                     )) %>%
                select(wealth_level, networth, liquid_networth, liquid_assets, income, `Business Interests`, `Real Estate`,`Primary Residence`,
                       `Vehicles`, `Retirement`,
                       `Stocks & Mutual Funds`, `Cash`, `Other`,
                       owns_home,
                       wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                      "L3 ($100k)", "L4 ($1M)",
                                                                      "L5 ($10M)", "L6 ($100M+)"))


income_200k_plus <- scf_stack %>%
              filter(income > 200000)

wealth_dist_200k_plus <- income_200k_plus %>%
                      summarise(pct05_nw = wtd.quantile(networth, weights = wgt, probs = 0.05),
                        pct10_nw = wtd.quantile(networth, weights = wgt, probs = 0.1),
                        pct25_nw = wtd.quantile(networth, weights = wgt, probs = 0.25),
                        median_nw = wtd.quantile(networth, weights = wgt, probs = 0.5),
                        pct75_nw = wtd.quantile(networth, weights = wgt, probs = 0.75))

income_200k_250k <- scf_stack %>%
  filter(income > 200000, income < 250000)


wealth_dist_200k_250k <- income_200k_250k %>%
  summarise(pct05_nw = wtd.quantile(networth, weights = wgt, probs = 0.05),
            pct10_nw = wtd.quantile(networth, weights = wgt, probs = 0.1),
            pct25_nw = wtd.quantile(networth, weights = wgt, probs = 0.25),
            median_nw = wtd.quantile(networth, weights = wgt, probs = 0.5),
            pct75_nw = wtd.quantile(networth, weights = wgt, probs = 0.75))

income_500k_750k <- scf_stack %>%
  filter(income > 500000, income < 750000)

wealth_dist_500k_750k <- income_500k_750k %>%
  summarise(pct01_nw = wtd.quantile(networth, weights = wgt, probs = 0.01),
            pct05_nw = wtd.quantile(networth, weights = wgt, probs = 0.05),
            pct10_nw = wtd.quantile(networth, weights = wgt, probs = 0.1),
            pct25_nw = wtd.quantile(networth, weights = wgt, probs = 0.25),
            median_nw = wtd.quantile(networth, weights = wgt, probs = 0.5),
            pct75_nw = wtd.quantile(networth, weights = wgt, probs = 0.75))


# ############################  End  ################################## #