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

folder_name <- "_twl/0005a_age_by_wealth_level"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# asset = value of all assets (fin + nfin)

# Add Wealth levels
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
              mutate(wealth_level = case_when(
                            networth < 10000 ~ "L1 (<$10k)",
                            floor(log10(networth)) == 4 ~ "L2 ($10k)",
                            floor(log10(networth)) == 5 ~ "L3 ($100k)",
                            floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                            floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                            floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                            TRUE ~ "ERROR"
                          ),
                     new_agecl = case_when(
                       age < 20 ~ "<20",
                       age < 30 ~ "20s",
                       age < 40 ~ "30s",
                       age < 50 ~ "40s",
                       age < 60 ~ "50s",
                       age < 70 ~ "60s",
                       TRUE ~ "70+"
                     )) %>%
                select(wealth_level, networth, age, new_agecl, wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                    "L3 ($100k)", "L4 ($1M)",
                                                                    "L5 ($10M)", "L6 ($100M+)"))

avg_age_by_wealth_lvl <- scf_stack %>%
              group_by(wealth_level) %>%
              summarise(average_age = wtd.mean(age, weights = wgt)) %>%
              ungroup()

#Calculate the age distribution across each wealth level
age_percentile_summary_by_level <- scf_stack %>%
  group_by(wealth_level) %>%
  summarise(age = wtd.quantile(age, 
                               weights = wgt, 
                               probs = seq(0.01, 0.99, 0.01)
  )
  ) %>%
  ungroup() %>%
  mutate(percentile = rep(seq(1, 99), 6))

pct_10_25_50_age_by_wealth_level <- age_percentile_summary_by_level %>%
            filter(percentile %in% c(10, 25, 50))

pct_1 <- age_percentile_summary_by_level %>%
  filter(percentile == 1)

# Now calculate wealth by age cohort
nw_summary_by_age <- scf_stack %>%
  filter(new_agecl != "<20") %>%
  group_by(new_agecl) %>%
  summarise(networth = wtd.quantile(networth, 
                               weights = wgt, 
                               probs = seq(0.01, 0.99, 0.01)
  )
  ) %>%
  ungroup() %>%
  mutate(percentile = rep(seq(1, 99), 6),
         wealth_level = case_when(
           networth < 10000 ~ 1,
           floor(log10(networth)) == 4 ~ 2,
           floor(log10(networth)) == 5 ~ 3,
           floor(log10(networth)) == 6 ~ 4,  
           floor(log10(networth)) == 7 ~ 5,  
           floor(log10(networth)) > 7 ~ 6, 
           TRUE ~ -999
         ))

nw_level_summary <- nw_summary_by_age %>%
                      filter(wealth_level != lag(wealth_level)) %>%
                      arrange(new_agecl, wealth_level) %>%
                      mutate(next_percentile = percentile - lag(percentile))

# ############################  End  ################################## #