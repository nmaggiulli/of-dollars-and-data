cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(jsonlite)
library(zoo)
library(readxl)
library(lubridate)
library(quantmod)
library(Hmisc)
library(scales)
library(tidyverse)

folder_name <- "_calculators/0006_income_by_age"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2025

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(year == data_year,
         age >= 20,
         age <= 79) %>%
  mutate(agecl = case_when(
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    TRUE ~ "75-79"))

df <- scf_stack %>%
  select(hh_id, imp_id, 
         income, wgt, 
         agecl) %>%
  arrange(hh_id, imp_id)

pcts <- seq(0.01, 0.99, 0.01)

final_pct_stack <- data.frame()

for(p in pcts){
  all_tmp <- df %>%
    summarise(
      pct = wtd.quantile(income, weights = wgt, probs=p)
    ) %>%
    mutate(agecl = "All Ages") %>%
    gather(-agecl, key=key, value=value) %>%
    mutate(pct = p) %>%
    select(-key)
  
  tmp <- df %>%
    group_by(agecl) %>%
    summarise(
      pct = wtd.quantile(income, weights = wgt, probs=p)
    ) %>%
    ungroup() %>%
    gather(-agecl, key=key, value=value) %>%
    mutate(pct = p) %>%
    select(-key)
  
  if(p == min(pcts)){
    final_pct_stack <- all_tmp %>% bind_rows(tmp)
  } else{
    final_pct_stack <- final_pct_stack %>% bind_rows(all_tmp, tmp)
  }
}

saveRDS(final_pct_stack, paste0(localdir, "/income_by_age_percentiles.Rds"))


to_calc <- readRDS(paste0(localdir, "/income_by_age_percentiles.Rds"))
# ---------------------------------------------------------------------------
# Output: ONE data file, income_data.js
#
# Upload it to wp-content/themes/odad/calculators/ (replace the old one), then
# flush Flywheel and Cloudflare. The calculator code, the age group list and the
# "Data from the [year] Survey of Consumer Finances" line all come from the
# theme + this file, so no HTML or JavaScript needs to change.
# ---------------------------------------------------------------------------

groups <- unique(to_calc$agecl)   # "All Ages" first, then the age groups

pct_values <- to_calc %>%
  filter(agecl == groups[1]) %>%
  arrange(pct) %>%
  pull(pct)

data_list <- list(
  year   = data_year,
  groups = groups,
  pct    = round(pct_values * 100),
  values = setNames(lapply(groups, function(g) {
    to_calc %>% filter(agecl == g) %>% arrange(pct) %>% pull(value) %>% round(4)
  }), groups)
)

# Every age group needs one value per percentile
stopifnot(all(sapply(data_list$values, length) == length(data_list$pct)))

json_data <- toJSON(data_list, auto_unbox = TRUE, digits = NA)

writeLines(paste0("window.INCOME_DATA = ", json_data, ";"),
           paste0(out_path, "/income_data.js"))

print(paste0("Wrote income_data.js for ", data_year, " (", length(groups), " groups)"))

# ############################  End  ################################## #