cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(Hmisc)
library(tidyverse)

folder_name <- "0310_who_holds_student_loan_debt"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2019
subset_agecls <- c("<35", "35-44", "45-54")
college_edcls <- c("Some College", "College Degree")

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(year == data_year)

scf_college_w_loan <- scf_stack %>%
  filter(edcl %in% college_edcls, payedu > 0, agecl %in% subset_agecls) %>%
  select(hh_id, imp_id, wgt,
         income, payedu, edcl, agecl) %>%
  arrange(hh_id, imp_id)

biden_cutoff_pct <- scf_college_w_loan %>%
  summarise(
    pct_95 = wtd.quantile(income, weights = wgt, probs=0.95)
  ) %>%
  ungroup() %>%
  gather(key=key, value=value)

#5% of households with at least some college and a student loan have income >$250k
# So let's exclude these and then find the income breakdown

debt <- scf_stack %>%
  filter(income < 250000, 
         agecl %in% subset_agecls,
         edcl %in% college_edcls,
         payedu > 0) %>%
  select(hh_id, imp_id, wgt, networth,
         income, payedu, edcl, agecl) %>%
  arrange(hh_id, imp_id)

#Understand the ages of eligible HHs
debt_age_summary <- debt %>%
                          select(hh_id, agecl) %>%
                          distinct() %>%
                          group_by(agecl) %>%
                          summarise(n_hhs = n()) %>%
                          ungroup()

# From this, find the ineligible comparator groups  
create_eligibility_group <- function(name, filter_string){

  nodebt_tmp <- scf_stack %>%
    filter(income < 250000,
           agecl %in% subset_agecls) %>%
    filter_(filter_string) %>%
    select(hh_id, imp_id, wgt, networth,
           income) %>%
    arrange(hh_id, imp_id)
  
  nodebt_tmp_hhs <- nodebt_tmp %>%
    select(hh_id, wgt) %>%
    summarise(sum_wgt = sum(wgt))
  
  assign(paste0(name, "_hhs"), nodebt_tmp_hhs, envir = .GlobalEnv)
  
  nodebt_inc_nw <- nodebt_tmp %>%
    summarise(
      pct_25_income = wtd.quantile(income, weights = wgt, probs=0.25),
      pct_50_income = wtd.quantile(income, weights = wgt, probs=0.5),
      pct_75_income = wtd.quantile(income, weights = wgt, probs=0.75),
      pct_25_nw = wtd.quantile(networth, weights = wgt, probs=0.25),
      pct_50_nw = wtd.quantile(networth, weights = wgt, probs=0.5),
      pct_75_nw = wtd.quantile(networth, weights = wgt, probs=0.75),
    ) %>%
    ungroup() %>%
    gather(key=key, value=value) %>%
    rename_(.dots = setNames("value", name))
  
  assign(paste0(name, "_inc_nw"), nodebt_inc_nw, envir = .GlobalEnv)
}

create_eligibility_group("debt_college", "edcl == 'College Degree' & payedu > 0")
create_eligibility_group("debt_college_some", "edcl == 'Some College' & payedu > 0")
create_eligibility_group("nodebt_college", "edcl == 'College Degree' & payedu == 0")
create_eligibility_group("nodebt_nocollege", "edcl != 'College Degree' & payedu == 0")

all_hh_wgt <- debt_college_hhs$sum_wgt + debt_college_some_hhs$sum_wgt + nodebt_college_hhs$sum_wgt  + nodebt_nocollege_hhs$sum_wgt

export_inc <- debt_college_inc_nw %>%
              left_join(debt_college_some_inc_nw) %>%
              left_join(nodebt_college_inc_nw) %>%
              left_join(nodebt_nocollege_inc_nw) %>%
              rename(measure = key) %>%
              gather(-measure, key=key, value=value) %>%
              spread(measure, value) %>%
              mutate(pct_hhs = case_when(
                key == "debt_college" ~ debt_college_hhs$sum_wgt/all_hh_wgt,
                key == "debt_college_some" ~ debt_college_some_hhs$sum_wgt/all_hh_wgt,
                key == "nodebt_college" ~ nodebt_college_hhs$sum_wgt/all_hh_wgt,
                key == "nodebt_nocollege" ~ nodebt_nocollege_hhs$sum_wgt/all_hh_wgt,
                TRUE ~ debt_college_hhs$sum_wgt/all_hh_wgt
              )) %>%
              select(key, pct_hhs, pct_25_income, pct_50_income, pct_75_income) %>%
              arrange(desc(pct_50_income))

export_nw <- debt_college_inc_nw %>%
  left_join(debt_college_some_inc_nw) %>%
  left_join(nodebt_college_inc_nw) %>%
  left_join(nodebt_nocollege_inc_nw) %>%
  rename(measure = key) %>%
  gather(-measure, key=key, value=value) %>%
  spread(measure, value) %>%
  mutate(pct_hhs = case_when(
    key == "debt_college" ~ debt_college_hhs$sum_wgt/all_hh_wgt,
    key == "debt_college_some" ~ debt_college_some_hhs$sum_wgt/all_hh_wgt,
    key == "nodebt_college" ~ nodebt_college_hhs$sum_wgt/all_hh_wgt,
    key == "nodebt_nocollege" ~ nodebt_nocollege_hhs$sum_wgt/all_hh_wgt,
    TRUE ~ debt_college_hhs$sum_wgt/all_hh_wgt
  )) %>%
  select(key, pct_hhs, pct_25_nw, pct_50_nw, pct_75_nw) %>%
  arrange(desc(pct_50_nw))

to_export <- export_to_excel(df = export_inc,
                             outfile = paste0(out_path, "/student_debt_stats_by_edc.xlsx"),
                             sheetname = "inc",
                             new_file = 1,
                             fancy_formatting = 0)

to_export <- export_to_excel(df = export_nw,
                             outfile = paste0(out_path, "/student_debt_stats_by_edc.xlsx"),
                             sheetname = "nw",
                             new_file = 0,
                             fancy_formatting = 0)



# ############################  End  ################################## #