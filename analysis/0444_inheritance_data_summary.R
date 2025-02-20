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
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0444_inheritance_data_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Import UPenn data
# https://budgetmodel.wharton.upenn.edu/issues/2021/7/16/inheritances-by-age-and-income-group

raw <- read_excel(paste0(importdir, "/0444_inheritance_data/upenn_inheritance_data_scf.xlsx")) %>%
          clean_cols() %>%
          rename(`<26` = under_26) 

long <- raw %>%
          gather(-income_group, key=key, value=value)

pre_plot <- long %>%
              filter(income_group == "All Incomes" | key == "all_ages",
                     !(income_group == "All Incomes" & key == "all_ages"))

to_plot <- pre_plot %>%
            filter(income_group == "All Incomes")

## Add plot ##

to_plot <- pre_plot %>%
  filter( key == "all_ages")

## Add plot ##

## Export heatmap data
for_heatmap <- raw %>%
                select(-all_ages) %>%
                filter(income_group != "All Incomes")

export_to_excel(df = for_heatmap,
                outfile = paste0(out_path, "/inheritance_data_for_heatmap.xlsx"),
                sheetname = "heatmap",
                new_file = 1,
                fancy_formatting = 0)

# ############################  End  ################################## #