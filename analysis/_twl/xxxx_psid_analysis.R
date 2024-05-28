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
library(psidR)
library(data.table)
library(quantmod)
library(tidyverse)

folder_name <- "_twl/xxxx_psid_analysis"

out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

psid_path <- paste0(importdir, "_twl/0001_psid/")
r <- system.file(package="psidR")
cwf <- openxlsx::read.xlsx(file.path(r,"psid-lists","psid.xlsx"))

wealth_years <- c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007,
                  2009, 2011, 2013, 2015, 2017)

# Select vars we care about and the years we care about
f <- fread(file.path(r,"psid-lists","famvars.txt")) %>%
        filter(name %in% c("faminc", "hvalue", "wealth"),
               year %in% wealth_years)
i <- fread(file.path(r,"psid-lists","indvars.txt")) %>%
        filter(name != "empstat",
               year %in% wealth_years)

# Reformat the shape
i <- dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f <- dcast(f[,list(year,name,variable)],year~name, value.var = "variable")

# Build the full panel data (we will add 1984-2007 after)
full_psid <- build.panel(datadir=psid_path,
                fam.vars=f,
                ind.vars=i,
                heads.only = TRUE,
                sample = "SRC",
                design="balanced") %>%
            rename()

#Loop through years 1984-2007
supplemental_years <- wealth_years[wealth_years < 2009]

for(s in supplemental_years){
  print(s)
  # Specify the name of the text file within the ZIP directory
  zip_file <- paste0(psid_path, "wlth", s, ".zip")
  txt_file <- paste0("WLTH", s, ".txt")
  
  # Open a connection to the ZIP file
  zip_connection <- unz(zip_file, txt_file)
  
  psid_supp_cols <- read_excel(paste0(psid_path, "psid_supp_wealth_columns.xlsx")) %>%
                      filter(year == s) %>%
                      select(year, colnum, width, colname)
  
  widths <- psid_supp_cols %>%
              arrange(colnum) %>%
              select(width) %>%
              pull()
  
  colnames <- psid_supp_cols %>%
                  arrange(colnum) %>%
                  select(colname) %>%
                  pull()
  
  tmp2 <- read.fwf(zip_connection, 
                   widths,
                   col.names = colnames,
                  header = FALSE, 
                  skip = 0) %>%
            mutate(year = s) %>%
            select(year, interview, supp_wealth)
  
  if(s == min(supplemental_years)){
    supp_stack <- tmp2
  } else{
    supp_stack <- supp_stack %>% bind_rows(tmp2)
  }
}

#Get CPI data for inflation adjustment
getSymbols('CPIAUCNS',src='FRED')

cpi_all_years <- data.frame(date=index(get("CPIAUCNS")), coredata(get("CPIAUCNS"))) %>%
  rename(cpi = `CPIAUCNS`) %>%
  mutate(year = year(date)) %>%
  filter(month(date) == 1, year %in% wealth_years) %>%
  select(year, cpi)

cpi_deflator <- cpi_all_years %>%
                  mutate(cpi_deflator = cpi/cpi_all_years[1, "cpi"]) %>%
                  select(year, cpi_deflator)
                        
# Join all data sources and deflate by CPI
full_psid_supp <- full_psid %>%
                    left_join(supp_stack) %>%
                    left_join(cpi_deflator) %>%
                    mutate(networth = case_when(
                      year < 2009 ~ supp_wealth/cpi_deflator,
                      TRUE ~ wealth/cpi_deflator
                    ),
                    wealth_level = case_when(
                      networth < 10000 ~ 1,
                      floor(log10(networth)) == 4 ~ 2,
                      floor(log10(networth)) == 5 ~ 3,
                      floor(log10(networth)) == 6 ~ 4,  
                      floor(log10(networth)) == 7 ~ 5,  
                      floor(log10(networth)) > 7 ~ 6, 
                      TRUE ~ NA
                    )) %>%
                    select(year, interview, weight, pernum, networth, wealth_level, faminc, hvalue,
                           ID1968, age, educ)

missing_nw_id_to_remove <- full_psid_supp %>%
                          filter(is.na(networth)) %>%
                          pull(ID1968)

# Now just keep one observation per HH
full_data <- full_psid_supp %>%
            filter(ID1968 != missing_nw_id_to_remove) %>%
            arrange(ID1968, year, pernum) %>%
            group_by(year, ID1968) %>%
            slice(1) %>%
            ungroup()

#Loop through start and end years
years_df <- data.frame(
   start_year = c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007),
   end_year = c(1994, 1999, 2003, 2009, 2011, 2013, 2015, 2017)
)

for(i in 1:nrow(years_df)){
  start_yr <- years_df[i, "start_year"]
  end_yr <- years_df[i, "end_year"]
  
  start_data <- full_data %>%
    filter(year == start_yr, weight > 0) %>%
    select(year, weight, ID1968, wealth_level) %>%
    rename(start_level = wealth_level)
  
  end_data <- full_data %>%
    filter(year == end_yr, weight > 0) %>%
    select(ID1968, wealth_level) %>%
    rename(end_level = wealth_level)
  
  merged_level_comparison <- start_data %>%
                              left_join(end_data)
  
  tmp_level_weight <- merged_level_comparison %>%
                      group_by(start_level) %>%
                      summarise(level_weight = sum(weight)) %>%
                      ungroup()
  
  tmp_levels_summary <- merged_level_comparison %>%
                      group_by(start_level, end_level) %>%
                      summarise(n_hhs = n(),
                                total_weight = sum(weight)) %>%
                      ungroup() %>%
                      left_join(tmp_level_weight) %>%
                      mutate(level_pct = total_weight / level_weight,
                             all_pct = total_weight / sum(total_weight),
                             start_year = start_yr,
                             end_year = end_yr)
  
  tmp_change_summary <- merged_level_comparison %>%
    mutate(level_change = end_level - start_level) %>%
    group_by(level_change) %>%
    summarise(n_hhs = n(),
              total_weight = sum(weight)) %>%
    ungroup() %>%
    mutate(all_pct = total_weight / sum(total_weight),
           start_year = start_yr,
           end_year = end_yr)
  
  if(start_yr == min(years_df$start_year)){
    levels_stack <- tmp_levels_summary
    change_stack <- tmp_change_summary
  } else{
    levels_stack <- levels_stack %>% bind_rows(tmp_levels_summary)
    change_stack <- change_stack %>% bind_rows(tmp_change_summary)
  }
}

#Summarize the summaries
all_levels_summary <- levels_stack %>%
                      group_by(start_level, end_level) %>%
                      summarise(level_pct = mean(level_pct),
                                all_pct = mean(all_pct)) %>%
                      ungroup()

all_change_summary <- change_stack %>%
                        group_by(level_change) %>%
                        summarise(all_pct = mean(all_pct)) %>%
                        ungroup()

# ############################  End  ################################## #