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
                  2009, 2011, 2013, 2015, 2017, 2019, 2021)

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
                        
full_psid_supp <- full_psid %>%
                    left_join(supp_stack) %>%
                    left_join(cpi_deflator) %>%
                    mutate(networth = case_when(
                      year < 2009 ~ supp_wealth/cpi_deflator,
                      TRUE ~ wealth/cpi_deflator
                    ),
                    wealth_level = case_when(
                      networth < 10000 ~ "L1 (<$10k)",
                      floor(log10(networth)) == 4 ~ "L2 ($10k)",
                      floor(log10(networth)) == 5 ~ "L3 ($100k)",
                      floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                      floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                      floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                      TRUE ~ "ERROR"
                    )) %>%
                    select(year, interview, weight, networth, wealth_level, faminc, hvalue,
                           ID1968, age, educ)

full_psid_supp$wealth_level <- factor(full_psid_supp$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                    "L3 ($100k)", "L4 ($1M)",
                                                                    "L5 ($10M)", "L6 ($100M+)"))

missing_nw_id_to_remove <- full_psid_supp %>%
                          filter(is.na(networth)) %>%
                          pull(ID1968)

full_data <- full_psid_supp %>%
            filter(ID1968 != missing_nw_id_to_remove) 
            
test_1984_1994 <- full_data %>%
  filter(year %in% c(1984, 1994), weight > 0) %>%
  group_by(year, wealth_level, ID1968) %>%
  summarise(total_weight = sum(weight)) %>%
  ungroup() %>%
  mutate(percentage = total_weight / sum(total_weight)) %>%
  arrange(ID1968, year) %>%
  mutate(level_change = case_when(
    ID1968 != lead(ID1968) ~ "",
    wealth_level == lead(wealth_level) ~ "0 (No Change)",
    wealth_level < lead(wealth_level) ~ "1 (Up Level)",
    TRUE ~ "-1 (Down Level)"
  ))



# ############################  End  ################################## #