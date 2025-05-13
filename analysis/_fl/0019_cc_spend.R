cat("\014") # Clear your console
rm(list = ls()) #clear your enviro01ent

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(readxl)
library(pdftools)
library(magrittr)
library(ggrepel)

folder_name <- "xxxx_cc_spend"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

local <- "/Users/nmaggiulli/Desktop/cc_spend"

extract_statement_date <- function(filename) {
  # Extract "Month Year" from filename
  date_str <- sub(".* - ([A-Za-z]+ \\d{4})\\.pdf$", "\\1", filename)
  
  # Convert to Date object as first of the month
  date_obj <- as.Date(paste0("01 ", date_str), format = "%d %B %Y")
  
  return(date_obj)
}

amex_2025 <- read_excel(paste0(local, "/amex_01/amex_2025.xlsx"), skip = 6) %>%
                clean_cols()

amex_autopays <- amex_2025 %>%
              filter(grepl("AUTOPAY", description)) %>%
              mutate(date = as.Date(date, format = "%m/%d/%Y"),
                     month_date = date_to_month(date),
                     total_spend = -1*amount,
                     source = "amex",
                     user = "01") %>%
              select(month_date, total_spend, source, user)

apple_01_list <- data.frame(file_name = list.files(path = paste0(local, "/apple_01/"), recursive = TRUE)) %>%
  filter(grepl(".pdf", file_name))

apple_01_data <- data.frame()
counter <- 1

for(i in 1:nrow(apple_01_list)){
  fname <- apple_01_list[i, "file_name"]
  print(fname)
  pdf_in <- paste0(local, "/apple_01/", fname)
  
  text <- data.frame(line = pdf_text(pdf_in))
  
  apple_balance_regex <- ".*?Previous Total Balance.*?Total Balance\\s+(\\$\\d+,?\\d+\\.\\d\\d).*"
  
  total_spend <- gsub(apple_balance_regex, "\\1", text[1, "line"])
  
  fname_date <- extract_statement_date(fname)
  
  apple_01_data[counter, "month_date"] <- fname_date
  apple_01_data[counter, "total_spend"] <- dollar_to_numeric(total_spend)
  apple_01_data[counter, "source"] <- "apple"
  apple_01_data[counter, "user"] <- "01"
  
  counter <- counter + 1
}

import_chase <- function(folder_name, source, user, balance_regex, date_regex){
  tmp_01_list <- data.frame(file_name = list.files(path = paste0(local, "/", folder_name), recursive = TRUE)) %>%
    filter(grepl(".pdf", file_name))
  
  chase_01_data <- data.frame()
  counter <- 1
  
  for(i in 1:nrow(tmp_01_list)){
    fname <- tmp_01_list[i, "file_name"]
    pdf_in <- paste0(local, "/", folder_name, "/", fname)
    
    text <- data.frame(line = pdf_text(pdf_in))
  
    total_spend <- gsub(balance_regex, "\\1", text[1, "line"])
    month_year_str <- gsub(date_regex, "\\1", text[1, "line"])
    
    chase_01_data[counter, "month_date"] <- as.Date(paste0("01 ", month_year_str), format = "%d %B %Y")
    chase_01_data[counter, "total_spend"] <- dollar_to_numeric(total_spend)
    chase_01_data[counter, "source"] <- source
    chase_01_data[counter, "user"] <- user
    
    counter <- counter + 1
  }
  
  return(chase_01_data)
}

# Import chase statements
chase_balance_regex1 <- ".*?New Balance\\s+(\\$\\d+,?\\d+\\.\\d+).*"
chase_date_regex1 <- ".*?New Balance\\\n\\s+(\\w+\\s\\d{4}).*"
amazon_01_data <- import_chase("amazon_01", "amazon", "01", chase_balance_regex1, chase_date_regex1)
chase_01_data <- import_chase("chase_01", "chase", "01", chase_balance_regex1, chase_date_regex1)
amazon_02_data <- import_chase("amazon_02", "amazon", "02", chase_balance_regex1, chase_date_regex1)
chase_fu_02_data <- import_chase("chase_fu_02", "chase_fu", "02", chase_balance_regex1, chase_date_regex1)

chase_balance_regex2 <- ".*?ULTIMATE\\sREWARDS®\\\n\\s+(\\$\\d+,?\\d+\\.\\d+).*"
chase_sp_02_data <- import_chase("chase_sp_02", "chase_sp", "02", chase_balance_regex2, chase_date_regex1)

chase_balance_regex3 <- ".*?UNITED\\sMILEAGEPLUS\\sAWARD\\\n\\s+(\\$\\d+,?\\d+\\.\\d+).*"
united_02_data <- import_chase("united_02", "united", "02", chase_balance_regex3, chase_date_regex1)

#Manual fixes
amazon_02_data <- amazon_02_data %>%
                    mutate(total_spend = case_when(
                      month_date == "2025-03-01" ~ 1169.24,
                      TRUE ~ total_spend
                    ))

#import other
other <- read_excel(paste0(local, "/other.xlsx"), skip = 0) %>%
  clean_cols()

income <- read_excel(paste0(local, "/income.xlsx"), skip = 0) %>%
  clean_cols()

# Final bind row
by_month <- amex_autopays %>%
          bind_rows(apple_01_data) %>%
          bind_rows(amazon_01_data) %>%
          bind_rows(chase_01_data) %>%
          bind_rows(amazon_02_data) %>%
          bind_rows(chase_fu_02_data) %>%
          bind_rows(chase_sp_02_data) %>%
          bind_rows(united_02_data) %>%
          bind_rows(other) %>%
          bind_rows(income) %>%
          arrange(user, source, month_date)

counts <- by_month %>%
            group_by(user, source) %>%
            summarize(n_months = n(),
                      min_month = min(month_date)) %>%
            ungroup()

summary <- by_month %>%
            group_by(user) %>%
            summarise(total_spend = sum(total_spend, na.rm = TRUE),
                      total_income = sum(total_income, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(net_income = total_income - total_spend)

average_net <- sum(summary$net_income)/2

final <- summary %>%
              mutate(net_payment = average_net - net_income)

# ############################  End  ################################## #