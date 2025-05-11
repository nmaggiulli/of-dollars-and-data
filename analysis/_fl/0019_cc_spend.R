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

amex_2025 <- read_excel(paste0(local, "/amex_01/amex_2025.xlsx"), skip = 6) %>%
                clean_cols()

amex_autopays <- amex_2025 %>%
              filter(grepl("AUTOPAY", description)) %>%
              mutate(date = as.Date(date, format = "%m/%d/%Y"),
                     month_date = date_to_month(date),
                     total_balance = -1*amount,
                     source = "amex",
                     purchaser = "01") %>%
              select(month_date, total_balance, source, purchaser)

apple_01_list <- data.frame(file_name = list.files(path = paste0(local, "/apple_01/"), recursive = TRUE)) %>%
  filter(grepl(".pdf", file_name))

extract_statement_date <- function(filename) {
  # Extract "Month Year" from filename
  date_str <- sub(".* - ([A-Za-z]+ \\d{4})\\.pdf$", "\\1", filename)
  
  # Convert to Date object as first of the month
  date_obj <- as.Date(paste0("01 ", date_str), format = "%d %B %Y")
  
  return(date_obj)
}

apple_01_data <- data.frame()
counter <- 1

for(i in 1:nrow(apple_01_list)){
  fname <- apple_01_list[i, "file_name"]
  print(fname)
  pdf_in <- paste0(local, "/apple_01/", fname)
  
  text <- data.frame(line = pdf_text(pdf_in))
  
  apple_balance_regex <- ".*?Previous Total Balance.*?Total Balance\\s+(\\$\\d+,?\\d+\\.\\d\\d).*"
  
  total_balance <- gsub(apple_balance_regex, "\\1", text[1, "line"])
  
  fname_date <- extract_statement_date(fname)
  
  apple_01_data[counter, "month_date"] <- fname_date
  apple_01_data[counter, "total_balance"] <- dollar_to_numeric(total_balance)
  apple_01_data[counter, "source"] <- "apple"
  apple_01_data[counter, "purchaser"] <- "01"
  
  counter <- counter + 1
}

#Do Amazon
amazon_01_list <- data.frame(file_name = list.files(path = paste0(local, "/amazon_01/"), recursive = TRUE)) %>%
  filter(grepl(".pdf", file_name))

amazon_01_data <- data.frame()
counter <- 1

for(i in 1:nrow(amazon_01_list)){
  fname <- amazon_01_list[i, "file_name"]
  print(fname)
  pdf_in <- paste0(local, "/amazon_01/", fname)
  
  text <- data.frame(line = pdf_text(pdf_in))
  
  chase_balance_regex <- ".*?New Balance\\s+(\\$\\d+,?\\.\\d+).*"
  chase_date_regex <- ".*?New Balance\\\n\\s+(\\w+\\s\\d{4}).*"
  
  total_balance <- gsub(chase_balance_regex, "\\1", text[1, "line"])
  month_year_str <- gsub(chase_date_regex, "\\1", text[1, "line"])
  
  amazon_01_data[counter, "month_date"] <- as.Date(paste0("01 ", month_year_str), format = "%d %B %Y")
  amazon_01_data[counter, "total_balance"] <- dollar_to_numeric(total_balance)
  amazon_01_data[counter, "source"] <- "amazon"
  amazon_01_data[counter, "purchaser"] <- "01"
  
  counter <- counter + 1
}

#Do other chase
chase_01_list <- data.frame(file_name = list.files(path = paste0(local, "/chase_01/"), recursive = TRUE)) %>%
  filter(grepl(".pdf", file_name))

chase_01_data <- data.frame()
counter <- 1

for(i in 1:nrow(chase_01_list)){
  fname <- chase_01_list[i, "file_name"]
  print(fname)
  pdf_in <- paste0(local, "/chase_01/", fname)
  
  text <- data.frame(line = pdf_text(pdf_in))
  
  chase_balance_regex <- ".*?New Balance\\s+(\\$\\d+,?\\.\\d+).*"
  chase_date_regex <- ".*?New Balance\\\n\\s+(\\w+\\s\\d{4}).*"
  
  total_balance <- gsub(chase_balance_regex, "\\1", text[1, "line"])
  month_year_str <- gsub(chase_date_regex, "\\1", text[1, "line"])
  
  chase_01_data[counter, "month_date"] <- as.Date(paste0("01 ", month_year_str), format = "%d %B %Y")
  chase_01_data[counter, "total_balance"] <- dollar_to_numeric(total_balance)
  chase_01_data[counter, "source"] <- "chase"
  chase_01_data[counter, "purchaser"] <- "01"
  
  counter <- counter + 1
}

# Final bind row
final <- amex_autopays %>%
          bind_rows(apple_01_data) %>%
          bind_rows(amazon_01_data) %>%
          bind_rows(chase_01_data) %>%
          arrange(source, month_date)

summary <- final %>%
            group_by(purchaser) %>%
            summarise(total_spend = sum(total_balance)) %>%
            ungroup()

# ############################  End  ################################## #