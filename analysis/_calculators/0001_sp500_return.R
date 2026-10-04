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
library(tidyverse)

folder_name <- "_calculators/0001_sp500_return"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

download_data <- 0
today <- Sys.Date() 
filter_date <- "1871-01-01"
url <- "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/34a1781b-f073-448f-b9f9-6230becb2e49/ie_data.xls?ver=1741185530815"
dest_file <- paste0(importdir, "0009_sp500_returns_pe/ie_data.xls") 

if(download_data == 1){
  download.file(url, dest_file, mode = "wb")
}

sp500_raw <- read_excel(paste0(importdir, "0009_sp500_returns_pe/ie_data.xls"),
                        sheet = "Data") 

colnames(sp500_raw) <- c("date", "price", "dividend", "earnings", "cpi_shiller", "date_frac", 
                         "long_irate", "real_price", "real_div", "real_tr",
                         "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2")

#Remove first 6 rows
sp500_raw <- sp500_raw[7:nrow(sp500_raw),]

# Convert vars to numeric
sp500_raw$price <- as.numeric(sp500_raw$price)
sp500_raw$dividend <- as.numeric(sp500_raw$dividend)
sp500_raw$cpi_shiller <- as.numeric(sp500_raw$cpi_shiller)
sp500_raw$real_price <- as.numeric(sp500_raw$real_price)
sp500_raw$real_div <- as.numeric(sp500_raw$real_div)
sp500_raw$real_tr <- as.numeric(sp500_raw$real_tr)
sp500_raw$date <- as.numeric(sp500_raw$date)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(today) + month(today)/100

# Select specific columns
sp500_subset <- sp500_raw %>%
  select(date, price, dividend, cpi_shiller, real_div) %>%
  filter(!is.na(date), date < end_date)

# Change the Date to a Date type for plotting the S&P data
sp500_subset <- sp500_subset %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d"))) %>%
  rename(month = date,
         realDividend = real_div)

yahoo_start <- max(sp500_subset$month)
yahoo_end <- as.Date(paste0(year(today), "-", month(today), "-01")) - days(1)

#Bring in Yahoo data
getSymbols("^SPX", from = yahoo_start, to = yahoo_end, 
           src="yahoo", periodicity = "daily")

yahoo_daily <- data.frame(date=index(get("SPX")), coredata(get("SPX"))) %>%
  rename(close = `SPX.Adjusted`) %>%
  select(date, close) %>%
  mutate(month = as.Date(paste0(year(date), "-", month(date), "-01")))

yahoo_monthly <- yahoo_daily %>%
  group_by(month) %>%
  summarise(price = mean(close, na.rm = TRUE)) %>%
  ungroup()

# Get CPI
#https://fred.stlouisfed.org/series/CPIAUCNS
cpi_monthly <- read.csv(paste0(importdir, "/0009_sp500_returns_pe/CPIAUCNS.csv")) %>%
  rename(cpi_fred = `CPIAUCNS`) %>%
  mutate(month = as.Date(observation_date)) %>%
  select(month, cpi_fred)

# Join Shiller, Yahoo, and FRED
sp500_ret_pe <- sp500_subset %>%
  filter(month < yahoo_start) %>%
  bind_rows(yahoo_monthly) %>%
  left_join(cpi_monthly) %>%
  mutate(cpi = case_when(
    !is.na(cpi_fred) ~ cpi_fred,
    !is.na(cpi_shiller) ~ cpi_shiller,
    TRUE ~ NA
  )) %>%
  select(-cpi_shiller, -cpi_fred)

sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)
sp500_ret_pe$realDividend <- na.locf(sp500_ret_pe$realDividend)

#Estimate CPI data for any months Shiller/FRED is missing (use his formula)
for(i in 1:nrow(sp500_ret_pe)){
  if(is.na(sp500_ret_pe[i, "cpi"])){
    sp500_ret_pe[i, "cpi"] <- 1.5*sp500_ret_pe[(i-1), "cpi"] - 0.5*sp500_ret_pe[(i-2), "cpi"]
  }
}

final_cpi <- sp500_ret_pe[nrow(sp500_ret_pe), "cpi"]

# Calculate nominal returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
    sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[i, "realPrice"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
    sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[(i-1), "realPricePlusDividend"]*((sp500_ret_pe[i, "realPrice"] + (sp500_ret_pe[i, "realDividend"]/12))/sp500_ret_pe[(i-1), "realPrice"])
  }
}

to_calc <- sp500_ret_pe %>%
  filter(month >= filter_date) %>%
  select(month, price, nominalPricePlusDividend, realPrice, realPricePlusDividend)

# Make sure months are in order with no gaps (the calculator finds months by position)
to_calc <- to_calc %>% arrange(month)

month_num <- year(to_calc$month) * 12 + month(to_calc$month)

if (any(diff(month_num) != 1)) {
  bad <- which(diff(month_num) != 1)
  print(to_calc$month[sort(unique(c(bad, bad + 1)))])
  stop("Months are missing or duplicated around the dates printed above.")
}

# Compact data file: start/end month plus one list of numbers per series.
# The theme builds the year lists, default dates and "Data through" label from this.
data_list <- list(
  start                    = format(min(to_calc$month), "%Y-%m"),
  end                      = format(max(to_calc$month), "%Y-%m"),
  price                    = to_calc$price,
  nominalPricePlusDividend = to_calc$nominalPricePlusDividend,
  realPrice                = to_calc$realPrice,
  realPricePlusDividend    = to_calc$realPricePlusDividend
)

# digits = 4 matches the old output, so results are unchanged
json_data <- toJSON(data_list, auto_unbox = TRUE, digits = 4)

# Upload this file to wp-content/themes/odad/calculators/sp500_data.js, then purge the page in Cloudflare
writeLines(paste0("window.SP500_DATA = ", json_data, ";"),
           paste0(out_path, "/sp500_data.js"))

print(paste0("Data end month = ", data_list$end))