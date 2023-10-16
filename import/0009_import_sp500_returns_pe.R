cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(readxl)
library(lubridate)
library(tidyverse)

########################## Start Program Here ######################### #

url <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
dest_file <- paste0(importdir, "0009_sp500_returns_pe/ie_data.xls") 

download.file(url, dest_file, mode = "wb")

# Bring in the data Shiller uses on his website
# Build the data in this step
# Also drop the 2017 data as the dividends are missing
sp500_ret_pe <- read_excel(paste0(importdir, "0009_sp500_returns_pe/ie_data.xls"),
                          sheet = "Data") 

colnames(sp500_ret_pe) <- c("date", "price", "div", "earnings", "cpi", "date_frac", 
                            "long_irate", "real_price", "real_div", "real_tr",
                            "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2",
                            "excess_cape", "blank", "bond_real")

#Remove first 6 rows
sp500_ret_pe <- sp500_ret_pe[7:nrow(sp500_ret_pe),]

# Convert vars to numeric
sp500_ret_pe$real_price <- as.numeric(sp500_ret_pe$real_price)
sp500_ret_pe$real_div <- as.numeric(sp500_ret_pe$real_div)
sp500_ret_pe$real_tr <- as.numeric(sp500_ret_pe$real_tr)
sp500_ret_pe$date <- as.numeric(sp500_ret_pe$date)
sp500_ret_pe$cpi <- as.numeric(sp500_ret_pe$cpi)
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)
sp500_ret_pe$long_irate <- as.numeric(sp500_ret_pe$long_irate)
sp500_ret_pe$real_earn <- as.numeric(sp500_ret_pe$real_earn)
sp500_ret_pe$price <- as.numeric(sp500_ret_pe$price)
sp500_ret_pe$div <- as.numeric(sp500_ret_pe$div)
sp500_ret_pe$real_earn_scaled <- as.numeric(sp500_ret_pe$real_earn_scaled)
sp500_ret_pe$bond_real <- as.numeric(sp500_ret_pe$bond_real)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(Sys.Date()) + month(Sys.Date())/100

# Filter out missing dividends
sp500_ret_pe <- sp500_ret_pe %>%
                  select(date, price, div, real_price, real_div, real_earn, real_earn_scaled, real_tr, long_irate, cape, cpi, bond_real) %>%
                  filter(!is.na(date), date < end_date) %>%
                  mutate(real_div = ifelse(is.na(real_div), 0, real_div))

# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- sp500_ret_pe %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")),
    long_irate = long_irate/100,
    ret_bond_real = bond_real/lag(bond_real, 1) - 1) %>%
    rename(price_plus_div = real_tr) %>%
  select(date, price, div, real_price, real_div, real_earn, long_irate, cape, cpi, price_plus_div,  ret_bond_real)

# Save down the data
saveRDS(sp500_ret_pe, paste0(localdir, "0009_sp500_ret_pe.Rds"))

# ############################  End  ################################## #