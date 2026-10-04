cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# ---------------------------------------------------------------------------
# ONE script for all three S&P 500 calculators:
#   S&P 500 calculator, S&P 500 DCA calculator, U.S. stock/bond calculator
#
# Writes ONE file, sp500_data.js. Each month:
#   1. Update CPIAUCNS.csv and GS10.csv from FRED (same as before)
#   2. Run this script
#   3. Upload sp500_data.js to wp-content/themes/odad/calculators/ (replace the old one)
#   4. Purge the Cloudflare cache
# The calculator code, year lists, default dates and "Data through" labels all
# come from the theme, so nothing else needs to change.
# ---------------------------------------------------------------------------

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

folder_name <- "_calculators/0000_sp500_calculator_data"
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
                         "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2",
                         "excess_cape", "orig_nom_bond_ret", "real_bond_index")

#Remove first 6 rows
sp500_raw <- sp500_raw[7:nrow(sp500_raw),]

# Convert vars to numeric
sp500_raw$price <- as.numeric(sp500_raw$price)
sp500_raw$dividend <- as.numeric(sp500_raw$dividend)
sp500_raw$cpi_shiller <- as.numeric(sp500_raw$cpi_shiller)
sp500_raw$real_div <- as.numeric(sp500_raw$real_div)
sp500_raw$date <- as.numeric(sp500_raw$date)
sp500_raw$orig_nom_bond_ret <- as.numeric(sp500_raw$orig_nom_bond_ret)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(today) + month(today)/100

# Select specific columns
sp500_subset <- sp500_raw %>%
  select(date, price, dividend, cpi_shiller, real_div, orig_nom_bond_ret) %>%
  filter(!is.na(date), date < end_date) %>%
  mutate(orig_nom_bond_ret = orig_nom_bond_ret - 1)

# Change the Date to a Date type
sp500_subset <- sp500_subset %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-",
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01",
    "%Y-%m-%d"))) %>%
  rename(month = date,
         realDividend = real_div,
         shiller_bond_ret = orig_nom_bond_ret)

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

# Get GS10
#https://fred.stlouisfed.org/series/GS10
gs10_monthly <- read.csv(paste0(importdir, "/0009_sp500_returns_pe/GS10.csv")) %>%
  rename(gs10 = `GS10`) %>%
  mutate(month = as.Date(observation_date)) %>%
  select(month, gs10)

# Join Shiller, Yahoo, and FRED
sp500_ret_pe <- sp500_subset %>%
  filter(month < yahoo_start) %>%
  bind_rows(yahoo_monthly) %>%
  left_join(cpi_monthly, by = "month") %>%
  left_join(gs10_monthly, by = "month") %>%
  mutate(cpi = case_when(
    !is.na(cpi_fred) ~ cpi_fred,
    !is.na(cpi_shiller) ~ cpi_shiller,
    TRUE ~ NA
  )) %>%
  select(-cpi_shiller, -cpi_fred) %>%
  arrange(month)

sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)
sp500_ret_pe$realDividend <- na.locf(sp500_ret_pe$realDividend)

#Estimate CPI data for any months Shiller/FRED is missing (use his formula)
for(i in 1:nrow(sp500_ret_pe)){
  if(is.na(sp500_ret_pe[i, "cpi"])){
    sp500_ret_pe[i, "cpi"] <- 1.5*sp500_ret_pe[(i-1), "cpi"] - 0.5*sp500_ret_pe[(i-2), "cpi"]
  }
}

final_cpi <- sp500_ret_pe[nrow(sp500_ret_pe), "cpi"]

# Calculate stock returns (same as before)
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

# ---------------------------------------------------------------------------
# Bond returns (for the stock/bond calculator)
#
# From 1953 on, Shiller's monthly bond return sits in the month the bond is
# bought (the return from month t to t+1). Shift it back one row so every month
# holds the return earned DURING that month, like the stock returns. (Shiller's
# own real bond index already uses this timing, so the real returns below match
# his index.)
#
# Months after Shiller's data ends use the 10-year Treasury yield (GS10): buy a
# 10-year bond at par paying last month's yield, value it at this month's yield,
# plus one month of interest. Same formula as the old stock/bond script.
# ---------------------------------------------------------------------------
bond_switch <- as.Date("1953-03-01")

sp500_ret_pe <- sp500_ret_pe %>%
  mutate(nom_bond_ret = case_when(
    month <  bond_switch ~ shiller_bond_ret,
    month <= yahoo_start ~ lag(shiller_bond_ret),
    TRUE ~ NA_real_
  ))

for (i in which(is.na(sp500_ret_pe$nom_bond_ret) & sp500_ret_pe$month >= bond_switch)) {
  y0 <- sp500_ret_pe$gs10[i - 1] / 100
  y1 <- sp500_ret_pe$gs10[i] / 100
  if (is.na(y0) || is.na(y1)) {
    stop(paste0("GS10 is missing for ", format(sp500_ret_pe$month[i], "%B %Y"),
                " (or the month before). Download the latest GS10.csv from FRED and run again."))
  }
  bond_price <- (y0 * 100) * (1 - (1 + y1)^-10) / y1 + 100 / (1 + y1)^10
  sp500_ret_pe$nom_bond_ret[i] <- (bond_price / 100 - 1) + (1 + y0)^(1/12) - 1
}

# Real bond return = nominal bond return after that month's inflation.
# Index levels start at 1; the calculators work out monthly returns from them.
sp500_ret_pe <- sp500_ret_pe %>%
  mutate(nom_bond_ret  = ifelse(row_number() == 1, 0, nom_bond_ret),
         real_bond_ret = ifelse(row_number() == 1, 0, (1 + nom_bond_ret) / (cpi / lag(cpi)) - 1),
         bondNominal   = cumprod(1 + nom_bond_ret),
         bondReal      = cumprod(1 + real_bond_ret))

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------
to_calc <- sp500_ret_pe %>%
  filter(month >= filter_date) %>%
  select(month, price, nominalPricePlusDividend, realPrice, realPricePlusDividend,
         cpi, bondNominal, bondReal) %>%
  arrange(month)

# Make sure months are in order with no gaps (the calculators find months by position)
month_num <- year(to_calc$month) * 12 + month(to_calc$month)
if (any(diff(month_num) != 1)) {
  bad <- which(diff(month_num) != 1)
  print(to_calc$month[sort(unique(c(bad, bad + 1)))])
  stop("Months are missing or duplicated around the dates printed above.")
}

# No missing values allowed
missing <- names(to_calc)[colSums(is.na(to_calc)) > 0]
if (length(missing) > 0) {
  stop(paste0("Missing values in: ", paste(missing, collapse = ", ")))
}

data_list <- list(
  start                    = format(min(to_calc$month), "%Y-%m"),
  end                      = format(max(to_calc$month), "%Y-%m"),
  # 4 decimals, same as the old S&P 500 calculator file, so its results are unchanged
  price                    = round(to_calc$price, 4),
  nominalPricePlusDividend = round(to_calc$nominalPricePlusDividend, 4),
  realPrice                = round(to_calc$realPrice, 4),
  realPricePlusDividend    = round(to_calc$realPricePlusDividend, 4),
  cpi                      = signif(to_calc$cpi, 8),
  bondNominal              = signif(to_calc$bondNominal, 10),
  bondReal                 = signif(to_calc$bondReal, 10)
)

json_data <- toJSON(data_list, auto_unbox = TRUE, digits = NA)

writeLines(paste0("window.SP500_DATA = ", json_data, ";"),
           paste0(out_path, "/sp500_data.js"))

# Quick look at the last few months
print(tail(sp500_ret_pe %>% select(month, price, cpi, gs10, nom_bond_ret, real_bond_ret), 4))
print(paste0("Data end month = ", data_list$end))
print(paste0("Shiller end month = ", format.Date(yahoo_start, "%m/%Y")))

# ############################  End  ################################## #