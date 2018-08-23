cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

# Download file manually to local drive after setting up Kaggle account
# https://www.kaggle.com/dgawlik/nyse/downloads/nyse.zip

# Bring in fundamentals file
nyse_fundamentals <- read.table(unz(paste0(importdir, "18-nyse-kaggle/nyse.zip"), 
                       "fundamentals.csv"), 
                       header=T, 
                       quote = "\"", 
                       sep=",")

# Select a subset of columns using dplyr
nyse_fundamentals_final <- select(nyse_fundamentals,
                                  Ticker.Symbol, Period.Ending,
                                  Accounts.Payable, Accounts.Receivable,
                                  Capital.Expenditures, Cash.and.Cash.Equivalents,
                                  Depreciation, Earnings.Before.Interest.and.Tax,
                                  Goodwill, Gross.Profit,
                                  Income.Tax, Liabilities,
                                  Net.Cash.Flow, Net.Income,
                                  Operating.Income, Total.Assets,
                                  Total.Equity, Total.Revenue,
                                  Earnings.Per.Share,
                                  Estimated.Shares.Outstanding)

# Bring in the adjusted prices as well
nyse_price_adj <- read.table(unz(paste0(importdir, "18-nyse-kaggle/nyse.zip"), 
                                    "prices-split-adjusted.csv"), 
                                header=T, 
                                quote = "\"", 
                                sep=",")

# Select only the variables we care about
nyse_price_adj <- select(nyse_price_adj, date, close, symbol)

# Save down the files
saveRDS(nyse_fundamentals_final, paste0(localdir, "18-nyse-fundamentals.Rds"))
saveRDS(nyse_price_adj, paste0(localdir, "18-nyse-price-adj.Rds"))


# ############################  End  ################################## #