cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

tickers <- c("AAPL", "AMZN", "XOM", "GE", "GS")

for (t in 1:length(tickers)){
  string     <- paste0("WIKI/", tickers[t])
  temp       <- Quandl(string)
  out        <- select(temp, Date, `Adj. Close`) %>%
                  mutate(ticker = tickers[t],
                         price_plus_div = `Adj. Close`)
  assign(tickers[t], out, envir = .GlobalEnv)
}

all_wiki_stocks <- bind_rows(AAPL, AMZN, XOM, GE, GS)
saveRDS(all_wiki_stocks, paste0(localdir, "23-wiki-single-stocks.Rds"))

# ############################  End  ################################## #