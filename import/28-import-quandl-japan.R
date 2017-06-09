cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

tickers <- c("NIKKEI/INDEX")

for (t in 1:length(tickers)){
  string     <- paste0(tickers[t])
  temp       <- Quandl(string)
}

saveRDS(temp, paste0(localdir, "28-quandl-japan.Rds"))

# ############################  End  ################################## #