cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

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

# Import FRED CPI data as well
jpy_cpi <- read.csv(paste0(importdir, "28-japan-fred/inflation_fred_japan.csv")) %>%
            mutate(cpi = FPCPITOTLZGJPN/100,
                    year = year(DATE)) %>%
            select(year, cpi)

for (i in 1:nrow(jpy_cpi)){
  if (i == 1){
    jpy_cpi[i, "index"] <- 100
  } else {
    jpy_cpi[i, "index"] <- jpy_cpi[(i-1), "index"] * (1 + jpy_cpi[(i-1), "cpi"])
  }
}

saveRDS(jpy_cpi, paste0(localdir, "28-fred-cpi-japan.Rds"))



# ############################  End  ################################## #