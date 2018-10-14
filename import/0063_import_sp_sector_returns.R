cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(readxl)
library(dplyr)

########################## Start Program Here ######################### #

df <- read_excel(paste0(importdir, "0063_sector_returns/sp500_sector_data.xlsx"), skip = 1) %>%
        filter(!(is.na(`Start Date`) | `Start Date` == 'Totals')) %>%
        mutate(sector = trimws(gsub("TR\\s?USD \\(USD\\)", 
                               "", 
                               gsub("S&P 500 Sec/", 
                                    "", 
                                    `Security Name`
                               )
                          )),
               date = as.Date(as.numeric(`Start Date`), origin = "1899-12-31"),
               ret = `Total Return %`/100) %>%
        select(sector, date, ret)

saveRDS(df, paste0(localdir, "0063_sp500_sector_returns.Rds"))

# ############################  End  ################################## #