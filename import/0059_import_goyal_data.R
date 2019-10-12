cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(readxl)
library(httr)

########################## Start Program Here ######################### #

GET('http://www.hec.unil.ch/agoyal/docs/PredictorData2018.xlsx', write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf, sheet="Monthly") %>%
        mutate(char_date = as.character(yyyymm),
                                             date = as.Date(paste0(substring(char_date, 1, 4),
                                                                   "-",
                                                                   substring(char_date, 5, 6),
                                                                   "-01"),
                                                            format = "%Y-%m-%d"),
                                             cpi = as.numeric(infl),
                                             stock_real = (Index/lag(Index) - 1) - cpi,
                                             corp_bond_real = as.numeric(corpr) - cpi,
                                             rf_real = (as.numeric(Rfree) - cpi),
                                             lt_bond_real = as.numeric(ltr) - cpi) %>%
  filter(!is.nan(cpi)) %>%
  select(date, stock_real, lt_bond_real, corp_bond_real, rf_real, cpi)

saveRDS(df, paste0(localdir, "0059_goyal_stock_bond_data.Rds"))

# ############################  End  ################################## #