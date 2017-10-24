cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

non_us_ret <- read.csv(paste0(importdir, "44-foreign-returns/developed-returns-ms.csv")) %>%
                mutate(`UK` = `MSCI.United.Kingdom.GR.USD`/100,
                       `France` = `MSCI.France.GR.USD`/100,
                       `Italy` = `MSCI.Italy.GR.USD`/100,
                       `Russia` = `MSCI.Russia.GR.USD`/100,
                       `Spain` = `MSCI.Spain.GR.USD`/100,
                       `Germany` = `MSCI.Germany.GR.USD`/100,
                       date = as.Date(X, "%m/%d/%y") + 1) %>%
                select(date, `UK`, `France`, `Italy`, `Russia`, `Spain`, `Germany`)

saveRDS(non_us_ret, file = paste0(localdir, "44-ms-non-us-equity.Rds"))


# ############################  End  ################################## #