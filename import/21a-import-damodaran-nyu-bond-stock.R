cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(gdata)
library(readxl)

########################## Start Program Here ######################### #

# Add link to the file path
link_to_file <- paste0(importdir, "21a-damodaran-nyu-stock-bond-data/histretSP.xlsx")

hist_ret <- read_excel(link_to_file)

# Subset to the columns and rows we want for the data
hist_ret <- hist_ret[18:106, 1:4]

# Rename the columns accordingly
colnames(hist_ret) <- c("Date", "ret_sp500", "ret_3m_bill", "ret_10yr_bond")

hist_ret$Date <- as.numeric(as.character(hist_ret$Date))
hist_ret$ret_sp500 <- as.numeric(sub("%", "", as.character(hist_ret$ret_sp500)))
hist_ret$ret_3m_bill <- as.numeric(sub("%", "", as.character(hist_ret$ret_3m_bill)))
hist_ret$ret_10yr_bond <- as.numeric(sub("%", "", as.character(hist_ret$ret_10yr_bond)))

saveRDS(hist_ret, paste0(localdir, "21-historical-returns-sp500-bond-damodaran.Rds"))


# ############################  End  ################################## #