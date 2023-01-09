cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(htmlTable)
library(xtable)
library(tidyverse)

folder_name <- "xxxx_df_to_html"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

to_html <- mtcars

print(xtable(to_html), type="html", file=paste0(out_path, "/example.html"))

# ############################  End  ################################## #