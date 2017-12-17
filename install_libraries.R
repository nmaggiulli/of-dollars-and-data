cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Install All in Libraries ########################## #

pkgs <- c("dplyr", "ggplot2", "tidyr", "stringr", "lubridate", 
          "ggjoy", "ggrepel", "scales", "grid", "gridExtra",
          "RColorBrewer", "quadprog", "fTrading", "quantmod",
          "gtable", "reshape2", "MASS", "gdata", "readxl",
          "Quandl")

install.packages(pkgs)

# ############################  End  ################################## #