cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "0122_reflexivity"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- data.frame(x=seq(0, 30))

for(i in 1:nrow(df)){
  x <- df[i, "x"]
  df[i, "y"] <- x^2 - 0.05*(x^3)
}

ggplot(df, aes(x=x, y=y)) +
  geom_point()


# ############################  End  ################################## #