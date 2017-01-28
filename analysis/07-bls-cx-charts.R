cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Load data fom local library
bls_cx <- readRDS(paste0(localdir, "07-bls-cx.Rds"))

read_in <- function(string){
  temp <- readRDS(paste0(localdir, "07-bls-cx-", string, ".Rds"))
  return(temp)
}

names <- c("item", "demographics", "characteristics", "subcategory", "category")

for (i in names){
  tmpname <- paste0(i)
  df      <- read_in(i)
  assign(tmpname, df, envir = .GlobalEnv)
  rm(df)
  rm(tmpname)
}


# ############################  End  ################################## #