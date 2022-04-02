cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(readxl)
library(ggjoy)
library(tidyverse)

folder_name <- "_fl/0010_sp500_vs_cpi"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

cpi_limit <- 0.07

raw <- read.csv(paste0(importdir, "/_fl/0010_sp500_vs_cpi/GrowthOfWealth_20220402092218.csv"),
                skip = 6,
                col.names = c("date", "index_sp500", "index_5yr", "index_cpi")) %>%
  filter(date != "", index_sp500 != "") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y") + days(1) - months(1),
         index_sp500 = as.numeric(index_sp500),
         index_5yr = as.numeric(index_5yr),
         index_cpi = as.numeric(index_cpi))

run_cpi_stats <- function(n_month_fwd){
  
 df<- raw %>%
    mutate(cpi_1yr = index_cpi/lag(index_cpi, 12) - 1,
           fwd_cpi = (lead(index_cpi, n_month_fwd)/index_cpi - 1), 
           fwd_sp500 = (lead(index_sp500, n_month_fwd)/index_sp500 - 1),
           fwd_5yr = (lead(index_5yr, n_month_fwd)/index_5yr - 1),
           fwd_sp500_real = fwd_sp500- fwd_cpi,
           fwd_5yr_real = fwd_5yr - fwd_cpi) %>%
    drop_na()
  
  for(i in 1:nrow(df)){
    cpi <- df[i, "cpi_1yr"]
    
    if(cpi > cpi_limit){
      df[i, "above_limit"] <- 1
    } else{
      df[i, "above_limit"] <- 0
    }
  }
  
  summary_by_limit <- df %>%
                        group_by(above_limit) %>%
                        summarise(fwd_months = n_month_fwd,
                          median_fwd_sp500_real = quantile(fwd_sp500_real, probs = 0.5),
                          median_fwd_5yr_real = quantile(fwd_5yr_real, probs = 0.5),
                                  n_months = n()) %>%
                        ungroup()
  
  if(n_month_fwd < 10){
    assign_string <- paste0("summary_0", n_month_fwd, "m")
  } else{
    assign_string <- paste0("summary_", n_month_fwd, "m")
  }
  assign(assign_string, summary_by_limit, envir = .GlobalEnv)
  assign("df", df, envir = .GlobalEnv)
}

run_cpi_stats(12)
run_cpi_stats(24)



# ############################  End  ################################## #

  
