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
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(tidyverse)

folder_name <- "0347_saving_vs_working"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

annual_income <- 100000


calculate_retire_time_diff <- function(yr_retire, ret, s_rate1, s_rate2){
  mt_retire <- yr_retire*12
  
  tmp <- data.frame()
  
  for(i in 1:mt_retire){
    tmp[i, "month"] <- i
    if(i == 1){
      tmp[i, "retirement_savings"] <- annual_income/12*s_rate1
    } else{
      tmp[i, "retirement_savings"] <- tmp[(i-1), "retirement_savings"]*(1 + ret/12) + (annual_income/12*s_rate1)
    }
  }
  
  final_value <- tmp[nrow(tmp), "retirement_savings"]
  
  savings2 <- 0
  f_counter <- 0
  while(savings2 < final_value){
    savings2 <- savings2 * (1 + ret/12) + (annual_income/12*s_rate2)
    f_counter <- f_counter + 1
  }
  
  months1 <- nrow(tmp)
  months2 <- f_counter
  
  print(paste0("Saving ",
               round(100*s_rate2, 1),
               "% for ",
               yr_retire, 
               " years (while earning ", 
               round(100*ret, 1), 
               "% annually) would save you ",
               months1 - months2, 
               " months compared to saving at a ",
               round(100*s_rate1, 1),
               "% rate."
               ))
  
  return(months1 - months2)
}

all_time_horizons <- seq(10, 30, 10)
all_savings_rates <- seq(0.05, 0.4, 0.05)
delta_savings <- c(0.01, 0.05, 0.1)
final_results <- data.frame()
counter <- 1

for(t in all_time_horizons){
  for(a in all_savings_rates){
    for(d in delta_savings){
      diff <- calculate_retire_time_diff(t, 0.05, a, a+d)
      final_results[counter, "time_horizon"] <- t
      final_results[counter, "orig_savings_rate"] <- a
      final_results[counter, "comp_savings_rate"] <- a + d
      final_results[counter, "months_saved"] <- diff
      
      counter <- counter + 1
    }
  }
}




# ############################  End  ################################## #