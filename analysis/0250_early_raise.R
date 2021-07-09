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
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0250_early_raise"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

starting_salary <- 50000
raise_size_singular <- 20000
raise_size <- 10000
n_years_raise_early <- 5
n_years_working <- 40

# These inputs don't affect the relative outcomes
investment_ret <- 0.05
savings_rate <- 0.1
match_rate <- 0.04


df <- data.frame()

for(i in 1:n_years_working){
  df[i, "year"] <- i
 
 if(i == 1){
   df[i, "salary_early"] <- starting_salary
   df[i, "salary_late"] <- starting_salary
   df[i, "port_early"] <- starting_salary*(savings_rate + match_rate)
   df[i, "port_late"] <- starting_salary*(savings_rate + match_rate)
   df[i, "multi_raise_early_premium"] <- 0
   
   df[i, "salary_early_single"] <- starting_salary
   df[i, "salary_late_single"] <- starting_salary
   df[i, "port_early_single"] <- starting_salary*(savings_rate + match_rate)
   df[i, "port_late_single"] <- starting_salary*(savings_rate + match_rate)
   df[i, "single_raise_early_premium"] <- 0
 } else{
   if(i == n_years_raise_early){
     df[i, "salary_early"] <- df[(i-1), "salary_early"] + raise_size
     df[i, "salary_late"] <- df[(i-1), "salary_late"]
     
     df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"] + raise_size_singular
     df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
   } else if(i %% n_years_raise_early == 0){
     df[i, "salary_early"] <- df[(i-1), "salary_early"] + raise_size
     df[i, "salary_late"] <- df[(i-1), "salary_late"] + raise_size
     
     if(i == 10){
       df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
       df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"] + raise_size_singular
     } else{
       df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
       df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
     }
   } else{
     df[i, "salary_early"] <- df[(i-1), "salary_early"]
     df[i, "salary_late"] <- df[(i-1), "salary_late"]
     
     df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
     df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
   }
   
   df[i, "port_early"] <- (df[(i-1), "port_early"] * (1 + investment_ret)) + (df[i, "salary_early"] * (savings_rate + match_rate)) 
   df[i, "port_late"] <- (df[(i-1), "port_late"] * (1 + investment_ret)) + (df[i, "salary_late"] * (savings_rate + match_rate)) 
   df[i, "multi_raise_early_premium"] <- df[i, "port_early"]/df[i, "port_late"] - 1
   
   df[i, "port_early_single"] <- (df[(i-1), "port_early_single"] * (1 + investment_ret)) + (df[i, "salary_early_single"] * (savings_rate + match_rate)) 
   df[i, "port_late_single"] <- (df[(i-1), "port_late_single"] * (1 + investment_ret)) + (df[i, "salary_late_single"] * (savings_rate + match_rate)) 
   df[i, "single_raise_early_premium"] <- df[i, "port_early_single"]/df[i, "port_late_single"] - 1
 }
}

# Charts


# ############################  End  ################################## #