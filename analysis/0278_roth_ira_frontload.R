cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

folder_name <- "0278_roth_ira_frontload"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Create function to run sims
run_frontload <- function(n_years){
  
  amount <- 6000
  start_yrs <- seq(1998, 2020-n_years) 
  
  final_results <- data.frame()
  counter <- 1
  
  for(start_yr in start_yrs){
    
    # Read in data for individual stocks and sp500 Shiller data
    df    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                          mutate(ret_sp500 = price_plus_div/lag(price_plus_div) - 1) %>%                
                          filter(year(date) >= start_yr, year(date) <= start_yr + n_years) %>%
                          select(date, ret_sp500)
    
    for(i in 1:nrow(df)){
      if(i == 1){
        df[i, "value_frontload"] <- amount
        df[i, "value_average_in_cash"] <- amount/12 * 11
        df[i, "value_average_in_vested"] <- amount/12
        df[i, "value_average_in"] <- df[i, "value_average_in_cash"] + df[i, "value_average_in_vested"]
      } else{
        ret_sp500 <- df[i, "ret_sp500"]
        mt <- month(pull(df[i, "date"]))
        
        if(mt == 1){
          df[i, "value_frontload"] <- (df[(i-1), "value_frontload"] + amount) * (1 + ret_sp500)
          df[i, "value_average_in_cash"] <- amount/12 * 11 
          df[i, "value_average_in_vested"] <- (df[(i-1), "value_average_in_vested"] + amount/12)  * (1 + ret_sp500)
        } else{
          df[i, "value_frontload"] <- df[(i-1), "value_frontload"] * (1 + ret_sp500)
          df[i, "value_average_in_cash"] <- df[(i-1), "value_average_in_cash"] - amount/12
          df[i, "value_average_in_vested"] <- (df[(i-1), "value_average_in_vested"] + amount/12)  * (1 + ret_sp500)
        }
        df[i, "value_average_in"] <- df[i, "value_average_in_cash"] + df[i, "value_average_in_vested"]
        
        df[i, "ret_frontload"] <- df[i, "value_frontload"]/df[(i-1), "value_frontload"] - 1
        df[i, "ret_average_in"] <- df[i, "value_average_in"]/df[(i-1), "value_average_in"] - 1
      }
      df[i, "frontload_diff"] <- df[i, "value_frontload"]/df[i, "value_average_in"]
    }
    final_results[counter, "start_year"] <- start_yr
    final_results[counter, "end_year"] <- start_yr + n_years
    final_results[counter, "value_fronload_final"] <- df[nrow(df), "value_frontload"]
    final_results[counter, "value_average_in_final"] <- df[nrow(df), "value_average_in"]
    final_results[counter, "pct_diff"] <- df[nrow(df), "frontload_diff"] - 1
    final_results[counter, "sd_frontload"] <- sd(df$ret_frontload, na.rm = TRUE)
    final_results[counter, "sd_average_in"] <- sd(df$ret_average_in, na.rm = TRUE)
    
    counter <- counter + 1
  }
  assign(paste0("results_", n_years), final_results, envir = .GlobalEnv)
}

run_frontload(5)
run_frontload(10)
run_frontload(1)

export_to_excel(df = results_5,
                outfile = paste0(out_path, "/max_ira_early_simulations.xlsx"),
                sheetname = "results_5yr",
                new_file = 1,
                fancy_formatting = 0)

export_to_excel(df = results_10,
                outfile = paste0(out_path, "/max_ira_early_simulations.xlsx"),
                sheetname = "results_10yr",
                new_file = 0,
                fancy_formatting = 0)

export_to_excel(df = results_1,
                outfile = paste0(out_path, "/max_ira_early_simulations.xlsx"),
                sheetname = "results_1yr",
                new_file = 0,
                fancy_formatting = 0)

# ############################  End  ################################## #

  
