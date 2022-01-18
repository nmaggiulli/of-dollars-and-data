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

folder_name <- "0278_frontload_401k"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

sim_start <- 1978

if(sim_start == 1978){
  # Test 401k max early
  sim_amount <- 20000
} else{
  # Test IRA max early (1998)
  sim_amount <- 6000
}

# Create function to run sims
run_frontload <- function(n_years){

  start_yrs <- seq(sim_start, 2020-n_years) 
  
  final_results <- data.frame()
  counter <- 1
  
  for(start_yr in start_yrs){
    
    # Read in data for individual stocks and sp500 Shiller data
    df    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                          mutate(ret_sp500 = price_plus_div/lag(price_plus_div) - 1) %>%                
                          filter(year(date) >= start_yr, year(date) <= start_yr + n_years - 1) %>%
                          select(date, ret_sp500)
    
    for(i in 1:nrow(df)){
      if(i == 1){
        df[i, "value_frontload"] <- sim_amount
        df[i, "value_average_in_cash"] <- sim_amount/12 * 11
        df[i, "value_average_in_vested"] <- sim_amount/12
        df[i, "value_average_in"] <- df[i, "value_average_in_cash"] + df[i, "value_average_in_vested"]
      } else{
        ret_sp500 <- df[i, "ret_sp500"]
        mt <- month(pull(df[i, "date"]))
        
        if(mt == 1){
          df[i, "value_frontload"] <- (df[(i-1), "value_frontload"] + sim_amount) * (1 + ret_sp500)
          df[i, "value_average_in_cash"] <- sim_amount/12 * 11 
          df[i, "value_average_in_vested"] <- (df[(i-1), "value_average_in_vested"] + sim_amount/12)  * (1 + ret_sp500)
        } else{
          df[i, "value_frontload"] <- df[(i-1), "value_frontload"] * (1 + ret_sp500)
          df[i, "value_average_in_cash"] <- df[(i-1), "value_average_in_cash"] - sim_amount/12
          df[i, "value_average_in_vested"] <- (df[(i-1), "value_average_in_vested"] + sim_amount/12)  * (1 + ret_sp500)
        }
        df[i, "value_average_in"] <- df[i, "value_average_in_cash"] + df[i, "value_average_in_vested"]
        
        df[i, "ret_frontload"] <- df[i, "value_frontload"]/df[(i-1), "value_frontload"] - 1
        df[i, "ret_average_in"] <- df[i, "value_average_in"]/df[(i-1), "value_average_in"] - 1
      }
      if(df[i, "value_frontload"] > df[i, "value_average_in"]){
        df[i, "frontload_diff"] <- df[i, "value_frontload"]/df[i, "value_average_in"] - 1
      } else{
        df[i, "frontload_diff"] <- df[i, "value_average_in"]/df[i, "value_frontload"] - 1
      }

    }
    final_results[counter, "start_year"] <- start_yr
    final_results[counter, "end_year"] <- start_yr + n_years - 1
    final_results[counter, "value_fronload_final"] <- df[nrow(df), "value_frontload"]
    final_results[counter, "value_average_in_final"] <- df[nrow(df), "value_average_in"]
    final_results[counter, "abs_diff"] <- final_results[counter, "value_fronload_final"] - final_results[counter, "value_average_in_final"]
    final_results[counter, "pct_diff"] <- df[nrow(df), "frontload_diff"]
    final_results[counter, "sd_frontload"] <- sd(df$ret_frontload, na.rm = TRUE)
    final_results[counter, "sd_average_in"] <- sd(df$ret_average_in, na.rm = TRUE)
    
    counter <- counter + 1
  }
  assign(paste0("results_", n_years), final_results, envir = .GlobalEnv)
  
  to_plot <- final_results
  
  file_path <- paste0(out_path, "/", sim_start, "_dist_", n_years, "_year_sims.jpeg")
  source_string <- paste0("Source: Shiller data, ", sim_start, "-2020 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Assumes portfolio is 100% invested in U.S. stocks.  ", 
                                 "Performance includes dividends and is adjusted for inflation."),
                          width = 85)

  plot <- ggplot(data = to_plot, aes(x = abs_diff)) +
    geom_density(fill = chart_standard_color) +
    geom_vline(xintercept = 0, linetype = "dashed", col = "black") +
    scale_x_continuous(label = dollar) +
    ggtitle(paste0("How Much More Money Do You Have\nWhen You Max Earlier in the Year?\n", n_years, "-Year Simulations")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(x = "Absolute Difference ($)" , y = "Frequency",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(n_years == 5){
    new_f <- 1
  } else{
    new_f <- 0
  }
  
  export_to_excel(df = final_results,
                  outfile = paste0(out_path, "/", sim_start, "_max_earlier_simulations.xlsx"),
                  sheetname = paste0("results_", n_years, "yr"),
                  new_file = new_f,
                  fancy_formatting = 0)
}

run_frontload(5)
run_frontload(10)
run_frontload(1)


# ############################  End  ################################## #

  
