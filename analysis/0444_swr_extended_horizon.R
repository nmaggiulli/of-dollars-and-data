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
library(tidyverse)

folder_name <- "0444_swr_extended_horizon"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_years <- 55
min_swr <- 0.04
max_swr <- 0.1

run_retirement_sim <- function(n_years, withdrawal_rate){
  start_year <- year(min(raw$date))
  end_year <- year(max(raw$date)) - n_years + 1
  all_years <- seq(start_year, end_year)
  
  final_results <- data.frame()
  counter <- 1
  for(year in all_years){
    end_yr <- year+ n_years - 1
    
    df <- raw %>%
      filter(yr >= year, yr <= end_yr) %>%
      select(date, yr, change_in_cpi, ret_port)
    
    start_port <- 1* 10^6
    for(i in 1:nrow(df)){
      ret_port <- df[i, "ret_port"]
      
      if(i == 1){
        current_year <- df[i, "yr"]
        
        required_spend <- start_port * withdrawal_rate
        monthly_spend <- required_spend/12
        
        df[i, "port"] <- (start_port - monthly_spend) * (1 + ret_port)
      } else{
        mt <- month(df[i, "date"])
        
        if(mt == 1){
          current_year <- df[i, "yr"]
          
          change_in_cpi <- df[i, "change_in_cpi"]
          
          required_spend <- required_spend * (1 + change_in_cpi)
          monthly_spend <- required_spend/12
          
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        } else{
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        }
      }
      if(df[i, "port"] < 0){
        df[i, "port"] <- 0
      }
    }
    final_results[counter, "withdrawal_rate"] <- withdrawal_rate
    final_results[counter, "n_years"] <- n_years
    final_results[counter, "start_year"] <- year
    final_results[counter, "end_year"] <- end_yr
    final_results[counter, "final_port"] <- df[nrow(df), "port"]
    
    counter <- counter + 1
  }
  return(final_results)
}

run_full_sim <- function(n_yrs, s_weight){

  # Do some data analysis to establish a long-term growth rate
  raw <- read.csv(paste0(importdir, "/xxxx_swr_extended/GrowthOfWealth_20250320195235.csv"),
                       skip = 7, 
                       row.names = NULL,
                       col.names = c("date", "index_bond",	"index_sp500", "cpi"))  %>%
    filter(!is.na(index_bond)) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           yr = year(date),
           change_in_cpi = cpi/lag(cpi, 12) - 1)
  
  for(i in 1:nrow(raw)){
    if(i == 1){
      raw[i, "value_bond"] <- 1 - s_weight
      raw[i, "value_stock"] <- s_weight
      raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
      raw[i, "ret_port"] <- 0
    } else{
      mt <- month(raw[i, "date"])
      if(mt == 1){
        raw[i, "value_bond"] <- raw[(i-1), "value_port"] * (1 - s_weight) * (raw[i, "index_bond"]/raw[(i-1), "index_bond"])
        raw[i, "value_stock"] <- raw[(i-1), "value_port"] * s_weight * (raw[i, "index_sp500"]/raw[(i-1), "index_sp500"])
      } else{
        raw[i, "value_bond"] <- raw[(i-1), "value_bond"] * raw[i, "index_bond"]/raw[(i-1), "index_bond"]
        raw[i, "value_stock"] <- raw[(i-1), "value_stock"] * raw[i, "index_sp500"]/raw[(i-1), "index_sp500"]
      }
      raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
      raw[i, "ret_port"] <- raw[i, "value_port"]/raw[(i-1), "value_port"] - 1
    }
  }
  
  assign("raw", raw, envir = .GlobalEnv)

  withdrawal_rates <- seq(min_swr, max_swr, 0.01)
  
  for(w in withdrawal_rates){
    print(paste0("Stock % = ", 100*s_weight, ", Withdrawal % = ", 100*w))
    fr <- run_retirement_sim(n_yrs, w)
    
    if(w == min(withdrawal_rates)){
      final_results_w <- fr
    } else{
      final_results_w <- final_results_w %>% bind_rows(fr)
    }
  }

  summary <- final_results_w %>%
                mutate(survival = ifelse(final_port > 0, 1, 0)) %>%
                group_by(withdrawal_rate, n_years) %>%
                summarise(n_simulations = n(),
                  survival_pct = mean(survival)) %>%
                rename(withdrawal_pct = withdrawal_rate) %>%
                ungroup()
  return(summary)
}

s_weights <- seq(0.5, 1, 0.1)

for(s in s_weights){
  results_base <- run_full_sim(n_years, s)
  
  #Plot sim results
  to_plot <- results_base %>%
    select(withdrawal_pct, survival_pct) %>%
    filter(withdrawal_pct %% 0.01 == 0)
  
  text_labels <- to_plot %>%
    mutate(label = paste0(100*round(survival_pct, 2), "%"))
  
  s_title <- paste0(100*s, "/", (100*(1-s)))
  
  if(s < 1){
    lead_zero <- "0"
  } else{
    lead_zero <- ""
  }
  
  file_path <- paste0(out_path, "/sim_results_", n_years, "_stock_", lead_zero, 100*s, "_swr.jpeg")
  source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Performance data goes from 1926 to 2022, includes dividends, and is adjusted for inflation. ",
                                 "Assumes that the portfolio is rebalanced every January, spending is adjusted for inflation (annually), and that there are no taxes or transaction fees."),
                          width = 80)
  
  plot <- ggplot(data = to_plot, aes(x = withdrawal_pct, y = survival_pct)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data=text_labels, aes(x=withdrawal_pct, y=survival_pct, label = label),
              col = chart_standard_color,
              vjust = -0.2,
              size = 3) +
    scale_x_continuous(label = percent_format(accuracy = 1), breaks = seq(min_swr, max_swr, 0.01)) +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Survival Rate of ", s_title, " Portfolio\nOver ", n_years, "-Year Periods\nBy Withdrawal Rate")) +
    labs(x = "Withdrawal Rate" , y = "Survival Percentage",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Rename column before stacking
  results_base <- results_base %>%
    rename_(.dots = setNames("survival_pct", paste0("stock_", 100*s, "_survival_pct")))
  
  # Stack
  if(s == min(s_weights)){
    final_results <- results_base
  } else{
    final_results <- final_results %>% left_join(results_base)
  }
}

export_to_excel(df = final_results,
                outfile = paste0(out_path, "/swr_results_all_ports_", n_years, ".xlsx"),
                sheetname = "all_ports",
                new_file = 1,
                fancy_formatting = 0)

create_gif(path = out_path, 
          file_stub = paste0("sim_results_", n_years, "_stock_*"),
          speed_milliseconds = 100,
          n_loops = 0,
          out_name = paste0("_all_swr_results_1926_2022_", n_years, ".gif"))


# ############################  End  ################################## #