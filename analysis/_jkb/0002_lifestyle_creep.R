cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "_jkb/0002_lifestyle_creep"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

inc <- 100000
withdrawal_pct <- 0.04
inc_growth <- 0.03
annual_ret <- 0.04

savings_rates <- seq(0.05, 0.6, 0.05)

for(initial_savings_rate in savings_rates){
      annual_savings <- inc*initial_savings_rate
      annual_expenditure <- inc*(1-initial_savings_rate)
      retirement_target <- annual_expenditure/withdrawal_pct
      
      n_periods_baseline <- log(1 + (retirement_target/annual_savings)*annual_ret)/log(1 + annual_ret)
      print(n_periods_baseline)
      
      raise_saved_pcts <- seq(initial_savings_rate + 0.1, 0.95, 0.01)
      
      tmp <- data.frame(
        annual_ret = rep(annual_ret, length(raise_saved_pcts)),
        inc_growth = rep(inc_growth, length(raise_saved_pcts)),
        raise_saved_pct = raise_saved_pcts
        )
      
      counter_f <- 1
      for(r in raise_saved_pcts){
        df <- data.frame(year = c(),
                         saving_amount = c(),
                         total_saved = c(),
                         retirement_target = c(),
                         income = c(),
                         pct_total_retirement = c())
        
        counter <- 1
        retire_pct <- 0
        while(retire_pct < 1){
          if(counter == 1){
            df[counter, "year"] <- counter
            df[counter, "saving_amount"] <- annual_savings
            df[counter, "total_saved"] <- annual_savings
            df[counter, "retirement_target"] <- retirement_target
            df[counter, "income"] <- inc
          } else{
            df[counter, "year"] <- counter
            df[counter, "income"] <- df[(counter-1), "income"] * (1 + inc_growth)
            df[counter, "saving_amount"] <- df[(counter-1), "saving_amount"] + (df[(counter), "income"] - df[(counter-1), "income"]) * r
            df[counter, "total_saved"] <- (df[(counter-1), "total_saved"] * (1 + annual_ret)) + df[counter, "saving_amount"]
            df[counter, "retirement_target"] <- (df[counter, "income"] - df[counter, "saving_amount"])/withdrawal_pct
          }
          
          df[counter, "pct_total_retirement"] <- df[counter, "total_saved"]/df[counter, "retirement_target"]
          retire_pct <- df[counter, "pct_total_retirement"]
          counter <- counter + 1
        }
        tmp[counter_f, "n_periods"] <- counter - 1
        tmp[counter_f, "better_than_baseline"] <- ifelse(tmp[counter_f, "n_periods"] < n_periods_baseline, 1, 0)
        
        if(r == initial_savings_rate & initial_savings_rate == 0.6){
          assign("investigate", df, envir = .GlobalEnv)
        }
        
        counter_f <- counter_f + 1
      }
      
      tmp2 <- tmp %>%
                filter(better_than_baseline == 1) %>%
                head(1) %>%
                select(-better_than_baseline) %>% 
        mutate(savings_rate = initial_savings_rate)
  
  if(initial_savings_rate == savings_rates[1]){
    final_results <- tmp2
  } else{
    final_results <- final_results %>% bind_rows(tmp2) %>%
                      select(savings_rate, raise_saved_pct, n_periods)
  }
}

file_path <- paste0(out_path, "/savings_rate_vs_raise_saved_pct.jpeg")

to_plot <- final_results

# Plot the results
plot <- ggplot(to_plot, aes(x = savings_rate, y = raise_saved_pct)) +
  geom_line() +
  scale_x_continuous(label = percent_format(accuracy = 1), breaks = savings_rates) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0.25, 0.8, 0.05)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("How Much of Your Raise You Need to Save\nBased on Initial Savings Rate")) +
  labs(x = "Initial Savings Rate" , y = "Raise Savings Percentage")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

export_to_excel(df = final_results,
                outfile = paste0(out_path, "/savings_rate_raise_pcts.xlsx"),
                sheet="raw",
                new_file = 1,
                fancy_formatting = 1
)

# ############################  End  ################################## #