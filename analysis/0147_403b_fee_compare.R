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

folder_name <- "0147_403b_fee_compare"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

solve_surrender_charge <- function(n_periods, ret_portfolio, fee_annuity, fee_non_annuity){
  
  annuity_growth <- (1 + ret_portfolio - fee_annuity)^n_periods
  non_annuity_growth <- (1 + ret_portfolio - fee_non_annuity)^n_periods
  
  surrender <- 1-annuity_growth/non_annuity_growth
  return(surrender)
}

period_seq <- seq(0, 30, 1)
n_periods_seq <- length(period_seq)
ret_port <- 0.05
f_ann <- 0.0225
f_non_ann <- 0.005

df <- data.frame(n_periods = period_seq,
                 ret_portfolio = rep(ret_port, n_periods_seq),
                 fee_annuity = rep(f_ann, n_periods_seq),
                 fee_non_annuity = rep(f_non_ann, n_periods_seq)
                 )

for(i in 1:nrow(df)){
  n   <- df[i, "n_periods"]
  rp  <- df[i, "ret_portfolio"]
  fa  <- df[i, "fee_annuity"]
  fna <- df[i, "fee_non_annuity"]
  
  df[i, "surrender_pct"] <- solve_surrender_charge(n, rp, fa, fna)
}

to_plot <- df %>%
            select(n_periods, surrender_pct)

# Do stock beats by year
file_path <- paste0(out_path, "/surrender_pct_by_year.jpeg")

source_string <- str_wrap(paste0("Source:  Simulated data (OfDollarsAndData.com)"), 
                          width = 85)
note_string <-  str_wrap(paste0("Note:  Shows the maximum surrender charge that could be incurred ",
                                "by an annuity participant before they would be worse off switching out ",
                                "of the annuity based on the number of years left to invest.  ",
                                "Assumes that annuity participants pay ", 100*f_ann,
                                "% annually while those who switch to a lower cost option pay ", 100*f_non_ann, 
                                "% annually.  Both the annuity and non-annuity portfolios return ", 
                                100*ret_port, "% every year."),
                         width = 85)

plot <- ggplot(data = to_plot, aes(x=n_periods, y = surrender_pct)) +
  geom_line() +
  scale_y_continuous(label = percent) +
  scale_x_continuous(limits = c(0, 30)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Maximum Surrender Percentage Based on\nNumber of Years To Invest")) +
  labs(x = paste0("Number of Years to Invest"), y = paste0("Maximum Surrender Percentage"),
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

starting_value <- 10000
surrender_values <- seq(0, 0.4, 0.05)

for(s in 1:length(surrender_values)){
  
  # Initialize data frames
  surr <- surrender_values[s]
  by_year <- data.frame(year = period_seq,
                        value_annuity = rep(starting_value, n_periods_seq),
                        value_non_annuity = rep(starting_value, n_periods_seq))
  
  for(i in 1:nrow(by_year)){
    if(i > 1){
      by_year[i, "value_annuity"] <- by_year[(i-1), "value_annuity"] * (1 + ret_port - f_ann)
      by_year[i, "value_non_annuity"] <- by_year[(i-1), "value_non_annuity"] * (1 + ret_port - f_non_ann)
    } else{
      by_year[i, "value_non_annuity"] <- (1-surr)*by_year[i, "value_non_annuity"]
    }
  }
  
  to_plot <- by_year %>%
                select(year, value_annuity, value_non_annuity) %>%
                rename(`Annuity` = value_annuity,
                       `Non-Annuity` = value_non_annuity) %>%
                gather(-year, key=key, value=value)
  
  surr_string <- str_pad(100*surr, width = 2, side = "left", pad = "0")
  
  file_path <- paste0(out_path, "/compare_growth_surr_", surr_string, "_pct.jpeg")
  
  source_string <- str_wrap(paste0("Source:  Simulated data (OfDollarsAndData.com)"), 
                            width = 85)
  note_string <-  str_wrap(paste0("Note:  Assumes no additional contributions and that annuity participants pay ", 100*f_ann,
                                  "% annually while those who switch to a lower cost option pay ", 100*f_non_ann, 
                                  "% annually.  Both the annuity and non-annuity portfolios return ", 
                                  100*ret_port, "% every year."),
                           width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=year, y = value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar, limits = c(0, 40000), breaks = seq(0, 40000, 5000)) +
    scale_x_continuous(limits = c(0, 30)) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("Growth of Annuity vs. Non-Annuity\nWith a ", 100*surr, "% Surrender Fee")) +
    labs(x = paste0("Year"), y = paste0("Portfolio Value"),
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # aggregate results
  tmp <- by_year %>%
          mutate(surrender_pct = surr)
  
  if(s == 1){
    final_results <- tmp
  } else{
    final_results <- bind_rows(final_results, tmp)
  }
}

create_gif(out_path,
           paste0("compare_growth_surr_*.jpeg"),
           100,
           0,
           paste0("_gif_compare_growth_surr.gif"))

# ############################  End  ################################## #