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
library(gganimate)
library(tidyverse)

folder_name <- "0432_bullish_enough_future_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
        rename(index = price_plus_div) %>%
        select(date, index)

lag_years <- c(25)
years_seq <- c(10)

for(l in lag_years){
  lag_months <- l * 12
  for(y in years_seq){
    fwd_months <- y * 12
    
  tmp <- df %>%
    mutate(lag_ret = (index/lag(index, lag_months))^(1/l) - 1,
           lead_ret = (lead(index, fwd_months)/index),
           n_years_ret = y) %>%
    filter(!is.na(lag_ret), !is.na(lead_ret)) 
  
    if(y == min(years_seq)){
      final_results <- tmp
    } else{
      final_results <- bind_rows(final_results, tmp)
    }
  }
  
  n_future <- 10
  
  upper_flag <- 0.065
  lower_flag <- 0.06

  to_plot <- final_results %>%
              filter(n_years_ret == n_future) %>%
              mutate(flagged = ifelse(lag_ret > lower_flag & lag_ret < upper_flag, 1, 0)) %>%
              select(date, lag_ret, lead_ret, n_years_ret, flagged)
  
  print(paste0("N year lookback = ", l))
  print(paste0("The correlation is: ", cor(to_plot$lag_ret, to_plot$lead_ret)))
  
  flagged_points <- to_plot %>%
                      filter(flagged == 1) %>%
                      arrange(date)

  print(flagged_points)
  
  source_string <- str_wrap(paste0("Source: Returns 2.0 (OfDollarsAndData.com)"), 
                            width = 80)
  note_string <-  str_wrap(paste0("Note:  Performance shown includes dividends, but is not adjusted for inflation."), 
                           width = 80)
  
  file_path <- paste0(out_path, "/10_fwd_growth_", l, "_prior_plot.jpeg")
  
  # Toggle the second 'black' value in the scale_color_manual() below to create annotated plots
  plot <- ggplot(to_plot, aes(x=lag_ret, y=lead_ret, col = as.factor(flagged))) +
    geom_point() +
    scale_color_manual(values = c("black", "red"), guide = FALSE) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = dollar, breaks = seq(0, 8, 1), limits = c(0, 8)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500\n", n_future, "-Year Future Growth\nBased on ", l, "-Year Prior Return")) +
    labs(x= paste0(l, "-Year Annualized Prior Return"), y = "Growth of $1\nOver Next Decade",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# ############################  End  ################################## #