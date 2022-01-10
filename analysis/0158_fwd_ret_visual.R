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

folder_name <- "0158_fwd_ret_visual"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

animate <- 0

raw <- read.csv(paste0(importdir, "0158_dfa_sp500/DFA_PeriodicReturns_20191223103141.csv"), skip = 7,
                col.names = c("date", "ret_sp500", "blank_col")) %>%
  select(-blank_col) %>%
  filter(!is.na(ret_sp500)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  select(date, ret_sp500)

# raw <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
#           mutate(ret_sp500 = price_plus_div/lag(price_plus_div) - 1) %>%
#           filter(date >= "1926-01-01")

df <- raw

for(i in 1:nrow(df)){
  ret <- df[i, "ret_sp500"]
  
  if(i == 1){
    df[i, "index"] <- 1
  } else{
    df[i, "index"] <- df[(i-1), "index"] * (1 + ret)
  }
}


lag_years <- c(1, 5, 10, 15, 20)
years_seq <- seq(1, 10, 1)

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
  
  to_plot <- final_results %>%
                select(lag_ret, lead_ret, n_years_ret)
  
  source_string <- str_wrap(paste0("Source: Returns 2.0 (OfDollarsAndData.com)"), 
                            width = 80)
  note_string <-  str_wrap(paste0("Note:  Performance shown includes dividends, but is not adjusted for inflation."), 
                           width = 80)
  
  plot <- ggplot(to_plot, aes(x=lag_ret, y=lead_ret)) +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = dollar, breaks = seq(0, 8, 1), limits = c(0, 8)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500\n{closest_state}-Year Future Growth\nBased on ", l, "-Year Prior Return")) +
    labs(x= paste0(l, "-Year Annualized Prior Return"), y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string)) +
    transition_states(n_years_ret) +
    ease_aes('linear')
  
  if(animate == 1){
    anim <- animate(plot, fps = 7)
  
    anim_save(filename = paste0("annual_fwd_ret_lag_", l, "_scatter.gif"), animation = anim, path = out_path)
  }
  
  if(l == 10 | l == 20){
    n_future <- 10
    
    if(l == 10){
      upper_flag <- 0.135
      lower_flag <- 0.13
    } else if (l == 20){
      upper_flag <- 0.065
      lower_flag <- 0.06
    }
    
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
      scale_color_manual(values = c("black", "black"), guide = FALSE) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_x_continuous(label = percent) +
      scale_y_continuous(label = dollar, breaks = seq(0, 8, 1), limits = c(0, 8)) +
      of_dollars_and_data_theme +
      ggtitle(paste0("S&P 500\n", n_future, "-Year Future Growth\nBased on ", l, "-Year Prior Return")) +
      labs(x= paste0(l, "-Year Annualized Prior Return"), y = "Growth of $1\nOver Next Decade",
           caption = paste0(source_string, "\n", note_string))
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

# ############################  End  ################################## #