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

folder_name <- "0347_save_more_retire_earlier"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

annual_income <- 100000
ret <- 0.05

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
  
  return(months1 - months2)
}

all_time_horizons <- c(10, 20, 30)
all_savings_rates <- seq(0.05, 0.5, 0.05)
delta_savings <- c(0.01, 0.05, 0.1)
final_results <- data.frame()
counter <- 1

for(t in all_time_horizons){
  for(a in all_savings_rates){
    for(d in delta_savings){
      diff <- calculate_retire_time_diff(t, ret, a, a+d)
      final_results[counter, "time_horizon"] <- t
      final_results[counter, "orig_savings_rate"] <- a
      final_results[counter, "change_in_savings_rate"] <- d
      final_results[counter, "months_saved"] <- diff
      
      counter <- counter + 1
    }
  }
}

plot_time_saved <- function(n_years){

  file_path <- paste0(out_path, "/savings_rate_vs_months_saved_", n_years, "_years.jpeg")
  source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Assumes a ", 100*ret, "% annual return on all invested savings."),
                          width = 80)
  
  text_labels <- data.frame()
  
  text_labels[1, "orig_savings_rate"] <- 0.11
  text_labels[2, "orig_savings_rate"] <- 0.11
  text_labels[3, "orig_savings_rate"] <- 0.12
  text_labels[1, "change_in_savings_rate"] <- "1%"
  text_labels[2, "change_in_savings_rate"] <- "5%"
  text_labels[3, "change_in_savings_rate"] <- "10%"
  text_labels[1, "label"] <- "+1%"
  text_labels[2, "label"] <- "+5%"
  text_labels[3, "label"] <- "+10%"
  
  if(n_years == 10){
    text_labels[1, "years_saved"] <- 1
    text_labels[2, "years_saved"] <- 3.15
    text_labels[3, "years_saved"] <- 4.6
    
    y_min <- 0
    y_max <- 7
    y_breaks <- 1
  } else if(n_years == 20){
    text_labels[1, "years_saved"] <- 1.75
    text_labels[2, "years_saved"] <- 5.25
    text_labels[3, "years_saved"] <- 8
    
    y_min <- 0
    y_max <- 11
    y_breaks <- 1
  } else if(n_years == 30){
    text_labels[1, "years_saved"] <- 2.25
    text_labels[2, "years_saved"] <- 6.75
    text_labels[3, "years_saved"] <- 10.5
    
    y_min <- 0
    y_max <- 15
    y_breaks <- 1
  }
  
  to_plot <- final_results %>%
              filter(time_horizon == n_years) %>%
              mutate(change_in_savings_rate = paste0(100*change_in_savings_rate, "%"),
                     years_saved = months_saved/12)
  
  plot <- ggplot(to_plot, aes(x=orig_savings_rate, y=years_saved, col = change_in_savings_rate)) +
    geom_line() +
    geom_text(data=text_labels, aes(x=orig_savings_rate, y=years_saved, col = change_in_savings_rate,
              label = label), family = my_font, size = 4) +
    scale_x_continuous(label = percent_format(accuracy = 1), breaks = seq(0.05, 0.5, 0.05)) +
    scale_y_continuous(label = comma, limits = c(y_min, y_max), breaks = seq(y_min, y_max, y_breaks)) +
    scale_color_discrete(guide = "none") +
    of_dollars_and_data_theme +
    ggtitle(paste0("How Much Earlier You Can Retire\nBased on Change in Savings Rate\nWhen ", n_years, " Years From Retirement")) +
    labs(x = "Current Savings Rate" , y = "Years",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(n_years == 10){
    file_path <- paste0(out_path, "/savings_rate_vs_months_saved_", n_years, "_years_1pct_only.jpeg")
    
    to_plot <- to_plot %>%
                filter(change_in_savings_rate == "1%") %>%
                mutate(months_saved = years_saved*12)
    
    assign("tp", to_plot, envir = .GlobalEnv)
    
    text_labels <- text_labels %>%
                      head(1) %>%
                      mutate(months_saved = years_saved*12)
    
    plot <- ggplot(to_plot, aes(x=orig_savings_rate, y=months_saved)) +
      geom_line(col = "black") +
      scale_x_continuous(label = percent_format(accuracy = 1), breaks = seq(0.05, 0.5, 0.05)) +
      scale_y_continuous(label = comma, limits = c(0, 18), breaks = seq(0, 18, 6)) +
      scale_color_discrete(guide = "none") +
      of_dollars_and_data_theme +
      ggtitle(paste0("How Much Earlier You Can Retire\nWhen Saving 1% More Annually\nWhile ", n_years, " Years From Retirement")) +
      labs(x = "Current Savings Rate" , y = "Months",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
  to_export <- final_results %>%
    mutate(years_saved = round(months_saved/12, 1)) %>%
    select(-months_saved) %>%
    spread(key=change_in_savings_rate, value=years_saved) %>%
    mutate(`Current Savings Rate` = paste0(100*orig_savings_rate, "%"),
           `Save 1% More` = `0.01`,
           `Save 5% More` = `0.05`,
           `Save 10% More` = `0.1`) %>%
    select(time_horizon, `Current Savings Rate`, `Save 1% More`, `Save 5% More`, `Save 10% More`)
  
  print(xtable(to_export %>% filter(time_horizon == n_years) %>% select(-time_horizon)), 
        include.rownames=FALSE,
        type="html", 
        file=paste0(out_path, "/years_saved_", n_years, "_yr.html"))
}

plot_time_saved(10)
plot_time_saved(20)
plot_time_saved(30)


# ############################  End  ################################## #