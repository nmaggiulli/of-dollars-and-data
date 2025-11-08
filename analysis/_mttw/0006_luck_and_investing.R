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
library(tidyverse)

folder_name <- "_mttw/0006_luck_and_investing"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                      filter(date <= "2020-12-31")

plot_years <- function(n_years, start_date, title_string){
  # Subset based on start date
  df <- sp500_ret_pe %>%
    filter(date >= start_date)
  
  # Find number of months available
  n_months_avail <- nrow(df)
  
  # Find the number of groups we can calculate
  n_groups_to_calc <- floor(n_months_avail/(n_years*12))
  n_months_to_calc <- n_years*12
  
  df <- df[(1:(n_months_to_calc*n_groups_to_calc)), ]
  
  counter <- 1
  for (i in 1:n_groups_to_calc){
    final <- df[(counter + n_months_to_calc - 1), "price_plus_div"]
    initial <- df[counter, "price_plus_div"]
    
    df[counter, "ret"] <- (final/initial)^(1/n_years) - 1
    
    counter <- counter + n_months_to_calc
  }
  
  to_plot <- df %>%
    filter(!is.na(ret)) %>%
    mutate(year = year(date)) %>%
    select(year, ret) %>%
    mutate(label = paste0(format(round(100*ret, digits=1), nsmall = 1), "%"))
  
  start_date_string <- as.character(start_date)
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/period_returns_", n_years, "yr_", start_date_string, ".jpeg")
  
  if(n_years > 20){
    text_bump <- 0.0025
  } else{
    text_bump <- 0.005
  }
  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = to_plot, aes(x = as.factor(year), y = ret)) +
    geom_bar(stat = "identity", position = "dodge", fill = "black") +
    geom_text(data = to_plot, 
              aes(as.factor(year), ret + (text_bump * sign(ret)), label = label),
              col = "black", 
              size = 4) +
    ggtitle(title_string) +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(labels = percent) +
    scale_x_discrete() +
    of_dollars_and_data_theme +
    labs(x = "起始年份" , y = paste0("年化實質總報酬率 (%)\n10年期間"))

  # Save the plot  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_years(10, as.Date("1910-01-01"), "S&P 500各十年期年化報酬率")



# ############################  End  ################################## #