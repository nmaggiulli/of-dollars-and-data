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

folder_name <- "_jkb/0013_luck_and_investing"
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
    select(year, ret)
  
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
              aes(as.factor(year), ret + (text_bump * sign(ret)), label = paste0(round(100*ret, 1), "%")),
              col = "black", 
              size = 4) +
    ggtitle(title_string) +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(labels = percent) +
    scale_x_discrete() +
    of_dollars_and_data_theme +
    labs(x = "Starting Year" , y = paste0("Annualized Real Total Return (%)\nFor ", n_years, " Years"))

  # Save the plot  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_years(10, as.Date("1910-01-01"), "Lucky and Unlucky Decades for the S&P 500")
plot_years(20, as.Date("1900-01-01"), "S&P 500 Annualized Real Total Return\nOver Two Decades")
plot_years(30, as.Date("1900-01-01"), "S&P 500 Annualized Real Total Return\nOver Three Decades")

# Do Sequence of return plots


# Define the number of years and the level of good and bad returns
n_years        <- 20
n_bad_years    <- 10
annual_savings <- 5000

good_return <- 0.1
bad_return  <- -0.1

# Define the scenario vectors
scenario_1 <- c(rep(bad_return, n_bad_years), rep(good_return, n_years - n_bad_years))
scenario_2 <- c(rep(good_return, n_years - n_bad_years), rep(bad_return, n_bad_years))

value_path <- data.frame(matrix(NA, nrow = n_years, ncol = 0))

for (i in 1:n_years){
  value_path[i, "year"] <- i
  if(i == 1){
    value_path[i, "Negative Returns Early"] <- annual_savings
    value_path[i, "Negative Returns Later"] <- annual_savings
  } else {
    value_path[i, "Negative Returns Early"] <- annual_savings + (value_path[(i - 1), "Negative Returns Early"] * (1 + scenario_1[i]))
    value_path[i, "Negative Returns Later"] <- annual_savings + (value_path[(i - 1), "Negative Returns Later"] * (1 + scenario_2[i]))
  }
}

value_path <- gather(value_path, scenario, value, -year)

to_plot <- value_path

file_path <- paste0(out_path, "/sequence_of_return_full_sim.jpeg")
top_title <- paste0("Negative Returns Have a Larger Impact\nLater in Life")

plot <- ggplot(to_plot, aes(x = year, y = value)) +
  geom_line() +
  geom_vline(xintercept = 10, linetype =  "dashed", col = "black") +
  facet_grid(~scenario) +
  ggtitle(top_title) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(limits = c(0, 200000), label = dollar) +
  scale_x_continuous(limits = c(1, n_years)) +
  labs(x = paste0("Year"), y = "Total Portfolio Value")

  
# Save the plot  
ggsave(file_path, plot, width = 15, height = 12, units = "cm") 


# ############################  End  ################################## #