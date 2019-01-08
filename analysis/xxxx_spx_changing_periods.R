cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(ggjoy)
library(tidyr)
library(readxl)
library(dplyr)

folder_name <- "xxxx_spx_changing_periods"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read_excel(paste0(importdir, "xxxx_spx_monthly_tr/dfa_sp500_tr.xlsx"), skip = 4) %>%
  filter(!is.na(Date)) %>%
  rename(index = `S&P 500 Index`) %>%
  mutate(date = as.Date(Date)) %>%
  select(-Date)

plot_period <- function(n_years){
  n_periods <- n_years * 12
  
  to_plot <- spx %>%
              mutate(period_ret = (lead(index, n_periods)/index)) %>%
              filter(!is.na(period_ret)) %>%
              mutate(number_of_years = n_years)
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
  
  if(n_years < 10){
    year_string <- paste0("0", n_years)
  } else{
    year_string <- n_years
  }
  
  # File path
  file_path <- paste0(out_path, "/period_plot_", year_string, ".jpeg")
  
  source_string <- paste0("Source:  DFA (OfDollarsAndData.com)")
  note_string <- paste0("Note:  Returns include dividends, but not adjusted for inflation.")
  
  plot <- ggplot(to_plot, aes(x=date, y=period_ret)) + 
    geom_point(alpha = 0.5) +
    scale_y_continuous(label = dollar, trans = log2_trans(), limits = c(0.25, 64)) +
    scale_x_date(date_labels = "%Y", breaks = c(
      as.Date("1920-01-01"),
      as.Date("1940-01-01"),
      as.Date("1960-01-01"),
      as.Date("1980-01-01"),
      as.Date("2000-01-01"),
      as.Date("2020-01-01")
    ),
                 limits = c(as.Date("1920-01-01"), as.Date("2020-01-01"))) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500 Growth of $1\nOver ", n_years, " Years")) +
    labs(x="Year", y=paste0("Growth of $1"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm") 
  
  
  if(n_years == 1){
    stacked <- to_plot
  } else{
    stacked <- get("stacked", envir = .GlobalEnv)
    stacked <- stacked %>% bind_rows(to_plot)
  }
  
  assign("stacked", stacked, envir = .GlobalEnv)
}

for (i in 1:30){
  plot_period(i)
}

create_gif(out_path, "period_plot_*.jpeg", 20, 0, "final.gif")


# Use the stacked data to make a big image

file_path <- paste0(out_path, "/all_stack.jpeg")

source_string <- paste0("Source:  DFA (OfDollarsAndData.com)")
note_string <- paste0("Note:  Returns include dividends, but not adjusted for inflation.")

plot <- ggplot(stacked, aes(x=date, y=period_ret, col =  as.factor(number_of_years))) + 
  geom_point(alpha = 0.5) +
  scale_color_discrete(guide=FALSE) +
  scale_y_continuous(label = dollar, trans = log2_trans(), limits = c(0.25, 64)) +
  scale_x_date(date_labels = "%Y", breaks = c(
    as.Date("1920-01-01"),
    as.Date("1940-01-01"),
    as.Date("1960-01-01"),
    as.Date("1980-01-01"),
    as.Date("2000-01-01"),
    as.Date("2020-01-01")
  ),
  limits = c(as.Date("1920-01-01"), as.Date("2020-01-01"))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Growth of $1")) +
  labs(x="Year", y=paste0("Growth of $1"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm") 

# ############################  End  ################################## #