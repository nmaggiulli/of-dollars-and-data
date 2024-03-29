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

folder_name <- "0394_japan_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")

plot_jpy <- function(end_date, title1){
  
  # Bring in JPY data
  jpy <- read.csv(paste0(importdir,"/0394_japan_data/NIKKEI225_fred.csv")) %>%
    mutate(date = as.Date(DATE),
           index_jpy = as.numeric(NIKKEI225),
           ret_jpy = index_jpy/lag(index_jpy, 1) - 1) %>%
    select(date, index_jpy, ret_jpy) %>%
    drop_na %>%
    filter(date >= "1980-01-01", date <= as.Date(end_date))
  
  for(i in 1:nrow(jpy)){
    if(i == 1){
      jpy[i, "market_value"] <- 1
      jpy[i, "basis"] <- 1
    } else{
      jpy[i, "market_value"] <- jpy[(i-1), "market_value"] * (1 + jpy[i, "ret_jpy"]) + 1
      jpy[i, "basis"] <- jpy[(i-1), "basis"] + 1
    }
  }
  
  to_plot <- jpy
  end_date_string <- date_to_string(as.Date(end_date))
  
  file_path <- paste0(out_path, "/jpy_1980_", end_date_string, ".jpeg")
  source_string <- "Source: FRED"
  
  plot <- ggplot(to_plot, aes(x=date, y=index_jpy)) + 
    geom_line(col = "black") +
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle(paste0(title1)) +
    labs(x = "Date" , y = "Index Value",
         caption = paste0(source_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  to_plot <- to_plot %>%
    select(date, market_value, basis) %>%
    gather(-date, key=key, value=value) %>%
    mutate(key = case_when(
      key == "market_value" ~ "Market Value",
      TRUE ~ "Cost Basis"
    ))
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
  
  # DCA into Japan
  file_path <- paste0(out_path, "/jpy_1980_dca_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
    geom_line() +
    scale_color_manual(values = bw_colors) +
    scale_y_continuous(label = dollar) + 
    scale_x_date(date_labels = "%Y") +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Portfolio Value vs. Cost Basis\n$1 Per Day into Japanese Stocks")) +
    labs(x = "Date" , y = "Value",
         caption = paste0(source_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_jpy("2020-12-31", "The Japanese Stock Market Was\nBelow Its Highs For Over Three Decades")
plot_jpy("2024-03-26", "The Highs, Lows, and New Highs\nFor Japanese Stocks")

# ############################  End  ################################## #