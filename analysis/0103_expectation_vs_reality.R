cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "0103_expectation_vs_reality"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "0093_best_stock_predictor/sp500_5yr_treasury_data.xlsx"), sheet = "DFA_PeriodicReturns_20180913112") %>%
          select(date, sp500)

for (i in 1:nrow(raw)){
  if(i == 1){
    raw[i, "reality"] <- 1
  } else{
    raw[i, "reality"] <- raw[(i-1), "reality"] * (1 + raw[i, "sp500"])
  }
}

check_above_below <- function(start_date, end_date){

  spx <- raw %>%
          select(-sp500) %>%
          filter(date >= start_date, date < end_date)
  
  start_date_string <- str_replace_all(start_date, "-", "_")
  end_date_string <- str_replace_all(end_date, "-", "_")
  
  first_index <- pull(spx[1, "reality"])
  total_return <- as.numeric((spx[nrow(spx), "reality"]/first_index)^(1/nrow(spx)) - 1)
  
  for (i in 1:nrow(spx)){
    if(i == 1){
      spx[i, "expectation"] <- 1
    } else{
      spx[i, "expectation"] <- spx[(i-1), "expectation"] * (1+total_return)
    }
  }
  
  spx <- spx %>%
          mutate(reality = reality/first_index, 
                above_reality = ifelse(reality > expectation, 1, 0),
                real_over_ex = reality/expectation)
  
  print("**** Expectation Results ****")
  
  print(paste0("The Real was : ", -100*round(1-min(spx$real_over_ex), 4), "% below Expected at its worst point."))
  print(paste0("The Real was: ", 100*round(max(spx$real_over_ex), 4), "% above Expected at its best point."))
  
  to_plot <- spx %>%
              select(-above_reality, -real_over_ex) %>%
              gather(-date, key=key, value=value)
  
  # Set note and source string
  source_string <- str_wrap("Source: DFA (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Includes dividends, but not adjusted for inflation. The S&P 500 compounded at an average rate of ", 100*round((1 + total_return)^12 - 1, 4), "% annually over this period."), 
                            width = 85)
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/expectation_vs_reality_", start_date_string, "_to_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col=key)) +
    geom_line() +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(trans='log10') +
    of_dollars_and_data_theme +
    ggtitle("The S&P 500 vs. Its Long Term Trend") +
    labs(x = "Year", y = "Growth of $1 (Log Scale)",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  print(paste0("Percentage of months where SPX is above its average growth: ", 100*round(mean(spx$above_reality), 4), "%."))
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
  
  # Just plot reality with an LM line and find the % of residuals that are positive versus negative
  to_plot_reality <- filter(to_plot, key == "reality")
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/lm_", start_date_string, "_to_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot_reality, aes(x=date, y = value)) +
    geom_point() +
    scale_y_continuous(trans='log10') +
    geom_smooth(method="lm") +
    of_dollars_and_data_theme +
    ggtitle("The S&P 500 With Linear Fit") +
    labs(x = "Year", y = "Growth of $1 (Log Scale)",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Create LM and print stats
  lm <- lm(log(value)~date, data=to_plot_reality)
  
  assign("lm", lm, envir = .GlobalEnv)
  
  # Transform the logs back before getting the percentage above and below
  lm_fit_over_real <- exp(lm$fitted.values)/(exp(lm$residuals) + exp(lm$fitted.values))
  lm_real_over_fit <- (exp(lm$residuals) + exp(lm$fitted.values))/exp(lm$fitted.values)
  
  print("**** Log Linear Results ****")
  print(paste0("The Real was : ", -100*round(1-min(lm_fit_over_real), 4), "% below the LM at its worst point."))
  print(paste0("The Real was: ", 100*round(max(lm_real_over_fit), 4), "% above the LM at its best point."))
  print(paste0("Percentage of months where SPX is above its LM: ", 100*round(length(which(lm$residuals>0))/length(lm$residuals), 4), "%."))
  print(paste0("The CAGR over this period was: ", 100*round(((exp(lm$fitted.values[length(lm$residuals)])/exp(lm$fitted.values[1]))^(1/1103))^12 - 1, 4), "%."))
}

check_above_below("1978-01-01", "2017-12-31")
check_above_below("1926-01-31", "2017-12-31")

# ############################  End  ################################## #
