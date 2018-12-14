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
                )
  
  to_plot <- spx %>%
              select(-above_reality) %>%
              gather(-date, key=key, value=value)
  
  # Set note and source string
  source_string <- str_wrap("Source: DFA (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Return includes dividends."), 
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
  
  print(mean(spx$above_reality))
}

check_above_below("1978-01-01", "2018-08-31")




# ############################  End  ################################## #
