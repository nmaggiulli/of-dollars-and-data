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

folder_name <- "0111_minimum_progress"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

sp500_ret_pe  <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    select(date, price_plus_div) %>%
                    arrange(desc(date))

# Find future low
absolute_minumum <- 10^8

for(i in 1:nrow(sp500_ret_pe)){
  current_p <- pull(sp500_ret_pe[i, "price_plus_div"])
  if (current_p < absolute_minumum){
    sp500_ret_pe[i, "future_low"] <- current_p
    absolute_minumum <- current_p
  } else{
    sp500_ret_pe[i, "future_low"] <- absolute_minumum
  }
}

# Resort the data
sp500_ret_pe <- arrange(sp500_ret_pe, date)

# Find current high
absolute_maximum <- 0

for(i in 1:nrow(sp500_ret_pe)){
  current_p <- pull(sp500_ret_pe[i, "price_plus_div"])
  if (current_p > absolute_maximum){
    sp500_ret_pe[i, "current_high"] <- current_p
    absolute_maximum <- current_p
  } else{
    sp500_ret_pe[i, "current_high"] <- absolute_maximum
  }
}

to_plot <- sp500_ret_pe %>%
            mutate(future_low = future_low/min(sp500_ret_pe$future_low),
                   current_high = current_high/min(sp500_ret_pe$current_high)) %>%
            select(date, future_low, current_high) %>%
            gather(-date, key=key, value=value)

ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  scale_color_discrete(guide = FALSE) +
  of_dollars_and_data_theme +
  ggtitle("Minimum Progress of the S&P 500") +
  labs(x="Date", y="Real Growth of $1")
    

# ############################  End  ################################## #