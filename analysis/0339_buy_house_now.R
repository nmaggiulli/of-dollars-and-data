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
library(xtable)
library(tidyverse)

folder_name <- "0339_buy_house_now"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Do some data analysis to establish a long-term growth rate
raw <- read.csv(paste0(importdir, "/0339_housing_data/IUSEHMSP_IUS30YMR_data.csv"),
                col.names = c("date", "median_price", "mortgage_rate")) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d"),
               mt = month(date),
               yr = year(date)) %>%
        arrange(date)

by_month <- raw %>%
              filter(date >= "2020-01-01",
                     date < "2023-02-01") %>%
              group_by(yr, mt) %>%
              summarise(median_price = max(median_price, na.rm = TRUE),
                        mortgage_rate = mean(mortgage_rate, na.rm = TRUE)/100) %>%
              ungroup() %>%
              mutate(date = as.Date(paste0(yr, "-", mt, "-01"))) %>%
              select(date, median_price, mortgage_rate) 

to_plot <- by_month %>%
        mutate(down_payment = 0.2*median_price,
               mortgage = median_price - down_payment,
               mortage_rate_monthly = mortgage_rate/12,
               monthly_payment = mortgage/((1 - (1 + mortage_rate_monthly)^(-360))/mortage_rate_monthly))

file_path <- paste0(out_path, "/mortgage_payment_for_median_home_2023.jpeg")
source_string <- str_wrap(paste0("Source: YCharts (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Assumes a 20% down payment and a 30-year fixed rate mortgage."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=monthly_payment)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Monthly Mortgage Payment Required to\nPurchase the Median U.S. Home")) +
  labs(x = "Month" , y = "Mortgage Payment ($)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #