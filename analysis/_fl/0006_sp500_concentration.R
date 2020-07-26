cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(stringr)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "/_fl/0006_stock_concentration"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, folder_name, "/TSLA_data.csv")) %>%
          mutate(date = as.Date(Period)) %>%
          rename(index = Tesla.Inc.Price) %>%
          arrange(date) %>%
          select(date, index)
             
to_plot <- drawdown_path(raw) %>%
            left_join(raw)

file_path <- paste0(out_path, "/tsla_dd.jpeg")
source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                          width = 75)

plot <- ggplot(to_plot, aes(x=date, y=pct)) +
  geom_area() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Tesla Drawdowns")) +
  labs(x = "Date", y = "Percentage of Value Lost",
       caption = paste0(source_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm") 

zeros <- to_plot %>%
            filter(pct == 0)

file_path <- paste0(out_path, "/tsla_ath.jpeg")
source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                          width = 75)

plot <- ggplot(zeros, aes(x=date, y=index)) +
  geom_point(col = "green", alpha = 0.5) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Tesla All-Time Highs Only")) +
  labs(x = "Date", y = "Price",
       caption = paste0(source_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm") 

file_path <- paste0(out_path, "/tsla_ath_price.jpeg")
source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                          width = 75)

plot <- ggplot(to_plot, aes(x=date, y=index)) +
  geom_line() +
  geom_point(data=zeros, aes(x=date, y=index), col = "green", alpha = 0.5) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Tesla Price with All-Time Highs")) +
  labs(x = "Date", y = "Price",
       caption = paste0(source_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm") 


# ############################  End  ################################## #

  
