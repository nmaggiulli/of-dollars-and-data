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
library(tidylog)
library(zoo)
library(ggjoy)
library(tidyverse)

folder_name <- "0272_russell_3000_mcaps"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

mcap <- read.csv(paste0(importdir, "/0272_russell_3000_mcaps/timeseries_12-10-2021.csv"),
                   skip = 6) %>%
  select(-Metric, -Name) %>%
  rename(symbol = Symbol) %>%
  gather(-symbol, key=key, value=mcap) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  select(date, symbol, mcap) %>%
  drop_na() %>%
  arrange(symbol, date, mcap)

min_date <- min(mcap$date)
max_date <- max(mcap$date)

beginning_deciles <- mcap %>%
                filter(date == min_date) %>%
                arrange(desc(mcap)) %>%
                mutate(decile = ntile(mcap, 10)) %>%
                select(symbol, decile)

df <- mcap %>%
        inner_join(beginning_deciles) 

mcaps_by_decile <- df %>%
                      group_by(date, decile) %>%
                      summarise(mcap = sum(mcap),
                                n_stocks = n()) %>%
                      ungroup()

starting_mcaps <- df %>%
                    filter(date == min_date) %>%
                    group_by(decile) %>%
                    summarise(total_mcap = sum(mcap),
                              min_mcap = min(mcap),
                              median_mcap = quantile(mcap, probs = 0.5),
                              max_mcap = max(mcap)) %>%
                    ungroup() 

to_plot <- mcaps_by_decile %>%
              left_join(starting_mcaps) %>%
              mutate(pct_change = mcap/total_mcap - 1) %>%
              filter(date == max_date) %>%
              select(decile, pct_change)

source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Size deciles are based on 2021 starting market capitalization."),
                        width = 85)

file_path <- paste0(out_path, "/russell_3000_size_decile_returns.jpeg")

plot <- ggplot(data = to_plot, aes(x = decile, y = pct_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  ggtitle(paste0("Russell 3000 YTD Return\nBy Size Decile")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "Smaller Stocks -> Larger Stocks" , y = "YTD Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #