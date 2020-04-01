cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(lemon)
library(Hmisc)
library(readxl)
library(tidyverse)

folder_name <- "0175_absolute_price_movement"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index")) %>%
        mutate(date = as.Date(date),
               mt = month(date),
               yr = year(date)) %>%
        arrange(date) %>%
        mutate(prior_index =lag(index),
          ret = index/prior_index - 1) %>%
        drop_na()

min_dates <- dow %>%
                group_by(yr, mt) %>%
                summarize(min_date = min(date)) %>%
                ungroup() %>%
                rename(date = min_date) %>%
                inner_join(dow) %>%
                rename(min_index = prior_index) %>%
                select(yr, mt, min_index)

min_max <- dow %>%
  group_by(yr, mt) %>%
  summarize(max_date = max(date)) %>%
  ungroup() %>%
  rename(date = max_date) %>%
  inner_join(dow) %>%
  rename(max_index = index) %>%
  select(yr, mt, max_index) %>%
  left_join(min_dates) %>%
  mutate(mt_ret = max_index/min_index - 1) %>%
  arrange(mt_ret)

summary <- dow %>%
            group_by(yr, mt) %>%
            summarize(abs_ret = sum(abs(ret), na.rm = TRUE),
                      n_days = n(),
                      avg_abs_ret = abs_ret/n_days) %>%
            ungroup() %>%
            arrange(-abs_ret)

to_plot <- summary %>%
            mutate(date = as.Date(
              paste0(
                yr, 
                "-",
                mt, 
                "-01"
              )
            ))

mean_ret <- mean(summary$abs_ret)

# Set note and source string
source_string <- str_wrap("Source: Bloomberg (OfDollarsAndData.com)",
                          width = 85)
note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends.  ",
                               "The average cumulative absolute percentage change in a given month is ",
                               round(100*mean_ret, 1), 
                               "%."),
                        width = 85)

file_path <- paste0(out_path, "/dow_abs_ret_by_month.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=abs_ret)) +
  geom_bar(stat = "identity", width = 31) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Cumulative Absolute Percentage Change\nby Month")) +
  labs(x = "Date" , y = "Cumulative Absolute Percentage Change",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- dow %>%
  filter(yr == 2020, mt == 3)

file_path <- paste0(out_path, "/dow_2020_03_breaks.jpeg")
note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends.  "),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=ret)) +
  geom_bar(stat = "identity", width = 1, fill = chart_standard_color) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.15, 0.15), 
                     breaks = seq(-0.15, 0.15, 0.05)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Daily Percentage Change\nMarch 2020")) +
  labs(x = "Date" , y = "Daily Percentage Change",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Do a side by side comparison with Jan 2020
jan_2020 <- dow %>%
  filter(yr == 2020, mt == 1) %>%
  mutate(label = "Jan 2020")

to_plot <- to_plot %>%
            mutate(label = "Mar 2020") %>%
            bind_rows(jan_2020)

file_path <- paste0(out_path, "/dow_2020_01_compare.jpeg")
note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends.  "),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=ret)) +
  geom_bar(stat = "identity", width = 1, fill = chart_standard_color) +
  facet_rep_wrap(label ~ ., scales = "free_x", repeat.tick.labels = c("left", "bottom")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.15, 0.15),
                     breaks = seq(-0.15, 0.15, 0.05)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Daily Percentage Change")) +
  labs(x = "Date" , y = "Daily Percentage Change",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #