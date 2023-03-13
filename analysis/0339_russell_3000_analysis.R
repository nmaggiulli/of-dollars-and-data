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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)
library(tidylog)

folder_name <- "0339_russell_3000_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0339_ycharts_russell_3000/timeseries_3-12-2023.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(date = as.Date(key, format= "X%Y.%m.%d")) %>%
  arrange(symbol, date) %>%
  select(date, symbol, value)

full_symbols <- raw %>%
                  drop_na() %>%
                  group_by(symbol) %>%
                  summarise(n_months = n()) %>%
                  ungroup() %>%
                  filter(n_months == 25) %>%
                  select(symbol)

raw_russell <- read.csv(paste0(importdir, "0339_ycharts_russell_3000/russell_3000_index_only.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(date = as.Date(key, format= "X%Y.%m.%d")) %>%
  arrange(symbol, date) %>%
  select(date, symbol, value)

df <- raw %>%
        inner_join(full_symbols) %>%
        bind_rows(raw_russell)

plot_growth_of_dollar <- function(start_date, end_date, outname){
  tmp <- df %>%
            filter(date >= start_date,
                   date <= end_date)
  
  yr <- year(end_date)
  
  first_values <- tmp %>%
                  filter(date == min(tmp$date)) %>%
                  rename(first_value = value) %>%
                  select(symbol, first_value)
  
  to_plot <- tmp %>%
            left_join(first_values) %>%
            mutate(value = value/first_value) %>%
            select(-first_value) %>%
            arrange(date, value)
  
  to_plot_all <- to_plot %>%
                  filter(symbol != "^RUATR")
  
  n_symbols <- to_plot_all %>%
                  select(symbol) %>%
                  distinct()
  
  to_plot_index <- to_plot %>%
    filter(symbol == "^RUATR")
  
  assign(paste0("to_plot_index_", yr), to_plot_index, envir = .GlobalEnv)
  
  file_path <- paste0(out_path, "/growth_of_dollar_", outname, ".jpeg")
  source_string <- "Source:  YCharts (OfDollarsAndData.com)"
  note_string <- str_wrap(paste0("Note: All performance includes dividends, but is not adjusted for inflation. ",
                          "Y-axis is a log scale. Red line is the Russell 3000 Index."),
                          width = 85)

  plot <- ggplot(to_plot_all, aes(x=date, y=value, color = symbol)) +
    geom_line() +
    scale_color_manual(values = rep("gray",nrow(n_symbols)), guide = "none") +
    geom_line(data = to_plot_index, aes(x=date, y = value), color = "red") +
    scale_y_continuous(label = dollar, trans = log10_trans()) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Growth of $1\nRussell 3000 Stocks vs. Russell 3000 Index\n", yr)) +
    labs(x="Date", y="Growth of $1",
         caption = paste0(source_string, "\n", note_string))

  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_growth_of_dollar("2020-12-31", "2021-12-31", "russell_3000_vs_index_2021")
plot_growth_of_dollar("2021-12-31", "2022-12-31", "russell_3000_vs_index_2022")

# ############################  End  ################################## #