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
library(readxl)
library(tidyverse)

folder_name <- "0176_aaii_asset_allocation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

first_date <- as.Date("1987-11-01")
last_date <- as.Date("2020-03-01")

all_dates <- seq.Date(first_date, 
                      last_date, 
                      "month")

raw <- read_excel(paste0(importdir, "0176_aaii_asset_allocation/aaii_asset_allocation_survey.xls"),
                  skip = 2)

# Specific subsetting
raw <- raw[1:length(all_dates), 2:6]

colnames(raw) <- c("pct_stock_funds", "pct_ind_stocks",
                   "pct_bond_funds", "pct_ind_bonds", "pct_cash_char")

df <- raw %>%
        bind_cols(data.frame(date=all_dates)) %>%
        mutate(pct_stocks = round(as.numeric(pct_stock_funds) + as.numeric(pct_ind_stocks), 3),
               pct_bonds = round(as.numeric(pct_bond_funds) + as.numeric(pct_ind_bonds), 3),
               pct_cash = 1 - pct_stocks - pct_bonds,
               pct_cash_change = pct_cash - lag(pct_cash,1)) %>%
        select(date, pct_stocks, pct_bonds, pct_cash, pct_cash_change)

to_plot <- df

file_path <- paste0(out_path, "/aaii_cash_change.jpeg")
source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey"),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=pct_cash_change)) +
  geom_bar(stat = "identity", col = chart_standard_color) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Changes in Investor Allocations to Cash")) +
  labs(x = "Date" , y = "One Month Change",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/aaii_pct_cash.jpeg")
source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey"),
                          width = 85)

plot <- ggplot(to_plot, aes(x=date, y=pct_cash)) +
  geom_line(col = chart_standard_color) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Investor Allocations to Cash")) +
  labs(x = "Date" , y = "Allocation",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do area plot
to_plot <- df %>%
            select(date, pct_cash, pct_stocks, pct_bonds) %>%
            rename(Cash = pct_cash,
                   Stocks = pct_stocks,
                   Bonds = pct_bonds) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/aaii_all_pct.jpeg")
source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey"),
                          width = 85)

plot <- ggplot(to_plot, aes(x=date, y=value, fill = key)) +
  geom_area() +
  scale_fill_manual(values = c("black", "blue", "green")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right") +
  ggtitle(paste0("Investor Allocations Over Time")) +
  labs(x = "Date" , y = "Allocation",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #