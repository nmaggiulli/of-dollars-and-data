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

spxtr <- read.csv(paste0(importdir, "0176_aaii_asset_allocation/spxtr_1989_2020.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")) %>%
  arrange(date) %>%
  select(date, value) %>%
  rename(index_sp500 = value)

df <- raw %>%
        bind_cols(data.frame(date=all_dates)) %>%
        mutate(pct_stocks = round(as.numeric(pct_stock_funds) + as.numeric(pct_ind_stocks), 3),
               pct_bonds = round(as.numeric(pct_bond_funds) + as.numeric(pct_ind_bonds), 3),
               pct_cash = 1 - pct_stocks - pct_bonds,
               pct_cash_change = pct_cash - lag(pct_cash,1)) %>%
        select(date, pct_stocks, pct_bonds, pct_cash, pct_cash_change) %>%
        left_join(spxtr)
  
to_plot <- df

file_path <- paste0(out_path, "/aaii_cash_change.jpeg")
source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey (OfDollarsAndData.com)"),
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

file_path <- paste0(out_path, "/aaii_all_pct_anno.jpeg")

curr_stock_allocation <- pull(df[nrow(df), "pct_stocks"])

plot <- ggplot(to_plot, aes(x=date, y=value, fill = key)) +
  geom_area() +
  scale_fill_manual(values = c("black", "blue", "green")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  geom_hline(yintercept = curr_stock_allocation, linetype="dashed", col = "black") +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right") +
  ggtitle(paste0("Investor Allocations Over Time")) +
  labs(x = "Date" , y = "Allocation",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot index of S&P 500
first_value <- spxtr[1, "index_sp500"]

to_plot <- df %>%
            select(date, pct_stocks, index_sp500) %>%
            drop_na() %>%
            mutate(below_curr = ifelse(pct_stocks < curr_stock_allocation, 1, 0),
                   index_sp500 = index_sp500/first_value)

file_path <- paste0(out_path, "/aaii_sp500_stocks_below_curr.jpeg")
source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey, YCharts (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns shown include dividends, but not adjusted for inflation."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=index_sp500, col = as.factor(below_curr), group = 1)) +
  geom_line() +
  scale_color_manual(values = c("black", "green"), guide = FALSE) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 and When Investor Stock Allocations\nWere Less than ", 
                 round(100*curr_stock_allocation, 0), "%")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

plot_fwd_ret <- function(n_years){
  to_plot <- df %>%
              mutate(fwd_ret = (lead(index_sp500, n_years*12)/index_sp500)^(1/n_years) - 1) %>%
              drop_na()
  
  fwd_ret_string <- str_pad(n_years, width = 2, pad = "0", side = "left")
  
  file_path <- paste0(out_path, "/aaii_fwd_ret_", fwd_ret_string, "yr.jpeg")
  source_string <- str_wrap(paste0("Source:  AAII Asset Allocation Survey, YCharts (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns shown are annualized with dividends, but not adjusted for inflation."),
                            width = 85)
  
  plot <- ggplot(to_plot, aes(x=pct_cash, y=fwd_ret)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "lm") +
    geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(n_years, "-Year Future Return vs.\nInvestor Allocation to Cash")) +
    labs(x = "% Allocation to Cash" , y = paste0(n_years,"-Year Future Return"),
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_fwd_ret(1)
plot_fwd_ret(10)


# ############################  End  ################################## #