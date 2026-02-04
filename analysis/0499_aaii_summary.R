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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "0499_aaii_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

aaii_raw <- read_excel(paste0(importdir, "0499_aaii_data/aaii.xls"))

# Create a date list to use for subsetting
end_month <- floor_date(Sys.Date(), "month")
all_dates <- seq.Date(as.Date("1987-12-01"), end_month, "month")-1

# Subset based on known properties of file
aaii <- aaii_raw[3:(length(all_dates)+2), 2:6]

colnames(aaii) <- c("stock_funds", "stocks", "bond_funds", "bonds", "cash")  

aaii <- aaii %>%
  mutate(date = as.POSIXct(all_dates) + days(1) - months(1),
         stock_allocation = as.numeric(stocks) + as.numeric(stock_funds),
         bond_allocation = as.numeric(bonds) + as.numeric(bond_funds),
         cash_allocation = as.numeric(cash)) %>%
  select(date, stock_allocation, bond_allocation, cash_allocation)

sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
          select(date, price_plus_div)

df <- aaii %>%
        left_join(sp500) %>%
        mutate(fwd_ret_10 = (lead(price_plus_div, 120)/price_plus_div)^(1/10) - 1)

to_plot <- df %>%
              select(date, stock_allocation, fwd_ret_10) %>%
              drop_na() %>%
              mutate(flag = case_when(
                year(date) == 2015 ~ 1,
                TRUE ~ 0
              ))

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/aaii_allocation_vs_sp500_rets_2025_12_31.jpeg")
source_string <- "Source: Shiller data, AAII (OfDollarsAndData.com)"
note_string <- str_wrap(paste0("Note: S&P 500 returns include dividends and are adjusted for inflation."),
                        width = 85)

plot <- ggplot(to_plot, aes(x = stock_allocation, y = fwd_ret_10)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Higher Investor Allocation to Equities\nCorresponds to Lower Future Returns")) +
  labs(x = "Average Investor Allocation to Equities" , y = "Future 10-Year Returns (Annualized)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now highlight 2025
file_path <- paste0(out_path, "/aaii_allocation_vs_sp500_rets_2025_12_31_highlighted.jpeg")

plot <- ggplot(to_plot, aes(x = stock_allocation, y = fwd_ret_10, col = as.factor(flag))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), guide = "none") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Stock Allocation vs. Future 10-Year Return\n2015 Highlighted")) +
  labs(x = "Average Investor Allocation to Equities" , y = "Future 10-Year Returns (Annualized)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/equity_allocation_over_time_1987_2025.jpeg")
source_string <- str_wrap("Source: AAII, 1987-2025",
                          width = 85)

# Plot the allocation vs the average
plot <- ggplot(aaii, aes(x = date, y = stock_allocation)) +
  geom_line() +
  geom_hline(yintercept = 0.62, col = "gray") +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Investor Allocation to Equities Over Time")) +
  labs(x = "Date" , y = "Average Investor Allocation to Equities",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #