cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0205_pe_vs_10yr"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_fred <- read.csv(paste0(importdir, "/0205_fred_10yr/fred_DGS10.csv")) %>%
              clean_cols() %>%
              mutate(date = as.Date(date),
                     rate_10yr = as.numeric(dgs10)/100,
                     mt = month(date),
                     yr = year(date)) %>%
              drop_na()

monthly_10yr <- raw_fred %>%
                  group_by(yr, mt) %>%
                  summarize(rate_10yr = mean(rate_10yr)) %>%
                  ungroup() %>%
                  mutate(date = as.Date(paste0(yr, "-", mt, "-01"))) %>%
                  select(date, rate_10yr)

sp500_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
              drop_na() %>%
              mutate(earnings_yield = 1/cape) %>%
              select(date, earnings_yield)

to_plot <- monthly_10yr %>%
        left_join(sp500_pe) %>%
        mutate(equity_premium = earnings_yield - rate_10yr) %>%
        drop_na()

file_path <- paste0(out_path, "/equity_earnings_premium.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, FRED (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = equity_premium)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.05, 0.05), breaks = seq(-0.05, 0.05, 0.01)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Stock Earnings Yield\nMinus 10-Year Treasury Rate")) +
  labs(x = "Date" , y = "Equity Earnings Premium",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/rate_10yr.jpeg")
source_string <- paste0("Source:  FRED (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = rate_10yr)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("10-Year Treasury Rate")) +
  labs(x = "Date" , y = "Rate",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/earnings_yield.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = earnings_yield)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Stock Earnings Yield")) +
  labs(x = "Date" , y = "Earnings Yield",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #