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

folder_name <- "/_fl/0007_not_dotcom"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_fred <- read.csv(paste0(importdir, "/_fl/0007_not_dotcom/fred_DGS10.csv")) %>%
              clean_cols() %>%
              mutate(date = as.Date(date),
                     rate_10yr = as.numeric(dgs10)/100,
                     mt = month(date),
                     yr = year(date)) %>%
              drop_na()

#Do nasdaq
nq <- read.csv(paste0(importdir, "_fl/0007_not_dotcom/ycharts_IXIC_data.csv")) %>%
  rename(index = `NASDAQ.Composite.Level`) %>%
  mutate(date = as.Date(Period)) %>%
  arrange(date) %>%
  select(date, index) %>%
  filter(date >= as.Date("1990-01-01"))

# Dates of interest for NASDAQ
bubble_high <- as.Date("2000-03-10")
bubble_low <- as.Date("2002-10-09")

highest_nq <- filter(nq, date == bubble_high) %>%
  pull(index)

highest_nq_mcap <- 6600

nq <- nq %>%
  mutate(mcap_billions = highest_nq_mcap*index/highest_nq)

# 10 year
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

df <- monthly_10yr %>%
        left_join(sp500_pe) %>%
        mutate(equity_premium = earnings_yield - rate_10yr) %>%
        drop_na()

to_plot <- df

file_path <- paste0(out_path, "/equity_earnings_premium.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, FRED (OfDollarsAndData.com)")

text_labels <- data.frame(date = c(as.Date("1993-01-01"), as.Date("1993-01-01")),
                          equity_premium = c(0.05, -0.05),
                          label = c("Stocks Are More Attractive",
                                    "Stocks Are Less Attractive"))

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = equity_premium)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=equity_premium, label = label),
            family = "my_font",
            size = 3.2) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.05, 0.05), breaks = seq(-0.05, 0.05, 0.01)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Stock Earnings Yield\nMinus 10-Year Treasury Rate")) +
  labs(x = "Date" , y = "Equity Earnings Premium",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- df %>%
            select(date, rate_10yr, earnings_yield) %>%
            rename(`10-Year Treasuries` = rate_10yr,
                   `Earnings Yield` = earnings_yield) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/rate_10yr_and_earnings_yield.jpeg")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16), breaks = seq(0, 0.16, 0.02)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("10-Year Treasury Rate and\nU.S. Stock Earnings Yield")) +
  labs(x = "Date" , y = "Yield",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #