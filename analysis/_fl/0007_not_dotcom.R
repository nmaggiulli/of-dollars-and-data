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
  rename(index = `Nasdaq.Composite.Level`) %>%
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
                  summarise(rate_10yr = mean(rate_10yr)) %>%
                  ungroup() %>%
                  mutate(date = as.Date(paste0(yr, "-", mt, "-01"))) %>%
                  select(date, rate_10yr)

sp500_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
              drop_na() %>%
              mutate(earnings_yield = 1/cape) %>%
              select(date, earnings_yield, cape)

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
  scale_color_manual(values = c("blue", "black")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16), breaks = seq(0, 0.16, 0.02)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("10-Year Treasury Rate and\nU.S. Stock Earnings Yield")) +
  labs(x = "Date" , y = "Yield",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/cape.jpeg")

to_plot <- df

text_labels <- to_plot %>% tail(1)

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = cape)) +
  geom_line(col = "black") +
  geom_point(data = text_labels, aes(x=date, y=cape), col = "red") +
  geom_text_repel(data = text_labels, aes(x=date, y = cape, label = round(cape, 1)), col = "red",
                  family = "my_font",
                  nudge_y = 5,
                  segment.colour = "transparent") +
  scale_y_continuous() +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Price-to-Earnings Ratio")) +
  labs(x = "Date" , y = "P/E",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Nasdaq
create_nq_index <- function(start_date, end_date, key){
  
  first <- nq %>%filter(date == start_date) %>% pull(index)
  print(first)
  
  tmp <- nq %>%
    filter(date >= start_date, date <= end_date) %>%
    mutate(pct_change = index/first - 1,
           day = row_number()/250,
           key = key) %>%
    select(day, pct_change, mcap_billions, key)
  
  return(tmp)
}

nq_2015 <- create_nq_index("2016-01-04", "2020-12-31", "2016-2020")
nq_1995 <-  create_nq_index("1995-01-03", "2000-01-03", "1995-1999")

to_plot <- nq_1995 %>%
              bind_rows(nq_2015)

file_path <- paste0(out_path, "/nasdaq_pct_change_end_1999_2020.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

text_labels <- to_plot %>%
                filter(day == max(to_plot$day)) %>%
                mutate(label = paste0(key, "\n+", round(100*pct_change, 0), "%"))

# Plot the results
plot <- ggplot(to_plot, aes(x = day, y = pct_change, col = key)) +
  geom_line() +
  geom_text_repel(data = text_labels, aes(x=day, y = pct_change, col = key, label = label)) +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("NASDAQ Composite 5-Year Growth")) +
  labs(x = "Year" , y = "Cumulative Percentage Change",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")








# ############################  End  ################################## #