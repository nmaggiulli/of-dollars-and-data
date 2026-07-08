cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "0511_debasement_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in sp500 Shiller 
sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  filter(date >= as.Date("2020-01-01"))

first <- pull(sp500_ret_pe[1, "price_plus_div"])
first_p <- pull(sp500_ret_pe[1, "price"])
first_cpi <- pull(sp500_ret_pe[1, "cpi"])

df <- sp500_ret_pe %>%
          mutate(pct_change_real = price_plus_div/first-1,
                 price_growth = price/first_p,
                 cpi_change = cpi/first_cpi) %>%
          select(date, pct_change_real, price_growth, cpi_change)

to_plot <- df

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/sp500_2020_2026_may_pct_change.jpeg")
source_string <- "Source: Shiller data (OfDollarsAndData.com)"
note_string <- "Note: Performance includes dividends and is adjusted for inflation."

plot <- ggplot(to_plot, aes(x = date, y = pct_change_real)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Total Inflation-Adjusted Return")) +
  labs(x = "Year" , y = "Percentage Change",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Side analysis on longest negative holding period
all_spx <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            select(date, price_plus_div)

df <- all_spx %>% arrange(date)

n      <- nrow(df)
prices <- df$price_plus_div
dates  <- df$date

# 1. Identify "record high" points (running max) — these are the only
#    candidates for the earliest s where price[s] >= price[t]
running_max <- cummax(prices)
is_record   <- prices == running_max          # TRUE at each new all-time high
record_idx  <- which(is_record)
record_val  <- prices[record_idx]

# 2. For each t, find the earliest record point whose value >= price[t]
#    (record_val is strictly increasing, so findInterval works as a binary search)
earliest_s <- record_idx[findInterval(prices - 1e-9, record_val) + 1]
# findInterval gives the count of records <= price[t]; +1 gives the first record >= price[t]

# 3. Compute the gap (in months) for every t, and find the max
gap <- seq_len(n) - earliest_s
best_t <- which.max(gap)
best_s <- earliest_s[best_t]

result <- data.frame(
  start_date   = dates[best_s],
  start_price  = prices[best_s],
  end_date     = dates[best_t],
  end_price    = prices[best_t],
  months       = gap[best_t],
  years        = round(gap[best_t] / 12, 1)
)

result

# ############################  End  ################################## #