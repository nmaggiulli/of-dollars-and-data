cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(tidyverse)

folder_name <- "_mttw/0009_taiwan_ls_vs_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_msci <- read.csv(paste0(importdir, "/", folder_name, "/MSTW_data.csv"),
                     col.names = c("date", "index_tw")) %>%
            mutate(date = as.Date(date),
                   month = as.Date(paste0(year(date), "-", month(date), "-01"))) %>%
            arrange(date) %>%
            filter(date < "2025-11-01")

tw_monthly <- raw_msci %>%
                group_by(month) %>%
                summarise(index_tw = mean(index_tw)) %>%
                ungroup() %>%
                mutate(index_ret = index_tw/lag(index_tw) - 1) %>%
                drop_na()

dates <- seq(from = min(tw_monthly$month), 
             to = max(tw_monthly$month), 
             by = "month")

returns_data <- tibble(
  date = dates,
  monthly_return = tw_monthly$index_ret
)

#Function to calculate outcomes for a single 12-month period
calculate_period_returns <- function(data, start_idx) {
  # Get 12 months of returns starting from start_idx
  period_returns <- data$monthly_return[start_idx:(start_idx + 11)]
  
  # Lump Sum: invest $1,200 immediately
  lump_sum_value <- 1200 * prod(1 + period_returns)
  
  # DCA: invest $100 each month
  dca_value <- 0
  for (i in 1:12) {
    # Each $100 investment gets returns for the remaining months
    months_invested <- 12 - i + 1
    investment_returns <- period_returns[i:12]
    dca_value <- dca_value + 100 * prod(1 + investment_returns)
  }
  
  return(tibble(
    start_date = data$date[start_idx],
    end_date = data$date[start_idx + 11],
    lump_sum_final = lump_sum_value,
    dca_final = dca_value,
    lump_sum_return = (lump_sum_value - 1200) / 1200,
    dca_return = (dca_value - 1200) / 1200,
    ls_advantage = lump_sum_value - dca_value
  ))
}

# Loop through all possible 12-month periods
n_periods <- nrow(returns_data) - 11

results <- map_df(1:n_periods, ~calculate_period_returns(returns_data, .x))

# Summary statistics
summary_stats <- results %>%
  summarise(
    n_periods = n(),
    ls_wins = sum(lump_sum_final > dca_final),
    dca_wins = sum(dca_final > lump_sum_final),
    ls_win_pct = mean(lump_sum_final > dca_final) * 100,
    avg_ls_advantage = mean(ls_advantage),
    median_ls_advantage = median(ls_advantage),
    avg_ls_return = mean(lump_sum_return) * 100,
    avg_dca_return = mean(dca_return) * 100
  )

print(summary_stats)

to_plot <- results

# Plot the results
file_path <- paste0(out_path, "/ls_vs_dca_taiwan_port.jpeg")

plot <-  to_plot %>%
  ggplot(aes(x = start_date)) +
  geom_line(aes(y = lump_sum_final, color = "Lump Sum"), linewidth = 0.8) +
  geom_line(aes(y = dca_final, color = "DCA"), linewidth = 0.8) +
  labs(
    title = "Final Portfolio Value: Lump Sum vs DCA",
    x = "Period Start Date",
    y = "Final Value ($)",
    color = "Strategy"
  ) +
  of_dollars_and_data_theme

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Distribution of advantage
file_path <- paste0(out_path, "/ls_vs_dca_taiwan_dist.jpeg")

plot <- to_plot %>%
  ggplot(aes(x = ls_advantage)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Lump Sum Advantage over DCA",
    x = "Lump Sum Advantage ($)",
    y = "Count"
  ) +
  of_dollars_and_data_theme
  
# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #