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
library(broom)
library(tidyverse)

folder_name <- "0432_bullish_enough_future_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
        rename(index = price_plus_div) %>%
        filter(date >= as.Date("1926-01-01")) %>%
        select(date, index)

future_years <- 10
prior_years <- 25

final_results <- df %>%
  mutate(lag_ret_10 = (index/lag(index, 12*10))^(1/10) - 1,
         lag_ret_15 = (index/lag(index, 12*15))^(1/15) - 1,
         lag_ret_20 = (index/lag(index, 12*20))^(1/20) - 1,
         lag_ret_25 = (index/lag(index, 12*25))^(1/25) - 1,
         lag_ret_30 = (index/lag(index, 12*30))^(1/30) - 1,
         lead_ret_10 = (lead(index, 12*future_years)/index))

# Subset data
to_test <- final_results %>%
  filter(!is.na(lag_ret_25), !is.na(lead_ret_10))

# Run regressions to test R^2
calculate_r_squared_tidy <- function(data, future_col, lag_cols) {
  map_df(lag_cols, function(lag_col) {
    data %>%
      select(all_of(c(future_col, lag_col))) %>%
      drop_na() %>%
      # Fit linear model
      {lm(reformulate(lag_col, future_col), data = .)} %>%
      # Extract R-squared
      glance() %>%
      select(r.squared) %>%
      mutate(lag_period = lag_col)
  })
}

# Define your column names
future_col <- "lead_ret_10"
lag_cols <- c("lag_ret_10", "lag_ret_15", "lag_ret_20", "lag_ret_25", "lag_ret_30")

# Using tidyverse approach
results_tidy <- calculate_r_squared_tidy(to_test, future_col, lag_cols)

# Lookup for flagging
upper_flag_24 <- 0.0525
lower_flag_24 <- 0.0475

# Plot the results
to_plot <- final_results %>%
            rename(lag_ret = lag_ret_25,
                   lead_ret = lead_ret_10) %>%
            filter(!is.na(lag_ret), !is.na(lead_ret)) %>%
            mutate(flag_2024 = ifelse(lag_ret > lower_flag_24 & lag_ret < upper_flag_24, 1, 0),
                   flag_1998_2000 = ifelse(year(date) >= 1998 & year(date) <= 2000, 1, 0)) %>%
            select(date, lag_ret, lead_ret, contains("flag_"))
  
start_year <- min(year(final_results$date))
end_year <- max(year(final_results$date))

source_string <- str_wrap(paste0("Source: Shiller data, ", start_year, "-", end_year, " (OfDollarsAndData.com)"), 
                          width = 80)
note_string <-  str_wrap(paste0("Note:  Performance shown includes dividends and is adjusted for inflation."), 
                         width = 80)

file_path <- paste0(out_path, "/_fit_10_fwd_growth_", prior_years, "_prior_plot.jpeg")

# Toggle the second 'black' value in the scale_color_manual() below to create annotated plots
plot <- ggplot(to_plot, aes(x=lag_ret, y=lead_ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = dollar, limits = c(0, 7), breaks = seq(0, 7, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500\n", future_years, "-Year Future Growth\nBased on ", prior_years, "-Year Prior Return")) +
  labs(x= paste0(prior_years, "-Year Annualized Prior Return"), y = "Growth of $1\nOver Next Decade",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/flag_2024_10_fwd_growth_", prior_years, "_prior_plot.jpeg")

plot <- ggplot(to_plot, aes(x=lag_ret, y=lead_ret, color = as.factor(flag_2024))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = dollar, limits = c(0, 7), breaks = seq(0, 7, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500\n", future_years, "-Year Future Growth\nBased on ", prior_years, "-Year Prior Return")) +
  labs(x= paste0(prior_years, "-Year Annualized Prior Return"), y = "Growth of $1\nOver Next Decade",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Grab all flag points and then their start dates
flag_2024_points <- to_plot %>%
  filter(flag_2024 == 1)

all_dates <- flag_2024_points %>%
                select(date) 

for(i in 1:nrow(all_dates)){
  d <- pull(all_dates[i, "date"])
  tmp <- final_results %>%
            filter(date >= d) %>%
            head(future_years*12)
  
  start_index <- pull(tmp[1, "index"])
  
  tmp2 <- tmp %>%
            mutate(month = row_number(),
              growth_of_dollar = index/start_index,
                   start_period = d) %>%
            select(month, growth_of_dollar, start_period)
  
  if(d == min(all_dates$date)){
    stack <- tmp2
  } else{
    stack <- stack %>% bind_rows(tmp2)
  }
}

file_path <- paste0(out_path, "/flag_2024_all_growth.jpeg")

# Plot each ten year return
plot <- ggplot(stack, aes(x=month, y=growth_of_dollar, color = as.factor(start_period))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 120, 12)) +
  scale_y_continuous(label = dollar) +
  scale_color_discrete(guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1 for the S&P 500\nWhen Prior 25-Year Returns Were 4.75%-5.25%")) +
  labs(x= paste0("Month"), y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Finally plot 1998-2000 data
file_path <- paste0(out_path, "/flag_2000_10_fwd_growth_", prior_years, "_prior_plot.jpeg")

plot <- ggplot(to_plot, aes(x=lag_ret, y=lead_ret, color = as.factor(flag_1998_2000))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = dollar, limits = c(0, 7), breaks = seq(0, 7, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500\n", future_years, "-Year Future Growth\nBased on ", prior_years, "-Year Prior Return")) +
  labs(x= paste0(prior_years, "-Year Annualized Prior Return"), y = "Growth of $1\nOver Next Decade",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Print results in a nice format
print(results_tidy)

# ############################  End  ################################## #