cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0330_rate_changes_v_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  select(date, long_irate, price_plus_div, real_earn_scaled, real_tr) %>%
                  filter(year(date) >= 1914)

n_months_long <- 60

to_plot <- sp500_ret_pe %>%
              mutate(rate_change_5yr = long_irate - lag(long_irate, n_months_long),
                    ret_prior_5yr = (price_plus_div/lag(price_plus_div, n_months_long)) - 1,
                    rate_change_1yr = long_irate - lag(long_irate, 12),
                    ret_prior_1yr = (price_plus_div/lag(price_plus_div, 12)) - 1,
                    ret_next_1yr = (lead(price_plus_div, 12)/price_plus_div  - 1),
                    ret_next_5yr = (lead(price_plus_div, n_months_long)/price_plus_div  - 1),
                    decade_1980 = ifelse(year(date) >= 1980 & year(date) < 1990, 1, 0)
                    ) %>%
              filter(month(date) %in% c(1, 4, 7, 10))

x_label <- "Change in 10-Year Treasury Rate"
y_label <- "Total Return in U.S. Stocks"

x_lower <- -0.08
x_upper <- 0.08
y_lower <- -0.5
y_upper <- 3

file_path <- paste0(out_path, "/rate_change_5yr_vs_sp500_change_prior_5yr_fix.jpg")
source_string <- paste0("Source: Shiller data, 1914-2022 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: The performance of U.S. stocks includes dividends and is adjusted for inflation. "),
                               width = 80)

plot <- ggplot(data = to_plot, aes(x=rate_change_5yr, y=ret_prior_5yr)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(x_lower, x_upper)) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_lower, y_upper)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Prior 5 Years) vs.\nChange in 10-Year Rate (Over Prior 5 Years)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Show 1980s highlighted
file_path <- paste0(out_path, "/rate_change_5yr_vs_sp500_5yr_1980_highlight_fix.jpg")

plot <- ggplot(data = to_plot, aes(x=rate_change_5yr, y=ret_prior_5yr, col = as.factor(decade_1980))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), guide = "none") +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(x_lower, x_upper)) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_lower, y_upper)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Prior 5 Years) vs.\nChange in 10-Year Rate (Over Prior 5 Years)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

mod_all <- lm(ret_prior_5yr ~ rate_change_5yr, data = to_plot)
cf_all <- coef(mod_all)
r2_all <- summary(mod_all)$r.squared

# Remove 1980s highlighted
file_path <- paste0(out_path, "/rate_change_5yr_vs_sp500_5yr_1980_remove_fix.jpg")
note_string <- str_wrap(paste0("Note: The performance of U.S. stocks includes dividends and is adjusted for inflation. ",
                               "Data from the mid-1970s to mid-1980s have been removed from the plot above."),
                        width = 80)

to_plot_no_1980s <- to_plot %>%
                      filter(!(year(date) >= 1980 & year(date) < 1990))

plot <- ggplot(data = to_plot_no_1980s, aes(x=rate_change_5yr, y=ret_prior_5yr)) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), guide = "none") +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(x_lower, x_upper)) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_lower, y_upper)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Prior 5 Years) vs.\nChange in 10-Year Rate (Over Prior 5 Years)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

mod_no_1980s <- lm(ret_prior_5yr ~ rate_change_5yr, data = to_plot_no_1980s)
cf_no_1980s <- coef(mod_no_1980s)
r2_no_1980s <- summary(mod_no_1980s)$r.squared

# Do returns over the next 5 years for S&P 500
file_path <- paste0(out_path, "/_check_rate_change_5yr_vs_sp500_change_next_5yr_fix.jpg")
note_string <- str_wrap(paste0("Note: The performance of U.S. stocks includes dividends and is adjusted for inflation. "),
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=rate_change_5yr, y=ret_next_5yr)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Next 5 Years) vs.\nChange in 10-Year Rate (Over Prior 5 Years)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

## As a check look at stock returns over next 1yr and rate changes over prior 1yr
file_path <- paste0(out_path, "/_check_rate_change_5yr_vs_sp500_change_next_1yr.jpg")

plot <- ggplot(data = to_plot, aes(x=rate_change_5yr, y=ret_next_1yr)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Next Year) vs.\nChange in 10-Year Rate (Over Prior 5 Years)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do rate change 1yr vs next year stocks
file_path <- paste0(out_path, "/_check_rate_change_1yr_vs_sp500_change_next_1yr.jpg")

plot <- ggplot(data = to_plot, aes(x=rate_change_1yr, y=ret_next_1yr)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Return in U.S. Stocks (Over Next Year) vs.\nChange in 10-Year Rate (Over Prior Year)")) +
  labs(x = x_label, y = y_label,
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")
## Check done

# Plot Treasury Rate
to_plot <- sp500_ret_pe 

file_path <- paste0(out_path, "/long_irate_1914_2022_fix.jpg")
source_string <- paste0("Source: Shiller data, 1914-2022 (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x=date, y=long_irate)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 0.16, 0.02)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("10-Year Treasury Rate Since 1914")) +
  labs(x = "Year", y = "10-Year Rate",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot real equity returns vs change in Treasury rate
decades <- sp500_ret_pe %>%
            filter(year(date) >= 1920, year(date) < 2020) %>%
            mutate(decade = floor(year(date)/10) * 10) %>%
            group_by(decade) %>%
            summarise(min_date = min(date),
                      max_date = max(date)) %>%
            ungroup()

first_values <- sp500_ret_pe %>%
                  inner_join(decades %>% rename(date = min_date) %>% select(date, decade))

last_values <- sp500_ret_pe %>%
  inner_join(decades %>% rename(date = max_date) %>% select(date, decade)) %>%
  rename(last_long_irate = long_irate,
         last_price_plus_div = price_plus_div) %>%
    select(decade, contains("last_"))

to_plot <- first_values %>%
              inner_join(last_values) %>%
              mutate(delta_rates = last_long_irate - long_irate,
                     delta_sp500 = (last_price_plus_div/price_plus_div)^(1/10) - 1) %>%
              select(decade, contains("delta_")) %>%
              gather(-decade, key=key, value=value) %>%
              mutate(key = case_when(
                key == "delta_rates" ~ "Change in Rates",
                key == "delta_sp500" ~ "Real U.S. Stock Return (Ann.)",
                TRUE ~ "Error"
              ))

file_path <- paste0(out_path, "/long_irate_vs_sp500_ret_by_decades_fix.jpg")
source_string <- paste0("Source: Shiller data, 1920-2019 (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x=as.factor(decade), y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "red")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Annualized Real U.S. Stock Return &\nChange in 10-Year Treasury Rates\nBy Decade")) +
  labs(x = "Decade", y = "Total Change Over Decade",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #