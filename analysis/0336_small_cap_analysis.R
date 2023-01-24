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
library(tidyverse)

folder_name <- "0336_small_cap_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

lag_months <- 12

raw <- read.csv(paste0(importdir, "/0336_small_cap_analysis/GrowthOfWealth_20230123090722.csv"),
                skip = 7, col.names = c("date", "index_small_cap", "index_sp500", "index_cpi")) %>%
          filter(!is.na(index_sp500)) %>%
          mutate(date = as.Date(date, format = "%m/%d/%Y"),
                 index_real_sc = index_small_cap/index_cpi,
                 index_real_sp500 = index_sp500/index_cpi) %>%
            select(date, contains("index_real"), contains("ret_real"))

first_values <- raw %>%
                  head(1) %>%
                  rename(first_sc = index_real_sc,
                         first_sp500 = index_real_sp500) %>%
                  select(contains("first_")) %>%
                  mutate(merge_key = 1)

df <- raw %>%
        mutate(merge_key = 1) %>%
        left_join(first_values) %>%
        mutate(index_real_sc = index_real_sc/first_sc,
               index_real_sp500 = index_real_sp500/first_sp500) %>%
        select(-merge_key, -first_sc, -first_sp500)

dd_sc <- drawdown_path(df %>% select(date, index_real_sc)) %>%
              mutate(key = "Small Cap Stocks")
dd_sp500 <- drawdown_path(df %>% select(date, index_real_sp500)) %>%
            mutate(key = "S&P 500")

to_plot <- dd_sc %>%
  bind_rows(dd_sp500)

to_plot$key <- factor(to_plot$key, levels = c("Small Cap Stocks", "S&P 500"))

file_path <- paste0(out_path, "/dd_sc_vs_sp500_1927_2022.jpeg")
source_string <- str_wrap(paste0("Source: Returns 2.0 (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns include dividends and are adjusted for inflation. ",
                               "Small cap stocks are represented by the Fama/French US Small Cap Research Index which 
                                contains the lower 50% market equity range of NYSE firms."),
                        width = 85)


plot <- ggplot(to_plot, aes(x=date, y=pct, fill = key)) +
  geom_area(position = "identity") +
  facet_wrap(. ~ key) +
  scale_fill_manual(values = c("blue", "black"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Drawdowns by Stock Type")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot decade returns
decades <- df %>%
  filter(year(date) >= 1930, year(date) < 2020) %>%
  mutate(decade = floor(year(date)/10) * 10) %>%
  group_by(decade) %>%
  summarise(min_date = min(date),
            max_date = max(date)) %>%
  ungroup()

first_values <- df %>%
  inner_join(decades %>% rename(date = min_date) %>% select(date, decade))

last_values <- df %>%
  inner_join(decades %>% rename(date = max_date) %>% select(date, decade)) %>%
  rename(last_real_sc = index_real_sc,
         last_real_sp500 = index_real_sp500) %>%
  select(decade, contains("last_"))

to_plot <- first_values %>%
  inner_join(last_values) %>%
  mutate(delta_sc = (last_real_sc/index_real_sc)^(1/10) - 1,
         delta_sp500 = (last_real_sp500/index_real_sp500)^(1/10) - 1) %>%
  select(decade, contains("delta_")) %>%
  gather(-decade, key=key, value=value) %>%
  mutate(key = case_when(
    key == "delta_sc" ~ "Small Cap Stocks",
    key == "delta_sp500" ~ "S&P 500",
    TRUE ~ "Error"
  ))

to_plot$key <- factor(to_plot$key, levels = c("Small Cap Stocks", "S&P 500"))

file_path <- paste0(out_path, "/sc_vs_sp500_rets_by_decade_1930_2019.jpeg")

plot <- ggplot(data = to_plot, aes(x=as.factor(decade), y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("blue", "black")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Annualized Real Stock Returns\nBy Type and Decade")) +
  labs(x = "Decade", y = "Annualized Real Return (Over Decade)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Plot rolling 10-year net returns
plot_n_year_net_diff <- function(n_years, 
                                 y_max,
                                 upper_label_pct, 
                                 date_upper,
                                 y_min,
                                 lower_label_pct,
                                 date_lower
                                 ){
  lag_months <- n_years*12
  
  to_plot <- df %>%
              mutate(ret_n_yr_sc = (index_real_sc/lag(index_real_sc, lag_months))^(1/n_years) - 1,
                     ret_n_yr_sp500 = (index_real_sp500/lag(index_real_sp500, lag_months))^(1/n_years) - 1,
                     net_diff = ret_n_yr_sc - ret_n_yr_sp500
                     ) %>%
                drop_na()
  
  text_labels <- data.frame()
  
  text_labels[1, "date"] <- date_upper
  text_labels[1, "net_diff"] <- upper_label_pct
  text_labels[1, "label"] <- "Small Caps Outperform"
  text_labels[2, "date"] <- date_lower
  text_labels[2, "net_diff"] <- lower_label_pct
  text_labels[2, "label"] <- "Small Caps Underperform"
  
  file_path <- paste0(out_path, "/sc_vs_sp500_rolling_", n_years, "_yr_diff.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x=date, y = net_diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
    geom_line() +
    geom_text(data = text_labels, aes(x=date, y=net_diff, label = label),
              family = "my_font") +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_min, y_max), breaks = seq(y_min, y_max, 0.05)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Small Cap Stocks - S&P 500\nDifference in Annualized Returns Over ", n_years, " Years")) +
    labs(x = "Ending Date", y = paste0("Annualized Difference Over ", n_years, " Years"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_n_year_net_diff(10, 
                     .15, 
                     .15,
                     as.Date("1980-01-01"),
                     -0.1, 
                    -0.09,
                     as.Date("1980-01-01"))

plot_n_year_net_diff(20, 
                     .1, 
                     .09,
                     as.Date("1985-01-01"),
                     -0.05, 
                     -0.05,
                     as.Date("1985-01-01"))

# ############################  End  ################################## #