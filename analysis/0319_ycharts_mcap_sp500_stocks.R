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
library(tidylog)
library(zoo)
library(tidyverse)
library(tidylog)

folder_name <- "0319_ycharts_mcap_sp500_stocks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_pre_2020 <- read_excel(paste0(importdir, "0159_ycharts_mcap_sp500_stocks/Historical Market Cap Data.xlsx"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(date = as.Date(as.numeric(key), origin = "1900-01-01"),
         year = year(date),
         month = month(date),
         name = str_remove_all(str_remove_all(name, " Corp"), " Inc")) %>%
  filter(symbol != "", !is.na(value), symbol != "GOOG") %>%
  rename(mcap_millions = value) %>%
  select(date, year, month, symbol, name, mcap_millions) %>%
  arrange(symbol, date) %>%
  filter(year(date) >= 2010) %>%
  arrange(date, -mcap_millions) %>%
  group_by(date) %>%
  mutate(rank = row_number(),
         name = case_when(name == "International Business Machines" ~ "IBM",
                          name == "Alphabet" ~ "Alphabet (Google)",
                          TRUE ~ name),
         faamg = ifelse(name %in% c("Facebook", "Apple", "Amazon.com", "Microsoft", "Alphabet (Google)"), 1, 0)) %>%
  ungroup()

raw_2020 <- read.csv(paste0(importdir, "0319_ycharts_mcap_sp500_stocks/timeseries_10-29-2022.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(date = as.Date(key, format = "X%Y.%m.%d"),
         year = year(date),
         month = month(date),
         name = str_remove_all(str_remove_all(name, " Corp"), " Inc")) %>%
  filter(symbol != "", !is.na(value), symbol != "GOOG") %>%
  rename(mcap_millions = value) %>%
  select(date, year, month, symbol, name, mcap_millions) %>%
  arrange(symbol, date) %>%
  filter(year(date) >= 2010) %>%
  arrange(date, -mcap_millions) %>%
  group_by(date) %>%
  mutate(rank = row_number(),
         name = case_when(name == "International Business Machines" ~ "IBM",
                          name == "Alphabet" ~ "Alphabet (Google)",
                          TRUE ~ name),
         faamg = ifelse(name %in% c("Facebook", "Apple", "Amazon.com", "Microsoft", "Alphabet (Google)"), 1, 0)) %>%
  ungroup()

raw <- raw_pre_2020 %>%
          bind_rows(raw_2020)

# Plot FAAMG market share over time
all_mcap <- raw %>%
              group_by(date) %>%
              summarise(mcap_total = sum(mcap_millions)) %>%
              ungroup()

to_plot <- raw %>%
                filter(faamg == 1) %>%
                group_by(date) %>%
                summarise(mcap_faamg = sum(mcap_millions)) %>%
                ungroup() %>%
                left_join(all_mcap) %>%
                mutate(pct_faamg = mcap_faamg/mcap_total)

file_path <- paste0(out_path, "/mcap_faamg.jpeg")
source_string <- "Source:  YCharts, 2010-2022 (OfDollarsAndData.com)"

number_labels <- to_plot %>%
  filter(date == max(to_plot$date) | date == min(to_plot$date)) %>%
  mutate(label = paste0(round(100*pct_faamg, 1), "%"))

plot <- ggplot(to_plot, aes(x=date, y=pct_faamg)) +
  geom_line() +
  geom_text_repel(data = number_labels, aes(x=date, y = pct_faamg),
                  label = number_labels$label,
                  size = 3,
                  nudge_y = -0.005,
                  segment.color = "transparent") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("FAAMG Stocks % Share of the S&P 500")) +
  labs(x="Date", y="Percentage of S&P 500",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Do plot by top 2, 5, and 10 companies in S&P 500
tops <- c(2, 5, 10)
for(t in tops){
  top_n <- raw %>%
    filter(rank <= t)
  
  top_n_sum <- top_n %>%
    group_by(date) %>%
    summarise(mcap_top_n = sum(mcap_millions)) %>%
    ungroup() %>%
    select(date, mcap_top_n) 
  
  tmp <- raw %>%
    group_by(date) %>%
    summarise(mcap_year = sum(mcap_millions)) %>%
    ungroup() %>%
    left_join(top_n_sum) %>%
    mutate(pct_top_n = mcap_top_n/mcap_year) %>%
    rename_(.dots = setNames("pct_top_n", paste0("pct_top_", t))) %>%
    select(date, contains("pct_"))
  
  if(t == min(tops)){
    mcap_by_year <- tmp
  } else{
    mcap_by_year <- mcap_by_year %>%
      left_join(tmp)
  }
}

to_plot <- mcap_by_year %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/mcap_tiers.jpeg")
source_string <- "Source:  YCharts, 2010-2019 (OfDollarsAndData.com)"

text_labels <- to_plot %>%
  filter(date == "2017-12-02") %>%
  mutate(label = case_when(
    grepl("_2", key) ~ "Top 2",
    grepl("_5", key) ~ "Top 5",
    grepl("_10", key) ~ "Top 10",
    TRUE ~ ""
  ))

number_labels <- to_plot %>%
  filter(date == max(to_plot$date) | date == min(to_plot$date)) %>%
  mutate(label = paste0(round(100*value, 1), "%"))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text_repel(data = text_labels, aes(x=date, y = value, col = key),
                  label = text_labels$label,
                  nudge_y = 0.03,
                  segment.color = "transparent") +
  geom_text_repel(data = number_labels, aes(x=date, y = value, col = key),
                  label = number_labels$label,
                  size = 3,
                  nudge_y = -0.005,
                  segment.color = "transparent") +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Share of Largest Companies in the S&P 500")) +
  labs(x="Date", y="Percentage of S&P 500",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #