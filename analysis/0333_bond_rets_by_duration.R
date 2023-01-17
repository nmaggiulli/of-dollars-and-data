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
library(tidyverse)

folder_name <- "0333_bond_rets_by_duration"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

lag_months <- 12

raw <- read.csv(paste0(importdir, "/0333_bond_data/GrowthOfWealth_20230115183214.csv"),
                       skip = 7, 
                       row.names = NULL,
                       col.names = c("date", "index_1m", "index_5yr", "index_lt", "index_cpi"))  %>%
  filter(!is.na(index_1m)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         index_real_1m = index_1m/index_cpi,
         index_real_5yr = index_5yr/index_cpi,
         index_real_lt = index_lt/index_cpi,
         ret_real_1m = index_real_1m/lag(index_real_1m, lag_months) - 1,
         ret_real_5yr = index_real_5yr/lag(index_real_5yr, lag_months) - 1,
         ret_real_lt = index_real_lt/lag(index_real_lt, lag_months) - 1,
         ) %>%
    select(date, contains("index_real"), contains("ret_real")) %>%
    drop_na()

dd_1m <- drawdown_path(raw %>% select(date, index_real_1m)) %>%
            mutate(key = "1-Month Treasury Bills")
dd_5yr <- drawdown_path(raw %>% select(date, index_real_5yr)) %>%
  mutate(key = "5-Year Treasury Notes")
dd_lt <- drawdown_path(raw %>% select(date, index_real_lt)) %>%
  mutate(key = "20-Year Treasury Bonds")

to_plot <- dd_1m %>%
            bind_rows(dd_5yr) %>%
            bind_rows(dd_lt)

to_plot$key <- factor(to_plot$key, levels = c("1-Month Treasury Bills", "5-Year Treasury Notes", "20-Year Treasury Bonds"))

file_path <- paste0(out_path, "/dd_all_bonds_1927_2022.jpeg")
source_string <- str_wrap(paste0("Source: Returns 2.0 (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns are adjusted for inflation."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=pct, fill = key)) +
  geom_area(position = "identity") +
  facet_wrap(. ~ key) +
  scale_fill_manual(values = c("blue", "green", "red"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Drawdowns by Bond Duration")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot decade returns
decades <- raw %>%
  filter(year(date) >= 1930, year(date) < 2020) %>%
  mutate(decade = floor(year(date)/10) * 10) %>%
  group_by(decade) %>%
  summarise(min_date = min(date),
            max_date = max(date)) %>%
  ungroup()

first_values <- raw %>%
  inner_join(decades %>% rename(date = min_date) %>% select(date, decade))

last_values <- raw %>%
  inner_join(decades %>% rename(date = max_date) %>% select(date, decade)) %>%
  rename(last_real_1m = index_real_1m,
         last_real_5yr = index_real_5yr,
         last_real_lt = index_real_lt) %>%
  select(decade, contains("last_"))

to_plot <- first_values %>%
  inner_join(last_values) %>%
  mutate(delta_1m = last_real_1m - index_real_1m,
         delta_5yr = last_real_5yr - index_real_5yr,
         delta_lt = last_real_lt - index_real_lt) %>%
  select(decade, contains("delta_")) %>%
  gather(-decade, key=key, value=value) %>%
  mutate(key = case_when(
    key == "delta_1m" ~ "1-Month Treasury Bills",
    key == "delta_5yr" ~ "5-Year Treasury Notes",
    key == "delta_lt" ~ "20-Year Treasury Bonds",
    TRUE ~ "Error"
  ))

to_plot$key <- factor(to_plot$key, levels = c("1-Month Treasury Bills", "5-Year Treasury Notes", "20-Year Treasury Bonds"))

file_path <- paste0(out_path, "/bond_rets_by_decades_maturities_1930_2019.jpg")

plot <- ggplot(data = to_plot, aes(x=as.factor(decade), y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("blue", "green", "red")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Real Bond Returns\nBy Maturity and Decade")) +
  labs(x = "Decade", y = "Total Change Over Decade",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do summary stats
create_summary_stats <- function(end_date, name){

summary_stats_html <- raw %>%
  select(date, contains("ret_real")) %>%
  filter(date < end_date) %>%
  gather(-date, key=key, value=value) %>%
  mutate(Duration = case_when(
    grepl("1m", key) ~ "1-Month Treasury Bills",
    grepl("5yr", key) ~ "5-Year Treasury Notes",
    grepl("lt", key) ~ "20-Year Treasury Bonds",
    TRUE ~ "Error"
  )) %>%
  group_by(Duration) %>%
  summarise(`Avg. 1-Year Return` = paste0(100*round(mean(value), 4), "%"),
            `Standard Deviation` = paste0(100*round(sd(value), 4), "%")
  )

summary_stats_html$Duration <- factor(summary_stats_html$Duration, levels = c("1-Month Treasury Bills", "5-Year Treasury Notes", "20-Year Treasury Bonds"))

summary_stats_html <- summary_stats_html %>%
                        arrange(Duration)

print(xtable(summary_stats_html), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/bond_duration_summary_stats_", name, ".html"))
}

create_summary_stats("1980-01-01", "pre_1980")
create_summary_stats("2030-01-01", "all")



# ############################  End  ################################## #