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

folder_name <- "0328_sp500_year_losses"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

down_pct <- -0.15

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(year(date) >= 1900) %>%
                    select(date, price_plus_div) %>%
                    mutate(ret_1yr = lead(price_plus_div, 12)/price_plus_div - 1)

year_rets <- sp500_ret_pe %>%
              filter(month(date) == 1) %>%
              drop_na() %>%
              mutate(decade = floor(year(date)/10) * 10,
                     pos_ret = ifelse(ret_1yr > 0, 1, 0))

overall_pos <- mean(year_rets$pos_ret)

to_plot <- year_rets %>%
                filter(decade < 2020) %>%
                group_by(decade) %>%
                summarise(pos_ret = mean(pos_ret)) %>%
                ungroup()

file_path <- paste0(out_path, "/decade_prob_pos_return.jpg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation. Shows calendar year returns only."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=as.factor(decade), y=pos_ret)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Percentage of Years with Positive Returns\nBy Decade for U.S. Stocks")) +
  labs(x = "Decade", y = "Percentage of Years",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

down_pct_label <- paste0(-100*down_pct, "%")

#Now do same for all years after 10% losses
to_plot <- year_rets %>%
                      filter(lag(ret_1yr) < down_pct) %>%
              mutate(category = paste0("After a Big Down Year (>", down_pct_label,")")) %>%
          select(category, ret_1yr) %>%
          bind_rows(year_rets %>% filter(lag(ret_1yr) >= -0.1) %>% mutate(category = "All Other Years") %>% select(category, ret_1yr))

to_plot$category <- factor(to_plot$category, levels = c("All Other Years", paste0("After a Big Down Year (>", down_pct_label,")")))

min_year <- min(year(year_rets$date))
max_year <- max(year(year_rets$date))

file_path <- paste0(out_path, "/dist_rets_after_neg_", -100*down_pct,"pct_year.jpg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation. Shows calendar year returns only. ",
                               "A big down year is defined as any calendar year where the market drops more than ", down_pct_label, "."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=ret_1yr, fill = category)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "black") +
  scale_fill_manual(values = c(chart_standard_color, "red")) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Annual Return of U.S. Stocks\nAfter a Big Down Year and For All Other Years\n", min_year, "-", max_year)) +
  labs(x = "Annual Return", y = "Frequency",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do all annual returns ranked
to_plot <- year_rets%>%
              mutate(year = year(date)) %>%
              select(year, ret_1yr) %>%
              bind_rows(data.frame(year = 2022, 
                                   ret_1yr = -0.18)) %>%
              mutate(yr_2022 = ifelse(year == 2022, 1, 0))

min_year <- min(to_plot$year)
max_year <- max(to_plot$year)

file_path <- paste0(out_path, "/all_calendar_year_rets_sp500.jpg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation. ",
                               "Assumes that 2022 (red) has a calendar year return of -18%."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=reorder(year, -ret_1yr), y = ret_1yr, fill = as.factor(yr_2022))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("black", "red"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(paste0("Calendar Year Returns of U.S. Stocks\n", min_year, "-", max_year,"*")) +
  labs(x = "All Years", y = "Annual Return (%)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Test yield curve inversions
raw_10y_2y <- read_excel(paste0(importdir, "0328_sp500_year_losses/T10Y2Y.xls"),
                         skip = 10) %>%
  mutate(date = as.Date(observation_date),
         month_date = as.Date(paste0(year(date), "-", month(date), "-01")),
         rate_10y_2y = T10Y2Y) %>%
  select(date, month_date, rate_10y_2y)

min_date <- min(raw_10y_2y$month_date)

rate_10y_2y <- raw_10y_2y %>%
  group_by(month_date) %>%
  summarise(rate_10y_2y = min(rate_10y_2y)) %>%
  ungroup()

for(i in 1:nrow(rate_10y_2y)){
  if(i == 1){
    rate_10y_2y[i, "inversion_counter"] <- 1
  } else{
    if(rate_10y_2y[i, "rate_10y_2y"] < 0){
      rate_10y_2y[i, "inversion_counter"] <- rate_10y_2y[(i-1), "inversion_counter"]
    } else{
      rate_10y_2y[i, "inversion_counter"] <- rate_10y_2y[(i-1), "inversion_counter"] + 1
    }
  }
}

start_inversions <- rate_10y_2y %>%
  filter(rate_10y_2y < 0, year(month_date) < 2022) %>%
  group_by(inversion_counter) %>%
  summarise(start_date = min(month_date),
            end_date = max(month_date),
    inversion_start_year = year(min(month_date))) %>%
    ungroup()

all_inversion_start_years <- start_inversions %>%
          select(inversion_start_year) %>%
          distinct() %>%
          mutate(date = as.Date(paste0(inversion_start_year, "-01-01"))) 

to_plot <- sp500_ret_pe %>%
              filter(month(date) == 1) %>%
              drop_na() %>% 
              mutate(lead_ret_1yr = lead(ret_1yr)) %>%
              filter(date >= "1978-01-01") %>%
              left_join(all_inversion_start_years) %>%
              mutate(category = ifelse(is.na(inversion_start_year), 
                                       "All Other Years",
                                       "Year After Inversion"))

to_plot$category <- factor(to_plot$category, levels = c("All Other Years", "Year After Inversion"))

min_year <- min(year(to_plot$date))
max_year <- max(year(to_plot$date))

file_path <- paste0(out_path, "/dist_rets_after_yc_inversion.jpg")
source_string <- paste0("Source: Shiller data, FRED (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation. Shows the distribution of ",
                               "returns in the calendar year after an inversion against the distribution of returns in all other calendar years. ",
                               "Data starts in 1978 with the first recorded yield curve inversion."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=lead_ret_1yr, fill = category)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "black") +
  scale_fill_manual(values = c(chart_standard_color, "red")) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Annual Return of U.S. Stocks\nAfter a Yield Curve Inversion & For All Other Years\n", min_year, "-", max_year)) +
  labs(x = "Annual Return", y = "Frequency",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do annual returns in a bar plot
to_plot <- to_plot %>%
              mutate(year = year(date),
                     category = ifelse(is.na(lag(inversion_start_year)), 
                                             "All Other Years",
                                             "Year After Inversion"))

to_plot$category <- factor(to_plot$category, levels = c("All Other Years", "Year After Inversion"))

file_path <- paste0(out_path, "/ann_rets_after_yc_inversion.jpg")
source_string <- paste0("Source: Shiller data, FRED (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation. ",
                               "Data starts in 1978 with the first recorded yield curve inversion."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=year, y=ret_1yr, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(chart_standard_color, "red")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Annual Returns of U.S. Stocks\nAfter Yield Curve Inversions")) +
  labs(x = "Year", y = "Annual Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #