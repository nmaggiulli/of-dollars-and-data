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
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0244_the_new_millionaire"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

import_folder <- "/0244_zillow_moontower/"
upper_limit <- 10 * 10^6

survey_data <- read_excel(paste0(importdir, import_folder, "moontower_nw_survey.xlsx")) %>%
                mutate(networth = ifelse(networth > upper_limit, upper_limit, networth),
                       rich_limit = ifelse(rich_limit > upper_limit, upper_limit, rich_limit))

to_plot <- survey_data

file_path <- paste0(out_path, "/nw_by_age_10m.jpeg")
source_string <- paste0("Source: MoontowerMeta.com")

plot <- ggplot(to_plot, aes(x=age, y = networth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(label = dollar, breaks = seq(0, upper_limit, 10^6)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Net Worth by Age")) +
  labs(x="Age", y="Net Worth",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/rich_limit_by_age.jpeg")

plot <- ggplot(to_plot, aes(x=age, y = rich_limit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(label = dollar, breaks = seq(0, upper_limit, 10^6)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("What is Considered Rich by Age")) +
  labs(x="Age", y="What Do You Consider Rich?",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Rich limit by NW
file_path <- paste0(out_path, "/rich_limit_by_nw.jpeg")

plot <- ggplot(to_plot, aes(x=networth, y = rich_limit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(label = dollar, limits = c(0, upper_limit), breaks = seq(0, upper_limit, 10^6)) +
  scale_y_continuous(label = dollar, limits = c(0, upper_limit), breaks = seq(0, upper_limit, 10^6)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, vjust= 0.5)) +
  ggtitle(paste0("What is Considered Rich by Net Worth")) +
  labs(x="Net Worth", y="What Do You Consider Rich?",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in CPI data
cpi <- readRDS(paste0(localdir, "0021_FRED_cpi.Rds")) %>%
          filter(year>= 2001, year <= 2022) %>%
          select(year, index_cpi)

cpi_latest <- cpi %>%
          mutate(cpi_latest = index_cpi/cpi[nrow(cpi), "index_cpi"]) %>%
          select(year, cpi_latest)

zillow_middle <- read.csv(paste0(importdir, import_folder, "Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_raw_mon.csv")) %>%
                      rename(region_name = RegionName) %>%
                      select(contains("region_"), contains("X")) %>%
                      gather(-region_name, key=key, value=value) %>%
                      mutate(date = as.Date(key, format = "X%Y.%m.%d")) %>%
                      select(-key) %>%
                      filter(date >= "2000-01-01")

earliest_value <- zillow_middle %>%
                    filter(date == "2000-01-31") %>%
                    rename(first_value = value) %>%
                    select(-date) %>%
                    drop_na()

zillow_change <- zillow_middle %>%
            inner_join(earliest_value) %>%
            mutate(value = value/first_value * 10^6) %>%
            select(date, region_name, value) %>%
            drop_na()

to_plot <- zillow_change %>%
            filter(region_name %in% c("United States", "Los Angeles-Long Beach-Anaheim, CA", "Flint, MI", "Overall Inflation")) %>%
            mutate(region_name = case_when(
              region_name == "Los Angeles-Long Beach-Anaheim, CA" ~ "Los Angeles, CA",
              TRUE ~ region_name
            ))

file_path <- paste0(out_path, "/change_in_home_value_2000_2021.jpeg")
source_string <- paste0("Source: Zillow")

plot <- ggplot(to_plot, aes(x=date, y = value, col = region_name)) +
  geom_line() +
  scale_color_manual(values = c("gray", "red", "black"),
                     guide_legend(nrow = 2)) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Nominal Change in U.S. Home Prices\nSince 2000")) +
  labs(x="Date", y="Growth of $1M",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in SCF data
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(year >= 2001) %>%
  select(year, wgt, networth) %>%
  left_join(cpi_latest) %>%
  mutate(networth_nominal = networth * cpi_latest)

to_plot <- scf_stack %>%
  group_by(year) %>%
  summarise(
    `93rd Percentile` = wtd.quantile(networth_nominal, weights = wgt, probs= 0.93)
  ) %>%
  ungroup() %>%
  gather(-year, key=key, value=value) 

file_path <- paste0(out_path, "/nw_93_over_time.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances")

plot <- ggplot(to_plot, aes(x= year, y = value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("black")) +
  scale_y_continuous(label = dollar, breaks = seq(0, 2.8*10^6, 2*10^5)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("93rd Percentile of Net Worth over Time")) +
  labs(x="Year", y="Net Worth (Nominal Dollars)",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #