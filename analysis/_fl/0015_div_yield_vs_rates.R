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
library(zoo)
library(tidyverse)

folder_name <- "_fl/0015_div_yield_vs_rates"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            mutate(div_yield = real_div/real_price) %>%
            select(date, div_yield, real_price)

raw_hikes <- read_excel(paste0(importdir, "/", folder_name, "/Fed Funds Moves.xlsx"))

raw_hikes$year <- na.locf(raw_hikes$year)

fed_rates <- raw_hikes %>%
                    mutate(date = as.Date(paste0(year, "-", month(day), "-01"))) %>%
                    select(date, rate)

df <- shiller %>%
        left_join(fed_rates) %>%
        filter(date >= min(fed_rates$date)) %>%
        mutate(hike_cycle = case_when(
          date >= "1971-01-01" & date <= "1974-07-01" ~ "1971 hike",
          date >= "1976-11-01" & date <= "1981-05-01" ~ "1976 hike",
          date >= "1992-08-01" & date <= "1995-02-01" ~ "1992 hike",
          date >= "1998-11-01" & date <= "2000-05-01" ~ "1998 hike",
          date >= "2004-06-01" & date <= "2006-06-01" ~ "2004 hike",
          date >= "2022-03-01" & date <= "2023-03-01" ~ "2022 hike",
          TRUE ~ "N/A"
        ))

df$rate <- na.locf(df$rate)

all_cycles <- df %>%
              filter(hike_cycle != "N/A") %>%
              group_by(hike_cycle) %>%
              summarise(min_date = min(date),
                     max_date = max(date)) %>%
              ungroup()

start_cycle <- df %>%
              inner_join(all_cycles %>% rename(date = min_date)) %>%
              rename(start_rate = rate,
                     start_price = real_price,
                     start_yield = div_yield) %>%
              select(hike_cycle, start_rate, start_price, start_yield)

end_cycle <- df %>%
              inner_join(all_cycles %>% rename(date = max_date)) %>%
              rename(end_rate = rate,
                     end_price = real_price,
                     end_yield = div_yield) %>%
              select(hike_cycle, end_rate, end_price, end_yield)

summary <- start_cycle %>%
                left_join(end_cycle) %>%
                mutate(change_rate = end_rate - start_rate,
                       change_yield = end_yield - start_yield,
                       change_price = end_price/start_price - 1)

to_plot <- df %>%
            filter(hike_cycle != "N/A")

file_path <- paste0(out_path, "/div_yield_vs_rates.jpeg")
source_string <- str_wrap(paste0("Source: Shiller data (OfDollarsAndData.com)"),
                          width = 85)

plot <- ggplot(to_plot, aes(x=rate, y=div_yield, col = hike_cycle)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("U.S. Stock Dividend Yield vs. Fed Target Rate\n1971-2023")) +
  labs(x = "Fed Target Rate" , y = "Dividend Yield",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #