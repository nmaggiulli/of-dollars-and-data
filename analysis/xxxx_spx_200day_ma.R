cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(jsonlite)
library(ggrepel)
library(slackr)
library(zoo)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "xxxx_spx_200day_ma"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "xxxx_spx_ycharts/SPX_data.csv"),
                col.names = c("date", "index_spx")) %>%
          mutate(date = as.Date(date)) %>%
          arrange(date)

df <- raw %>%
        filter(date <= "2024-12-31") %>%
        mutate(ma_200day = rollmean(x = index_spx, 200, align = "right", fill = NA),
               in_out = case_when(
                 index_spx < ma_200day ~ 0,
                 TRUE ~ 1
               ),
               in_out_status = case_when(
                 in_out == 1 & lag(in_out, 1) == 1 ~ "Stay Invested",
                 in_out == 0 & lag(in_out, 1) == 0 ~ "Stay Out",
                 in_out == 1 & lag(in_out, 1) == 0 ~ "Enter",
                 in_out == 0 & lag(in_out, 1) == 1 ~ "Exit",
                 TRUE ~ NA
               ),
               ret_fwd_250 = lead(index_spx, 250)/index_spx - 1,
               ret_bh = case_when(
                 row_number() == 1 ~ 1,
                 TRUE ~ index_spx/lag(index_spx, 1)
               ),
               ret_ma = case_when(
                 row_number() == 1 ~ 1,
                 in_out_status %in% c("Stay Out", "Enter") ~ 1,
                 TRUE ~ index_spx/lag(index_spx, 1)
               ),
               port_bh = cumprod(ret_bh),
               port_ma = cumprod(ret_ma),  
               ) %>%
              filter(!is.na(in_out_status), !is.na(ret_fwd_250))

summary_by_status <- df %>%
            group_by(in_out_status) %>%
            summarise(n_days = n(),
                      pct25_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.25),
                      median_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.5),
                      pct75_ret_fwd_1yr = quantile(ret_fwd_250, probs = 0.75)) %>%
            ungroup()

to_plot <- df %>%
            select(date, port_bh, port_ma) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/spx_200day_ma_vs_bh.jpeg")
source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x= date, y = value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("black", "green"), guide = "none") +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1\nBuy & Hold vs. 200-Day MA")) +
  labs(x="Year", y="Portfolio Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #
