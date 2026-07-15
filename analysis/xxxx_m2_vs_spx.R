cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(quadprog)
library(lubridate)
library(fTrading)
library(tidyverse)

folder_name <- "xxxx_m2_vs_spx"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

m2 <- read.csv(paste0(importdir, "M2SL.csv")) %>%
          mutate(date = as.Date(observation_date),
                 m2 = M2SL) %>%
          select(date, m2)

df <- sp500_ret_pe %>%
          left_join(m2) %>%
          filter(date >= "2008-01-01") %>%
          select(date, price_plus_div, m2) %>%
          arrange(date)

first_m2 <- pull(df[1, "m2"])
first_spx <- pull(df[1, "price_plus_div"])

to_plot <- df %>%
              mutate(SPX = price_plus_div/first_spx - 1,
                     M2 = m2/first_m2 - 1) %>%
              select(date, M2, SPX) %>%
              gather(-date, key=key, value = value)
              
file_path <- paste0(out_path, "/m2_vs_spx_2008_2026.jpeg")
source_string <- paste0("Source: Shiller data, FED data, 2008-2026 (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note: S&P 500 performance includes reinvested dividends and is adjusted for inflation."),
                          width = 85)

plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 vs. M2 Money Supply")) +
  labs(x = "Year", y = "Percentage Change (since 2008)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")
