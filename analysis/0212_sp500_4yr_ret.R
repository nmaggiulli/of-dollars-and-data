cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0212_sp500_4yr_ret"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

lookback_months <- 48
lookback_years <- lookback_months/12

#Bring in raw data
raw <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
  select(date, price_plus_div) %>%
  mutate(ret_sp500_lookback = price_plus_div/lag(price_plus_div, lookback_months) - 1) %>%
  filter(month(date) == 1)

election_years <- seq(1920, 2020, 4)

plot_dist <- function(base_year){
  
  to_plot <- raw %>%
                filter(year(date) %in% election_years, year(date) >= base_year)
  
  min_year <- year(min(to_plot$date))
  max_year <- year(max(to_plot$date))
  
  pct_pos <- nrow(to_plot %>% filter(ret_sp500_lookback > 0))/nrow(to_plot)
  pct_double <- nrow(to_plot %>% filter(ret_sp500_lookback >= 1))/nrow(to_plot)
  
  file_path <- paste0(out_path, "/sp500_lookback_", lookback_months, "m_", base_year, ".jpeg")
  source_string <- paste0("Source: Shiller data, ", min_year, "-", max_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Returns include reinvested dividends and are adjusted for inflation.  ",
                                 "Four years after a new presidential term starts, the stock market is higher ",
                                 round(100*pct_pos, 1), "% of the time and has doubled ",
                                 round(100*pct_double, 1), "% of the time."), width = 85)
  
  plot <- ggplot(to_plot, aes(x = ret_sp500_lookback)) +
    geom_density(fill = chart_standard_color) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("Distribution of ", lookback_years, "-Year Total Returns\nFor U.S. Stocks")) +
    labs(x = "Return" , y = paste0("Frequency"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_dist(1920)
plot_dist(1948)


# ############################  End  ################################## #