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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0166_why_we_invest_in_bonds"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0166_ycharts_spxtr_vustx/SPX_MVUSTX_data.csv"),
                col.names = c("date","index_sp500", "index_bond")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

first_year <- year(min(raw$date))
last_year <- year(max(raw$date))

plot_stock_vs_bonds <- function(n_days){
  to_plot <- raw %>%
              mutate(ret_sp500 = index_sp500/lag(index_sp500, n_days) - 1,
                     ret_bond = index_bond/lag(index_bond, n_days) - 1) %>%
              drop_na() %>%
              filter(ret_sp500 < 0)
  
  n_days_string <- str_pad(n_days, width = 3, side = "left", pad = "0")
  
  file_path <- paste0(out_path, "/bonds_perf_when_stocks_down_", n_days_string, ".jpeg")
  source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Stocks are represented by the S&P 500 Total return while ",
                                 "bonds are represented by the Vanguard Long-Term Treasury Fund (VUSTX)."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=ret_sp500, y=ret_bond)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_continuous(label = percent, limits = c(-0.5, 0)) +
    scale_y_continuous(label = percent, limits = c(-0.1, 0.4)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("How Bonds Peform When Stocks Fall\nOver ", n_days, " Sessions")) +
    labs(x = paste0("Stock Return"), 
         y = paste0("Bond Return"),
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

all_days <- c(5, 10, 20, 60, 120, 250, 500)

for(d in all_days){
  plot_stock_vs_bonds(d)
}


# ############################  End  ################################## #