cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyverse)

folder_name <- "0366_us_stock_real_drawdowns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    select(date, price_plus_div) %>%
                    filter(date >= "1926-01-01")

dd <- drawdown_path(sp500_ret_pe)

dd_counter <- 1
for(i in 1:nrow(dd)){
  if(i == 1){
    dd[i, "dd_counter"] <- dd_counter
    dd[i, "dd_month"] <- 0
  } else{
    if(dd[i, "pct"] == 0){
      dd_counter <- dd_counter + 1
      dd[i, "dd_counter"] <- dd_counter
      dd[i, "dd_month"] <- 0
    } else{
      dd[i, "dd_counter"] <- dd_counter
      dd[i, "dd_month"] <- dd[(i-1), "dd_month"] + 1
    }
  }
}

dd_lengths <- dd %>%
                group_by(dd_counter) %>%
                summarise(dd_start = min(date),
                          dd_end = max(date),
                  n_months = n(),
                          max_dd = min(pct)) %>%
                ungroup()

worst_10_dd<- dd_lengths %>%
                  arrange(desc(n_months)) %>%
                  head(10) %>%
                  inner_join(dd)

plot_dd_comparison<- function(n_months){
  
  n_years <- n_months/12
  
  n_month_filter <- as.numeric(n_months)

  to_plot <- worst_10_dd %>%
                filter(dd_month < n_month_filter) %>%
                mutate(dd_year = year(dd_start),
                       flag_2021 = case_when(
                         dd_year == 2021 ~ 1,
                         TRUE ~ 0
                       )) %>%
                select(dd_month, pct, dd_year, flag_2021)
  
  file_path <- paste0(out_path, "/us_stock_worst_real_drawdowns_", n_months, "_months.jpeg")
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  
  plot <- ggplot(data = to_plot, aes(x = dd_month, y = pct, col = as.factor(dd_year), size = as.factor(flag_2021))) +
    geom_line() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(0, n_months, 12), limits = c(0, n_months)) +
    scale_size_manual(values = c(0.5, 2), guide = "none") +
    ggtitle(paste0("Top 10 U.S. Stock Real Drawdowns\nOver First ", n_years, " Years")) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    of_dollars_and_data_theme +
    labs(x = "Month" , y = "Percentage Decline",
         caption = paste0(source_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
}

plot_dd_comparison(36)
plot_dd_comparison(13*12)

# ############################  End  ################################## #