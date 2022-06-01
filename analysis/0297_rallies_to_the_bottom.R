cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(readxl)
library(tidyverse)

folder_name <- "0297_rallies_to_the_bottom"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow <- read_excel(paste0(importdir, "0297_dow_daily/Dow daily 2022.xlsx")) %>%
        mutate(date = as.Date(date))

rallies <- read_excel(paste0(importdir, "0297_dow_daily/dow_rallies.xlsx")) %>%
              mutate(start_rally = as.Date(start_rally),
                     end_rally = as.Date(end_rally))

rally_starts <- rallies %>%
                  select(start_rally) %>%
                  rename(date = start_rally) %>%
                  inner_join(dow) %>%
                  rename(start_dow = index_dow,
                         start_rally = date)

rally_ends <- rallies %>%
  select(end_rally) %>%
  rename(date = end_rally) %>%
  inner_join(dow) %>%
  rename(end_dow = index_dow,
         end_rally = date)

rallies_final <- rallies %>%
                  left_join(rally_starts) %>%
                  left_join(rally_ends) %>%
                  mutate(pct_change = end_dow/start_dow - 1,
                    pct_change_label = paste0("+", round(100*(pct_change), 0), "%"))

rallies_summary <- rallies_final %>%
                    summarise(mean_length = mean(rally_length),
                              median_length = quantile(rally_length, probs = 0.5),
                              mean_pct_change = mean(pct_change),
                              median_pct_change = quantile(pct_change, probs = 0.5))

top_dates <- c(as.Date("1929-09-03"), 
                  as.Date("1973-01-11"),
                  as.Date("2000-01-14"),
                  as.Date("2007-10-09"))

bottom_dates <- c(as.Date("1932-07-08"), 
                  as.Date("1974-12-06"),
                  as.Date("2003-03-11"),
                  as.Date("2009-03-09"))

plot_top_to_bottom <- function(top_date, bottom_date){
  
  to_plot <- dow %>%
              filter(date >= top_date, date <= bottom_date)
  
  top_yr <- year(top_date)
  bottom_yr <- year(bottom_date)
  
  tmp_rally_start <- rallies_final %>%
                  filter(bottom_year == bottom_yr) %>%
                  select(start_rally, start_dow) %>%
                  rename(date = start_rally,
                         index_dow = start_dow)
  
  tmp_rally_end <- rallies_final %>%
    filter(bottom_year == bottom_yr) %>%
    select(end_rally, end_dow, pct_change_label) %>%
    rename(date = end_rally,
           index_dow = end_dow)
  
  file_path <- paste0(out_path, "/dow_rallies_", top_yr, "_", bottom_yr, ".jpeg")
  source_string <- paste0("Source: Bloomberg, YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Figures do not include dividends and are not adjusted for inflation."),
                          width = 85)
  
  y_max <- max(to_plot$index_dow)
  y_min <- min(to_plot$index_dow)
  y_unit <- (y_max - y_min)/100
  
  plot <- ggplot(data = to_plot, aes(x=date, y=index_dow)) +
    geom_line() +
    geom_point(data = tmp_rally_start, aes(x=date, y=index_dow), col = "red") +
    geom_point(data = tmp_rally_end, aes(x=date, y=index_dow), col = "green") +
    geom_text(data = tmp_rally_end, aes(x=date, y=index_dow, label = pct_change_label), 
              col = "green",
              nudge_y = y_unit*5) +
    scale_y_continuous(label = comma) +
    of_dollars_and_data_theme +
    scale_x_date(date_labels = "%m/%y") +
    ggtitle(paste0("Dow Rallies to the Bottom\n", top_yr, "-", bottom_yr)) +
    labs(x = "Date", y = "Index Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
}

for(i in 1:length(top_dates)){
  tp <- top_dates[i]
  bt <- bottom_dates[i]
  
  plot_top_to_bottom(tp, bt)
}



# ############################  End  ################################## #