cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(zoo)
library(tidyverse)

folder_name <- "0104_global_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_global <- function(start_date, end_date){
  
  start_date_string <- str_replace_all(start_date, "-", "_")
  end_date_string <- str_replace_all(end_date, "-", "_")
  
  gr <- read.csv(paste0(importdir, "/0104_world_indices_total_return/world_tr_data_ycharts.csv")) %>%
        clean_cols() %>%
        mutate(date = as.Date(gsub("(.*)\\s.*", "\\1", period), format = "%Y-%m-%d")) %>%
        filter(date >= start_date, date <= end_date) %>%
        select(-period) %>%
        arrange(date)
  
  countries <- data.frame(country = c("China", "Germany", "Greece", "Italy", "Japan",
                 "Spain", "UK", "US")
                )
          
  colnames(gr) <- c(countries$country, "date")
  
  gr_long <- gr %>%
              gather(key=key, value=value, -date)
  
  first_date <- gr_long %>%
            filter(!is.na(value)) %>%
            group_by(date) %>%
            summarize(n_countries = n()) %>%
            filter(n_countries == (ncol(gr) - 1)) %>%
            head(1)
  
  gr_1 <- gr_long %>%
            filter(date== pull(first_date[1, "date"])) %>%
            rename(first_index = value) %>%
            select(key, first_index)
  
  gr_no_na <- gr %>%
                filter(date >= pull(first_date[1, "date"]))
  
  # Fill in NAs by date
  for (i in 1:(ncol(gr_no_na)-1)){
    gr_no_na[, i] <- na.locf(gr_no_na[, i])
  }
  
  to_plot <- gr_no_na %>%
                gather(key = key, value = value, -date) %>%
                mutate(yr = year(date),
                       mt = month(date)) %>%
                group_by(yr, mt, key) %>%
                summarize(value = mean(value)) %>%
                ungroup() %>%
                mutate(date = as.Date(paste0(yr,"-", mt, "-01"))) %>%
                left_join(gr_1) %>%
                mutate(value = value/first_index) %>%
                select(date, key, value)
  
  last_month <- filter(to_plot, date == max(to_plot$date)) 
  
  # Plot Growth of $1
  # Set note and source string
  source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Total return index values are averaged monthly and are not adjusted for inflation or currency effects."), 
                            width = 85)
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/growth_dollar_global_market_",
                start_date_string, "_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_color_discrete(guide = FALSE) +
    geom_text_repel(data=last_month, aes(x=date, y=value, col=key),
                    label = paste0(last_month$key, " $", round(last_month$value, 2)),
                    max.iter = 1000) +
    of_dollars_and_data_theme +
    labs(x="Date", y= "Growth of $1",
         caption = paste0(source_string, "\n", note_string)) +
    ggtitle(paste0("Global Market Growth of $1\n", year(start_date), "-", year(end_date)))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Create dd datasets
  for (i in 1:nrow(countries)){
    country <- countries[i, "country"]
    pre_dd <- filter(to_plot, key == country) %>%
                select(date, value)
    dd <- drawdown_path(pre_dd) %>%
            mutate(key = country)
    
    if (i == 1){
      final_dd <- dd
    } else{
      final_dd <- bind_rows(final_dd, dd)
    }
  }
  
  last_month <- filter(final_dd, date == max(final_dd$date)) 
  
  # Plot drawdowns
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/dd_global_market_", start_date_string, "_", end_date_string, ".jpeg")
  
  plot <- ggplot(final_dd, aes(x=date, y=pct, col = key)) +
    geom_line() +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(label = percent) +
    geom_text_repel(data=last_month, aes(x=date, y=pct, col=key),
                    label = paste0(last_month$key),
                    max.iter = 1000) +
    of_dollars_and_data_theme +
    labs(x="Date", y="Percentage Decline from All-Time High",
         caption = paste0(source_string, "\n", note_string)) +
    ggtitle(paste0("Global Market Drawdowns\n", year(start_date), "-", year(end_date)))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_global("1995-02-01", "2018-12-31")
plot_global("2009-01-01", "2018-12-31")

# ############################  End  ################################## #
