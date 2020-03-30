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
library(readxl)
library(tidyverse)

folder_name <- "0174_was_that_the_bottom"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index")) %>%
        mutate(date = as.Date(date)) 


plot_dow <- function(start_date, end_date){
  
  first_value <- dow %>%
                    filter(date == start_date) %>%
                    pull(index)
  
  to_plot <- dow %>%
              filter(date >= start_date, date <= end_date) %>%
              mutate(pct = index/first_value - 1)
  
  # Set note and source string
  source_string <- str_wrap("Source: Bloomberg (OfDollarsAndData.com)",
                            width = 85)
  note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends."),
                          width = 85)
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  file_path <- paste0(out_path, "/dow_price_", 
                      start_date_string, 
                      "_",
                      end_date_string,
                      ".jpeg")
  
  print(tail(to_plot))
  
  plot <- ggplot(to_plot, aes(x=date, y=pct)) +
    geom_line() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_x_date(date_labels = "%m/%y") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow After the 1932 Bottom")) +
    labs(x = "Date" , y = "Percentage Off Lows",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_dow("1932-07-08", "1932-09-01")
plot_dow("1932-07-08", "1933-03-03")

# ############################  End  ################################## #