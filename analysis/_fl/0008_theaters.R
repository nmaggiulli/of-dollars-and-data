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

folder_name <- "/_fl/0008_theaters"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/_fl/0008_theaters/movie_data.xlsx"),
                  sheet = "raw") %>%
        filter(year < 2020) %>%
        mutate(tickets_sold = tickets_sold/1000000,
               real_box_office = real_box_office/1000000)

to_plot <- raw

file_path <- paste0(out_path, "/tickets_sold.jpeg")
source_string <- paste0("Source:  The Numbers")

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = tickets_sold)) +
  geom_line(col = chart_standard_color) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(limits = c(1995, 2021)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Movie Tickets Sold in U.S.")) +
  labs(x = "Year" , y = "Tickets Sold\n(in millions)",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/real_box_office.jpeg")
source_string <- paste0("Source:  The Numbers")

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = real_box_office)) +
  geom_line(col = chart_standard_color) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(limits = c(1995, 2021)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Inflation-Adjusted Box Office Sales")) +
  labs(x = "Year" , y = "Inflation-Adjusted Sales\n(in millions)",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/total_releases.jpeg")
source_string <- paste0("Source:  The Numbers")

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = total_releases)) +
  geom_line(col = chart_standard_color) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(limits = c(1995, 2021)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Total U.S. Film Releases")) +
  labs(x = "Year" , y = "Total Releases",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")








# ############################  End  ################################## #