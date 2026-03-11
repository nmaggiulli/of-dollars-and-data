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
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "0535_end_of_an_era"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

blog_data <- read_excel(paste0(importdir, "/0535_end_of_an_era/blog_data.xlsx"))

to_plot <- blog_data

file_path <- paste0(out_path, "/odad_pageviews_2017_2026.jpeg")
source_string <- paste0("Source: Google Analytics (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x = year, y = pageviews)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = paste0(round(pageviews / 1e6, 1), "M")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Of Dollars And Data Pageviews by Year")) +
  labs(x = "Year", y = "Total Pageviews",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Sales now
file_path <- paste0(out_path, "/odad_book_sales_2017_2026.jpeg")
source_string <- paste0("Source: Harriman House, Penguin Random House (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes actual sales of Just Keep Buying and The Wealth Ladder through 2025, with conservative sales estimate for 2026."),
                        width = 75)

plot <- ggplot(to_plot, aes(x = year, y = book_sales)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = ifelse(book_sales == 0, "0", comma(round(book_sales, 0)))),
            vjust = -0.5, size = 3) +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Book Sales by Year")) +
  labs(x = "Year", y = "Total Sales",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do relative ebit
file_path <- paste0(out_path, "/odad_ebit_2017_2026.jpeg")
source_string <- paste0("Source: Of Dollars And Data LLC (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures are indexed to 1 in 2020 (my first year of revenue)."),
                        width = 75)

plot <- ggplot(to_plot, aes(x = year, y = annual_ebit)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = ifelse(annual_ebit == 0, "0", paste0(round(annual_ebit, 1), "x"))),
            vjust = -0.5, size = 3) +
  scale_y_continuous(label = comma, limits = c(0, 10), breaks = seq(1, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Of Dollars And Data Annual EBIT\nRelative to 2020")) +
  labs(x = "Year", y = "Annual EBIT",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #