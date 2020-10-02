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

folder_name <- "/_fl/0008_ch11_bankruptcies"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_months_sma <- 6
raw <- read_excel(paste0(importdir, "/_fl/0008_ch11_bankruptcy/bankruptcy_data.xlsx"),
                  sheet = "raw") %>%
        mutate(ch11_sma = rollmean(ch11_bankruptcies, n_months_sma, fill = NA, align = "right"))

to_plot <- raw

file_path <- paste0(out_path, "/ch11_by_month.jpeg")
source_string <- paste0("Source:  American Bankruptcy Institute (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = ch11_bankruptcies)) +
  geom_bar(stat = "identity", fill = chart_standard_color, col = chart_standard_color) +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Ch.11 Bankruptcies Are Up Since Covid\nBut Don't Match the GFC")) +
  labs(x = "Date" , y = "Number of Monthly Filings",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/ch11_ma_by_month.jpeg")
source_string <- paste0("Source:  American Bankruptcy Institute (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = ch11_sma)) +
  geom_line(col = chart_standard_color) +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0(n_months_sma, "-Month Moving Average of Bankruptcy Filings")) +
  labs(x = "Date" , y = "Number of Monthly Filings",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")








# ############################  End  ################################## #