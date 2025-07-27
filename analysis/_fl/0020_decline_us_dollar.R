cat("\014") # Clear your console
rm(list = ls()) #clear your enviro01ent

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(readxl)
library(pdftools)
library(magrittr)
library(quantmod)
library(FinCal)
library(ggrepel)

folder_name <- "_fl/0020_decline_us_dollar"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/", folder_name, "/cpi_since_1913.xlsx"))

first_value <- pull(raw[1, "index"])

to_plot <- raw %>%
        mutate(value_of_dollar = first_value/index) %>%
        select(year, value_of_dollar)

file_path <- paste0(out_path, "/decline_us_dollar_1913_2025.jpeg")
source_string <- paste0("Source:  Federal Reserve Bank of Minneapolis (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = value_of_dollar)) +
  geom_line(col = "black") +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Value of the U.S. Dollar Over Time")) +
  labs(x = "Year" , y = "Value",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #