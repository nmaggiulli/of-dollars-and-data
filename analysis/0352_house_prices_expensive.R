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
library(tidyverse)

folder_name <- "0352_house_prices_expensive"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

date_string <- date_to_string(Sys.Date())

shiller_housing <- read_excel(paste0(importdir, "0352_shiller_hpi_data/shiller_house_data_2024_09_19.xls"),
                              sheet = "Data",
                              skip = 6) %>%
                    select(1, 2) 

colnames(shiller_housing) <- c("date", "real_housing_index")

to_plot <- shiller_housing

file_path <- paste0(out_path, "/shiller_hpi_real_", date_string, ".jpeg")
source_string <- str_wrap(paste0("Source: Shiller HPI data (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Index value is adjusted for inflation."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=date, y=real_housing_index)) +
  geom_line() +
  scale_y_continuous(label =  comma, breaks = seq(0, 225, 25)) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real U.S. Housing Index Since 1890")) +
  labs(x="Year", y="Real Housing Index",
      caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #