cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0329_nw_change"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

nw_changes <- read_excel(paste0(importdir, "0329_nw_change/nm_nw_changes.xlsx"))

to_plot <- nw_changes

file_path <- paste0(out_path, "/nm_nw_changes_by_year.jpg")
source_string <- paste0("Source: My life (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x=year, y=nw_change)) +
  geom_bar(stat = "identity", fill = "purple") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), max(to_plot$year), 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Net Worth Percentage Change by Year")) +
  labs(x = "Year", y = "Percentage Change from Prior Year",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# New plot 
to_plot <- nw_changes %>%
            gather(-year, key=key, value = value) %>%
            mutate(key = case_when(
              key == "nw_change" ~ "Total Change",
              key == "savings_change" ~ "Change from Savings",
              key == "investment_change" ~ "Change from Investments",
              TRUE ~ "Error"
            ))

file_path <- paste0(out_path, "/nm_nw_changes_with_attribution.jpg")
note_string <- str_wrap(paste0("Note: Investment change is assumed to be the annual return in an 80/20 portfolio. ",
                               width = 80))

plot <- ggplot(data = to_plot, aes(x=year, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "blue", "purple")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), max(to_plot$year), 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Net Worth Percentage Change by Year\nWith Attribution")) +
  labs(x = "Year", y = "Percentage Change from Prior Year",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Stacked bar plot
to_plot <- nw_changes %>%
  select(-nw_change) %>%
  gather(-year, key=key, value = value) %>%
  mutate(key = case_when(
    key == "savings_change" ~ "Change from Savings",
    key == "investment_change" ~ "Change from Investments",
    TRUE ~ "Error"
  ))

file_path <- paste0(out_path, "/nm_nw_changes_with_attribution_stacked.jpg")
note_string <- str_wrap(paste0("Note: Investment change is assumed to be the annual return in an 80/20 portfolio. ",
                               width = 80))

plot <- ggplot(data = to_plot, aes(x=year, y=value, fill = key)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), max(to_plot$year), 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Attribution of Net Worth Change by Year")) +
  labs(x = "Year", y = "Percentage Change from Prior Year",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #