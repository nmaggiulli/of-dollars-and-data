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
library(zoo)
library(purrr)
library(ggrepel)

folder_name <- "_fl/0022_mstw_spxtr"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/", folder_name, "/MSTW_SPXTR_data_2000_2025_10_13.csv"),
                col.names = c("date", "index_msci_taiwan", "index_spx")) %>%
        mutate(date = as.Date(date)) %>%
        arrange(date) %>%
        rename(`MSCI Taiwan` = index_msci_taiwan,
               `S&P 500` = index_spx)

first_mstw <- raw[1, "MSCI Taiwan"]
first_spx <- raw[1, "S&P 500"]

to_plot <- raw %>%
            gather(-date, key=key, value=value) %>%
            mutate(growth_of_dollar =  case_when(
              key == "MSCI Taiwan" ~ value/first_mstw,
              TRUE ~ value/first_spx
            )) %>%
          select(-value)

max_date <- max(to_plot$date)
to_plot$growth_of_dollar <- na.locf(to_plot$growth_of_dollar)

file_path <- paste0(out_path, "/growth_of_dollar_msci_taiwan_vs_sp500.jpeg")

text_labels <- data.frame()

max_msci_taiwan <- to_plot %>% filter(date == max_date, key == "MSCI Taiwan") %>% pull(growth_of_dollar)
text_labels[1, "date"] <- as.Date("2024-01-01")
text_labels[1, "growth_of_dollar"] <- 2
text_labels[1, "key"] <- "MSCI Taiwan"
text_labels[1, "label"] <- paste0("MSCI Taiwan\nNT", format_as_dollar(max_msci_taiwan, 2))

max_spx <- to_plot %>% filter(date == max_date, key == "S&P 500") %>% pull(growth_of_dollar)
text_labels[2, "date"] <- as.Date("2022-01-01")
text_labels[2, "growth_of_dollar"] <- 6
text_labels[2, "key"] <- "S&P 500"
text_labels[2, "label"] <- paste0("S&P 500\nNT", format_as_dollar(max_spx, 2))

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = growth_of_dollar, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=growth_of_dollar, label = label, col = key),
            family = "my_font") +
  scale_color_manual(guide = "none", values = c("black", "blue")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of NT$1\nMSCI Taiwan vs. S&P 500\n2000-2025")) +
  labs(x = "Year" , y = "Growth of NT$1")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do DCA analysis
to_plot <- to_plot %>%
  group_by(key) %>%
  # if growth_of_dollar is a cumulative index, ret should be simple return:
  mutate(ret = (growth_of_dollar / lag(growth_of_dollar)) - 1,
         ret = coalesce(ret, 0)) %>%
  mutate(
    dca = {
      out <- accumulate(ret, ~ (.x * (1 + .y)) + 100, .init = 0) |> tail(-1)
      out[1] <- 100
      out
    }
  ) %>%
  ungroup()

# Now plot it
file_path <- paste0(out_path, "/dca_msci_taiwan_vs_sp500.jpeg")

text_labels <- data.frame()

max_msci_taiwan <- to_plot %>% filter(date == max_date, key == "MSCI Taiwan") %>% pull(dca)
text_labels[1, "date"] <- as.Date("2022-01-01")
text_labels[1, "dca"] <- 0.9*max_msci_taiwan
text_labels[1, "key"] <- "MSCI Taiwan"
text_labels[1, "label"] <- paste0("MSCI Taiwan\nNT", format_as_dollar(max_msci_taiwan, 0))

max_spx <- to_plot %>% filter(date == max_date, key == "S&P 500") %>% pull(dca)
text_labels[2, "date"] <- as.Date("2023-01-01")
text_labels[2, "dca"] <- 0.2*max_spx
text_labels[2, "key"] <- "S&P 500"
text_labels[2, "label"] <- paste0("S&P 500\nNT", format_as_dollar(max_spx, 0))

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = dca, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=dca, label = label, col = key),
            family = "my_font") +
  scale_color_manual(guide = "none", values = c("black", "blue")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of NT$100 Per Day\nMSCI Taiwan vs. S&P 500\n2000-2025")) +
  labs(x = "Year" , y = "Value")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #