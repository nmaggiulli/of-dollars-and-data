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
library(quantmod)
library(tidyverse)

folder_name <- "0414_nvda_valuation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in P/S
raw <- read.csv(paste0(importdir, "/", folder_name, "/MSFT_NVDA_data.csv"),
                col.names = c("date", "NVDA", "MSFT")) %>%
        mutate(date = as.Date(date)) %>%
        arrange(date)

raw_long <- raw %>%
                gather(-date, key=key, value=value)

process_peak <- function(peak_date, symbol){
  tmp_long <- raw_long %>%
                mutate(lookfwd = lead(date, lookback))
  
  start_date <- tmp_long %>%
                filter(lookfwd == peak_date) %>%
                pull(date)
  
  tmp <- tmp_long %>%
            select(-lookfwd) %>%
            filter(date >= start_date, key == symbol) %>%
            mutate(day = row_number())
  
  return(tmp)
}

lookback <- 2500

nvda_peak <- process_peak(as.Date("2023-07-18"), "NVDA")
msft_peak <- process_peak(as.Date("1999-12-27"), "MSFT")

to_plot <- msft_peak %>%
              bind_rows(nvda_peak)

file_path <- paste0(out_path, "/nvda_msft_hist_ps_ratio_", lookback, ".jpeg")
source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
note_string <- paste0("Note: The peak P/S ratio for each stock is aligned on the ", formatC(lookback, big.mark = ","), "th trading day.")

text_labels <- data.frame()
label_day <- 4500

text_labels[1, "day"] <- label_day
text_labels[1, "value"] <- 30
text_labels[1, "key"] <- "NVDA"
text_labels[1, "label"] <- "NVDA (Today)"
text_labels[2, "day"] <- label_day
text_labels[2, "value"] <- 20
text_labels[2, "key"] <- "MSFT"
text_labels[2, "label"] <- "MSFT (DotCom)"

plot <- ggplot(to_plot, aes(x = day, y = value, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=day, y=value, col = key, label = label),
            family = my_font) +
  scale_x_continuous(label = comma) +
  scale_color_manual(guide = "none", values = c("black", "red")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Nvidia vs. Microsoft\nHistorical P/S Ratio")) +
  labs(x="Trade Day", y="P/S Ratio",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #