cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(tidyverse)

folder_name <- "_mttw/0008_taiwan_inflation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_msci <- read.csv(paste0(importdir, "/", folder_name, "/MSTW_data.csv"),
                     col.names = c("date", "index_tw")) %>%
            mutate(date = as.Date(date)) %>%
            arrange(date)

raw_cpi <- read.csv(paste0(importdir, "/", folder_name, "/tw_cpi.csv"), col.names = c("date", "rate_cpi")) %>%
            mutate(date = as.Date(date, format = "%m/%d/%y"),
                   year = year(date)) %>%
            select(year, rate_cpi)

msci_monthly <- raw_msci %>%
                  mutate(month = as.Date(paste0(year(date), "-", month(date), "-01"))) %>%
                  group_by(month) %>%
                  summarize(index_tw = mean(index_tw)) %>%
                  ungroup()

start_month <- min(msci_monthly$month)
end_month <- max(msci_monthly$month)

tw_cpi <- data.frame(month = seq.Date(from = start_month, to = end_month, by = "month")) %>%
                  mutate(year = year(month)) %>%
                  left_join(raw_cpi) %>%
                  mutate(monthly_cpi = (rate_cpi/100+1)^(1/12))

# Initialize the index column
tw_cpi$index_cpi <- NA
tw_cpi$index_cpi[1] <- 1

# Loop to calculate cumulative index
for(i in 2:nrow(tw_cpi)) {
  tw_cpi$index_cpi[i] <- tw_cpi$index_cpi[i-1] * tw_cpi$monthly_cpi[i]
}

first_tw <- pull(msci_monthly[1, "index_tw"])

to_plot <- msci_monthly %>%
              left_join(tw_cpi %>% select(-year, -monthly_cpi, -rate_cpi)) %>%
              mutate(index_tw = index_tw/first_tw,
                     index_tw_real = index_tw/index_cpi) %>%
              select(-index_cpi) %>%
              gather(-month, key=key, value=value)

start_year <- year(min(to_plot$month))
end_year <- year(max(to_plot$month))

file_path <- paste0(out_path, "/nominal_vs_real_taiwan_msci_", start_year, "_", end_year, ".jpeg")

text_labels <- data.frame()

text_labels[1, "month"] <- as.Date("2022-01-01")
text_labels[1, "value"] <- 3.5
text_labels[1, "key"] <- "index_tw"
text_labels[1, "label"] <- paste0("名目價值")

text_labels[2, "month"] <- as.Date("2022-01-01")
text_labels[2, "value"] <- 0.75
text_labels[2, "key"] <- "index_tw_real"
text_labels[2, "label"] <- paste0("通膨調整後價值")

# Plot the results
plot <- ggplot(to_plot, aes(x = month, y = value, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x = month, y = value, label = label, col = key),
            family = "my_font") +
  scale_color_manual(guide = "none", values = c("blue", "black")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("新台幣 1 元的報酬成長\nMSCI 台灣\n(", start_year, "-", end_year, ")")) +
  labs(x = "日期" , y = "1 元成長值")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #