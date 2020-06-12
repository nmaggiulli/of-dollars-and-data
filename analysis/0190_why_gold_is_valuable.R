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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(ggrepel)
library(tidyverse)

folder_name <- "0190_why_is_gold_valuable"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0190_ycharts_gold_spx/SPX_IGPUSD_data.csv"), 
                col.names = c("date","index_gold", "index_sp500")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(ret_gold = index_gold/lag(index_gold, 1) - 1,
         ret_sp500 = index_sp500/lag(index_sp500, 1) - 1) %>%
  filter(!is.na(ret_sp500),!is.na(ret_gold)) %>%
  mutate(up_down_group = case_when(
    ret_gold >= 0 & ret_sp500 >= 0 ~ "Both Up",
    ret_gold < 0 & ret_sp500 > 0 ~ "Stocks Up",
    ret_gold > 0 & ret_sp500 < 0 ~ "Gold Up",
    ret_gold <= 0 & ret_sp500 <= 0 ~ "Both Down",
    TRUE ~ "Error")
  )

raw$up_down_group <- factor(raw$up_down_group, levels = c("Both Up", "Stocks Up", "Gold Up", "Both Down"))

to_plot <- raw

file_path <- paste0(out_path, "/gold_perf_by_up_down_status.jpeg")

source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note:  Does not include dividends and is not adjusted for inflation.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=date, y=ret_gold, fill = up_down_group)) + 
  geom_bar(stat="identity") +
  facet_rep_grid(up_down_group ~ ., repeat.tick.labels = 'bottom') +
  scale_fill_manual(values = c("green", "blue", "yellow", "red"), guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-.15, .15)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Gold Performance Based on When\nStocks + Gold Are Up vs. Down")) +
  labs(x = "Date" , y = "Daily Return",
       caption = paste0(source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #