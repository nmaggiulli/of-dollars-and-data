cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidylog)
library(zoo)
library(ggjoy)
library(tidyverse)

folder_name <- "0262_crave_risk_lose_reward"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

first_drops <- paste0("drop", seq(1,7))
last_drops <- paste0("drop", seq(8, 20))
col_types_num <- rep("numeric", 23)

raw <- read_excel(paste0(importdir, "0261_aaii_data/asset.xls"),
                  skip = 2,
                  col_names = c("date", 
                                first_drops,
                                "pct_stocks", "pct_bonds", "pct_cash",
                                last_drops
                                ),
                  col_types = c("date", col_types_num)) %>%
          select(date, contains("pct_")) %>%
          filter(!is.na(pct_stocks))

first_date <- as.Date(min(raw$date, na.rm = TRUE) + days(1) - months(1))
last_date <- as.Date(max(raw$date, na.rm = TRUE))

date_seq <- seq.Date(first_date, last_date, by = "month")

df <- raw %>%
        head(length(date_seq)) %>%
        select(-date) %>%
        bind_cols(date = date_seq) %>%
        select(date, contains("pct_"))

to_plot <- df %>%
            mutate(`Stocks` = rollmean(pct_stocks, 60, align = "right", fill = "NA"),
                   `Bonds` = rollmean(pct_bonds, 60, align = "right", fill = "NA"),
                   ) %>%
              select(date, Stocks, Bonds) %>%
              gather(-date, key=key, value=value) %>%
              drop_na()

file_path <- paste0(out_path, "/bond_stock_ma_60.jpeg")
source_string <- "Source: AAII Asset Allocation Survey (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x= date, y=value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_date(breaks = c(
    as.Date("1995-01-01"),
    as.Date("2000-01-01"),
    as.Date("2005-01-01"),
    as.Date("2010-01-01"),
    as.Date("2015-01-01"),
    as.Date("2020-01-01")
  ),
  date_labels = "%Y") +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("5-Year Average Investor Allocation to\nStocks & Bonds")) +
  labs(x="Date", y="Average Investor Allocation",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #