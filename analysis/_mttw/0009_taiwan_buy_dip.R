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

folder_name <- "_mttw/0009_taiwan_buy_dip"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_msci <- read.csv(paste0(importdir, "/", folder_name, "/GrowthOfWealth_20251113221715_tw.csv"),
                     col.names = c("date", "index_tw"), skip = 6) %>%
            filter(!(date == ""), !(index_tw == "")) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y"),
                   index_tw = as.numeric(index_tw)) %>%
            arrange(date)

dd <- drawdown_path(raw_msci %>% select(date, index_tw))

start_year <- year(min(dd$date))
end_year <- year(max(dd$date))

#Plot DD
file_path <- paste0(out_path, "/taiwan_msci_dd_", start_year, "_", end_year, ".jpeg")

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(position = "identity", fill = "red") + 
  scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle("台灣MSCI指數回檔") +
  labs(x = "日期" , y = "價值損失百分比")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now bring in S&P 500
raw_sp500 <- read.csv(paste0(importdir, "/", folder_name, "/GrowthOfWealth_20251113223116_sp500.csv"),
                      col.names = c("date", "index_tw"), skip = 6) %>%
  filter(!(date == ""), !(index_tw == "")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         index_tw = as.numeric(index_tw)) %>%
  arrange(date)  

dd <- drawdown_path(raw_sp500 %>% select(date, index_tw))

start_year <- year(min(dd$date))
end_year <- year(max(dd$date))

#Plot DD for S&P 500 now too
file_path <- paste0(out_path, "/sp500_dd_", start_year, "_", end_year, ".jpeg")

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(position = "identity", fill = "red") + 
  scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle("標準普爾500指數") +
  labs(x = "日期" , y = "價值損失百分比")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #