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
library(xtable)
library(tidyverse)

folder_name <- "_mttw/0010_global_stock_performance"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

pct_for_html <- function(x){
  t <- paste0(100*round(x, 3), "%")
  return(t)
}

countries <- c("日本", "美國", "加拿大", "英國", 
              "印度", "德國", 
              "澳洲", "義大利", "CPI", "rfr")

exclude <- c("CPI", "rfr")

my_colors <- c("black",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6")

raw <- read.csv(paste0(importdir, "/_mttw/0010_global_stock_performance/GrowthOfWealth_20250727131334.csv"),
                skip = 7,
                col.names = c("date", countries)) %>%
        filter(!is.na(`美國`)) %>%
        mutate(date = as.Date(date, "%m/%d/%Y"),
               yr = year(date),
               mt = month(date)) %>%
        filter(date <= "2024-12-31") %>%
        arrange(date)

long <- raw %>%
  gather(-date, -yr, -mt, key=key, value=value) 

last_value <- long %>%
              filter(date == max(long$date)) %>%
              arrange(desc(value)) %>%
              mutate(annualized = value^(1/30) - 1)

cpi_ann <- last_value %>%
              filter(key == "CPI") %>%
              pull(annualized)

last_value <- last_value %>%
                filter(!(key %in% exclude)) %>%
                mutate(real_annualized = (1+annualized)/(1+cpi_ann) - 1)

to_plot <- long %>%
              filter(!(key %in% exclude))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

# Do Growth of dollar since march 2009
march_2009 <- long %>%
              filter(!(key %in% exclude)) %>%
                filter(yr == 2009, mt == 3) %>%
                rename(first = value) %>%
                select(key, first)

to_plot <- long %>%
  filter(!(key %in% exclude),
         date >= "2009-03-01") %>%
  left_join(march_2009) %>%
  mutate(value = value/first) %>%
  select(-first)

last_value <- to_plot %>%
              filter(date == max(to_plot$date)) %>%
              arrange(desc(value))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

file_path <- paste0(out_path, "/global_stock_growth_of_dollar_2009_2024.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("1美元的成長\n(2009-2024)")) +
  labs(x = "日期" , y = "1美元的成長")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Growth of dollar from 2000-2009
jan_2000 <- long %>%
  filter(!(key %in% exclude)) %>%
  filter(yr == 2000, mt == 1) %>%
  rename(first = value) %>%
  select(key, first)

to_plot <- long %>%
  filter(!(key %in% exclude),
         date >= "2000-01-01",
         date <= "2009-03-01") %>%
  left_join(jan_2000) %>%
  mutate(value = value/first) %>%
  select(-first)

last_value <- to_plot %>%
  filter(date == max(to_plot$date)) %>%
  arrange(desc(value))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

file_path <- paste0(out_path, "/global_stock_growth_of_dollar_2000_2009.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("1美元的成長1\n(2000-2009)")) +
  labs(x = "日期" , y = "1美元的成長")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #