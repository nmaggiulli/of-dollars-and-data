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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0242_leveraged_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow_pre_2020 <- read_excel(paste0(importdir, "/0242_leveraged_returns/Dow daily 2020.xlsx"),
                           col_names = c("date", "index")) %>%
                  mutate(date = as.Date(date)) %>%
                  filter(year(date) < 2020)

dow_2020 <- read.csv(paste0(importdir, "/0242_leveraged_returns/DJI_data.csv"),
                       col.names = c("date", "index")) %>%
              mutate(date = as.Date(date)) %>%
              filter(year(date) == 2020)

n_years <- 10
n_trading_days <- n_years*250

df <- dow_pre_2020 %>%
        bind_rows(dow_2020) %>%
        arrange(date) %>%
        mutate(ret = index/lag(index, 1) - 1,
               ret_2x = ret*2 + 1,
               ret_3x = ret*3 + 1,
               ret = ret + 1,
               lead_date = lead(date, n_trading_days)) %>%
          filter(!is.na(ret))

min_date <- min(df$date)
max_date <- max(df$date)

# Fill gaps in data for YCharts ingest
to_ycharts <- data.frame(date = seq.Date(min_date, max_date, by = "day")) %>%
          left_join(df) %>%
          filter(weekdays(date) != "Saturday", weekdays(date) != "Sunday") %>%
          select(date, index) %>%
          mutate(index = na.locf(index)) %>%
          mutate(ret = round(index/lag(index, 1) - 1, 5)) %>%
          filter(!is.na(ret)) %>%
              rename(Date = date,
                     Value = ret) %>%
              select(Date, Value)

export_to_excel(to_ycharts, 
                outfile = paste0(out_path, "/dow_daily.xlsx"),
                sheetname = "dow_raw",
                new_file = 1,
                fancy_formatting = 0)

final_results <- data.frame()

all_dates <- df %>%
              select(date, lead_date) %>%
              drop_na() %>%
              distinct()

for(i in 1:nrow(all_dates)){
  print(i)
  
  dt1 <- all_dates[i, "date"]
  dt2 <- all_dates[i, "lead_date"]
  
  tmp <- df %>%
          filter(date >= dt1, date <= dt2)
  
  ret_1 <- prod(tmp$ret)
  ret_2 <- prod(tmp$ret_2x) 
  ret_3 <- prod(tmp$ret_3x)
    
  final_results[i, "start_date"] <- dt1
  final_results[i, "end_date"] <- dt2
  final_results[i, "port_1x"] <- ret_1
  final_results[i, "port_2x"] <- ret_2
  final_results[i, "port_3x"] <- ret_3
    
}

bps_fee <- 0.006

to_plot <- final_results %>%
              mutate(ann_1x = port_1x^(1/n_years) - 1,
                     ann_2x = port_2x^(1/n_years) - 1 - bps_fee,
                     ann_3x = port_3x^(1/n_years) - 1 - bps_fee,
                     `2x Leverage` = ann_2x - ann_1x,
                     `3x Leverage` = ann_3x - ann_1x) %>%
          select(start_date, `2x Leverage`, `3x Leverage`) %>%
          gather(-start_date, key=key, value=value)

avg_2x <- to_plot %>%
            filter(key == "2x Leverage") %>%
            summarize(avg = mean(value)) %>%
            pull(avg)

avg_3x <- to_plot %>%
  filter(key == "3x Leverage") %>%
  summarize(avg = mean(value)) %>%
  pull(avg)

file_path <- paste0(out_path, "/leveraged_outperformance_", n_years, "yrs.jpeg")
source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"
note_string <- str_wrap(paste0("Note: Assumes an annual fee of ", 10000*bps_fee, " basis points for each leveraged strategy. ",
                               "The average annualized outperformance of the 2x Leverage strategy is ", round(100*avg_2x, 2), "%. ",
                               "The average annualized outperformance of the 3x Leverage strategy is ", round(100*avg_3x, 2), "%."),
                        width = 85)

text_labels <- data.frame()

if(n_years == 10){
  label_date <- as.Date("1965-01-01")  
  date_breaks <- seq.Date(as.Date("1920-01-01"), as.Date("2010-01-01"), "10 years")
  y_max <- 0.4
} else if (n_years == 20){
  label_date <- as.Date("1960-01-01")  
  date_breaks <- seq.Date(as.Date("1920-01-01"), as.Date("2000-01-01"), "10 years")
  y_max <- 0.3
} else if (n_years == 30){
  label_date <- as.Date("1955-01-01")  
  date_breaks <- seq.Date(as.Date("1920-01-01"), as.Date("1990-01-01"), "10 years")
  y_max <- 0.2
}

y_min <- -1 * y_max
text_labels[1, "start_date"] <- label_date
text_labels[1, "label"] <- "Leveraged Portfolio Outperforms"
text_labels[1, "value"] <- y_max

text_labels[2, "start_date"] <- label_date
text_labels[2, "label"] <- "Leveraged Portfolio Underperforms"
text_labels[2, "value"] <- y_min + 0.05

plot <- ggplot(to_plot, aes(x=start_date, y= value, col = key)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data = text_labels, aes(x=start_date, y=value, label = label), col = "black", family = "my_font") +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_min, y_max), breaks = seq(y_min, y_max, 0.1)) +
  scale_x_date(breaks = date_breaks, date_labels = "%Y") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Dow Jones\nLeveraged Annualized Outperformance\nOver ", n_years, " Years")) +
  labs(x="Start Year", y="Annualized Outperformance",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #