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

folder_name <- "0242_levered_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow_pre_2020 <- read_excel(paste0(importdir, "/0242_levered_returns/Dow daily 2020.xlsx"),
                           col_names = c("date", "index")) %>%
                  mutate(date = as.Date(date)) %>%
                  filter(year(date) < 2020)

dow_2020 <- read.csv(paste0(importdir, "/0242_levered_returns/DJI_data.csv"),
                       col.names = c("date", "index")) %>%
              mutate(date = as.Date(date)) %>%
              filter(year(date) == 2020)

n_years <- 20
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

to_plot <- final_results %>%
              mutate(ann_1x = port_1x^(1/n_years) - 1,
                     ann_2x = port_2x^(1/n_years) - 1,
                     ann_3x = port_3x^(1/n_years) - 1,
                     `2x Leverage` = ann_2x - ann_1x,
                     `3x Leverage` = ann_3x - ann_1x) %>%
          select(start_date, `2x Leverage`, `3x Leverage`) %>%
          gather(-start_date, key=key, value=value)

file_path <- paste0(out_path, "/levered_outperformance_", n_years, "yrs.jpeg")
source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=start_date, y= value, col = key)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_discrete() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Dow Jones Levered Outperformance\nOver ", n_years, " Years")) +
  labs(x="Date", y="Annualized Outperformance",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #