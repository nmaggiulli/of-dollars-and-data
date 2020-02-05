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

folder_name <- "xxxx_spanish_flu_vs_dow"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

flu_data <- read.csv(paste0(importdir, "xxxx_spanish_flu_data/spanish_flu_wiki_webplotdigi.csv"),
                     col.names = c("date", "deaths_per_1000")) %>%
                mutate(date = as.Date(date, "%Y/%m/%d")) %>%
                arrange(date) %>%
                mutate(daily_diff = (lead(deaths_per_1000,1) - deaths_per_1000)/as.numeric(lead(date,1) - date))

flu_data[1, "date"] <- as.Date("1918-06-28")

min_date <- min(flu_data$date)
max_date <- max(flu_data$date)

all_dates <- data.frame(date = seq.Date(min_date, max_date + days(365), 1))

dow_daily <- read_excel(paste0(importdir, "xxxx_spanish_flu_data/daily_dow_bloomberg.xlsx"), sheet="Sheet1",
                        col_names = c("date", "index_dow")) %>%
              mutate(date = as.Date(date)) %>%
              filter(date >= min_date, date <= max_date + days(365))

df <- all_dates %>%
        left_join(dow_daily) %>%
        left_join(flu_data) %>%
        arrange(date)

for(i in 2:nrow(df)){
  if(is.na(df[i, "daily_diff"])){
    df[i, "daily_diff"] <- df[(i-1), "daily_diff"]
  }
  if(is.na(df[i, "index_dow"])){
    df[i, "index_dow"] <- df[(i-1), "index_dow"]
  }
  
  if(df[i, "date"] <= max_date){
    df[i, "deaths_per_1000"] <-  df[(i-1), "deaths_per_1000"] + df[i, "daily_diff"]
  } else{
    df[i, "deaths_per_1000"] <- NA
    df[i, "daily_diff"] <- NA
  }

}

# Plot deaths per 1,000 over time
file_path <- paste0(out_path, "/per_capita_deaths_spanish_flu.jpeg")
source_string <- "Source:  Wikimedia Commons (OfDollarsAndData.com)"
note_string <-  str_wrap(paste0("Note:  Days with missing data were filled using linear extrapolation."), 
                         width = 85)

to_plot <- df %>%
  filter(date <= max_date)

plot <- ggplot(to_plot, aes(x=date, y=deaths_per_1000)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  scale_x_date(date_labels = "%m/%d/%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Per-Capita Deaths During the Spanish Flu")) +
  labs(x=paste0("Date"), y=paste0("Deaths Per 1,000 People"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

plot_fwd_ret <- function(n_days){
  
  file_path <- paste0(out_path, "/deaths_vs_", n_days, "_day_fwd_ret.jpeg")
  source_string <- "Source:  Bloomberg, Wikimedia Commons (OfDollarsAndData.com)"
  note_string <-  str_wrap(paste0("Note:  Days with missing data were filled using linear extrapolation."), 
                           width = 85)

  to_plot <- df %>%
    mutate(fwd_ret = lead(index_dow, n_days)/index_dow - 1) %>%
    filter(date <= max_date)
  
  plot <- ggplot(to_plot, aes(x=deaths_per_1000, y=fwd_ret)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(label = percent, limits = c(-0.1, 0.5), breaks = seq(-0.1, 0.5, 0.1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Spanish Flu Per-Capita Deaths vs.\n", n_days, "-Day Forward Return")) +
    labs(x=paste0("Deaths Per 1,000 People"), y=paste0(n_days, "-Day Forward Return"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_fwd_ret(30)
plot_fwd_ret(60)
plot_fwd_ret(90)
plot_fwd_ret(180)
plot_fwd_ret(365)




# ############################  End  ################################## #