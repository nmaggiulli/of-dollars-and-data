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

folder_name <- "0233_dow_watermark"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Bring in Dow and filter to local bottom on 3/23/2020
raw <- read_excel(paste0(importdir, "0233_dow_rolling_90_day_watermark/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index_dow")) %>%
          filter(date <= "2020-03-23")

first_year <- min(year(raw$date))

df <- raw %>%
        arrange(desc(date))

# Find low matermark
absolute_minumum <- 10^8

for(i in 1:nrow(df)){
  current_p <- df[i, "index_dow"]
  if (current_p < absolute_minumum){
    df[i, "low_watermark"] <- current_p
    absolute_minumum <- current_p
  } else{
    df[i, "low_watermark"] <- absolute_minumum
  }
}

df <- df %>%
        arrange(date) %>%
        mutate(rolling_90day_index = rollmean(x = index_dow, 60, align = "right", fill = NA)) %>%
        drop_na %>%
        mutate(lower_future = ifelse(low_watermark < (1-.17)*index_dow, 1, 0),
               lead_date = lead(date, 1))

to_plot <- df %>%
              select(date, lead_date, index_dow, lower_future)

shading <- to_plot %>%
                  filter(lower_future == 1) %>%
                  mutate(max_dow = max(to_plot$index_dow))

file_path <- paste0(out_path, "/dow_watermark_decline_17pct.jpeg")
source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=date, y=index_dow)) +
  geom_line(col = "black") +
  geom_rect(data=shading, 
            aes(xmin = date, ymin = 0, 
    xmax = lead_date, ymax = max_dow), fill = "red", alpha = 0.2) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Time Where the Dow Would Decline\nBy 17% or More in the Future")) +
  labs(x="Date", y="Index Level (Log Scale)",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #