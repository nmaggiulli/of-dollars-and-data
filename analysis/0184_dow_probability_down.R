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
library(tidyverse)

folder_name <- "0184_dow_probability_down"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Bring in Dow and filter to local bottom on 3/23/2020
raw <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
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
        mutate(max_loss = low_watermark/index_dow - 1)

plot_all_by_date <- function(start_date){
    
  df <- df %>%
          filter(date >= start_date)
  
  start_date_string <- date_to_string(start_date)
  
  # Plot Dow vs. watermark
  to_plot <- df %>%
    select(date, index_dow, low_watermark) %>%
    gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/dow_low_watermark_", start_date_string, ".jpeg")
  source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = comma, trans = log10_trans()) +
    scale_color_manual(guide = FALSE, values = c("black", "red")) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow Index and Low Watermark")) +
    labs(x="Date", y="Index Value",
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  to_plot <- df %>%
    select(date, index_dow, low_watermark) %>%
    gather(-date, key=key, value=value)
  

  
  # Plot max_loss
  to_plot <- df

  median_max_loss <- quantile(to_plot$max_loss, probs = 0.5)
  pct25_max_loss <- quantile(to_plot$max_loss, probs = 0.25)
  pct75_max_loss <- quantile(to_plot$max_loss, probs = 0.75)
  
  file_path <- paste0(out_path, "/dow_max_loss_", start_date_string, ".jpeg")
  source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"
  note_string <- str_wrap(paste0("Note:  The median maximum future loss over this time period is ",
                                 round(100*median_max_loss, 1), "%, while the 75th percentile maximum future loss is ",
                                 round(100*pct25_max_loss, 1),"%, and the 25th percentile maximum future loss is ",
                                 round(100*pct75_max_loss, 1), "%."),
                          width = 85)
  
  min_date <- min(to_plot$date)
  max_date <- max(to_plot$date)
  
  plot <- ggplot(to_plot, aes(x=date, y=max_loss)) +
    geom_line(col = "black") +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow Maximum Loss In Future\n",
                   format.Date(min_date, "%m/%d/%Y"),
                   "-",
                   format.Date(max_date, "%m/%d/%Y"))) +
    labs(x="Date", y="Maximum Future Loss",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Find probability of being down X% in the future
  probs <- seq(-0.05, -0.5, -0.05)
  
  for(p in probs){
    tmp_p <- to_plot %>%
                mutate(p_down = ifelse(max_loss < p, 1, 0)) %>%
                summarise(mean_p_down = mean(p_down)) %>%
                mutate(p_down = -1*p) %>%
                select(p_down, mean_p_down)
    
    if(p == max(probs)){
      final_probs <- tmp_p
    } else{
      final_probs <- bind_rows(final_probs, tmp_p)
    }
  }
  
  file_path <- paste0(out_path, "/dow_prob_down_", start_date_string, ".jpeg")
  source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"
  
  plot <- ggplot(final_probs, aes(x=p_down, y=mean_p_down)) +
    geom_line(col = "black") +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow Probability of Being Down in Future\n",
                   format.Date(min_date, "%m/%d/%Y"),
                   "-",
                   format.Date(max_date, "%m/%d/%Y"))) +
    labs(x="Future Percentage Decline", y="Probability",
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_all_by_date("1915-01-05")
plot_all_by_date("1950-01-01")

# ############################  End  ################################## #