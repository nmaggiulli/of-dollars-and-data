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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0177_after_the_rally"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_days_back <- 11
rise_cutoff <- 0.2

raw <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(ret_lag = index/lag(index, n_days_back) - 1)

first_year <- year(min(raw$date))
last_year <- year(max(raw$date))

run_fwd_rets <- function(n_days_fwd){

  df <- raw %>%
    mutate(lead_date = lead(date, n_days_fwd))

  less_than_xpct_dates <- df %>%
                          filter(ret_lag > rise_cutoff) %>%
                          select(date, lead_date)
  
  if(n_days_fwd == 5){
    assign("less_than_xpct", less_than_xpct_dates, envir = .GlobalEnv)
  }
  
  for(d in 1:nrow(less_than_xpct_dates)){
    
    my_date <- less_than_xpct_dates[d, "date"]
    future_date <- less_than_xpct_dates[d, "lead_date"]
    
    tmp <- df %>%
            filter(date > my_date, date <= future_date) %>%
            mutate(start_date = as.character(my_date)) %>%
            select(date, index, start_date)
    
    first_value <- pull(tmp[1, "index"])
    
    tmp <- tmp %>%
            mutate(index = index/first_value,
                   day = row_number())
    
    if(my_date == min(less_than_xpct_dates$date)){
      to_plot <- tmp
    } else{
      to_plot <- bind_rows(to_plot, tmp)
    }
  }
  
  avg <- to_plot %>%
          group_by(day) %>%
          summarize(index = mean(index, na.rm = TRUE)) %>%
          ungroup() %>%
          mutate(start_date = "2100-01-01")
  
  final_avg <- avg[nrow(avg), "index"] - 1
  
  if(final_avg > 0){
    up_down <- "up"
  } else{
    up_down <-"down"
  }
  
  to_plot <- to_plot %>%
              bind_rows(avg)
  
  n_days_fwd_string <- str_pad(n_days_fwd, width = 3, side="left", pad = "0")
  
  n_days <- length(unique(to_plot$start_date))
  
  text_labels <- avg %>%
                  filter(day == n_days_fwd) %>%
                  mutate(label = "Average")
  
  last_day <- to_plot %>%
                filter(day == n_days_fwd)
  
  print(paste0("N-days = ", n_days_fwd))
  print(max(last_day$index) - 1)
  print(min(last_day$index) - 1)
  
  file_path <- paste0(out_path, "/fwd_ret_", n_days_fwd_string, "_sessions.jpeg")
  source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  There were ", n_days-1, " trading days where the Dow rose by ", 
                                 100*rise_cutoff, 
                                 "% or more over the prior ",
                          n_days_back, " sessions.  ",
                        "Over the next ", n_days_fwd, 
                        " sessions, the market was ", up_down, " by ", 
                        round(100*final_avg, 1),
                        "% on average."),
                        width = 85)
  
  plot <- ggplot(to_plot, aes(x=day, y=index, col = as.factor(start_date))) + 
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_text_repel(data=text_labels, aes(x=day, y=index, label=label),
                    col = "red",
                    nudge_y = 0.02,
                    segment.colour = "transparent") +
    scale_color_manual(guide = FALSE, values = c(rep("gray", n_days-1), "red")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow Over Next ", n_days_fwd, " Sessions\nFollowing ", 100*rise_cutoff,"%+ Rise")) +
    labs(x = "Session" , y = "Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

n_fwd_days <- c(5, 10, 20, 60, 120, 250, 500, 1250)

for(n in n_fwd_days){
  run_fwd_rets(n)
}

# Plot all rise dates
file_path <- paste0(out_path, "/index_w_rise_days_dots.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

to_plot <- raw

points <- to_plot %>%
            inner_join(less_than_xpct) %>%
            select(date, index)

plot <- ggplot(to_plot, aes(x=date, y=index)) + 
  geom_line(col = "black") +
  geom_point(data = points, aes(x=date, y=index), col = "red", size = 1.5, alpha = 0.5) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow with ", 100*rise_cutoff, "%+\n", n_days_back, "-Session Increases Highlighted")) +
  labs(x = "Date" , y = "Dow Index",
       caption = paste0("\n", source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #