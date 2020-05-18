cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(stringr)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "/_fl/0004_ue_and_stocks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

ue <- read.csv(paste0(importdir, folder_name, "/UNRATE_FRED.csv"), 
               col.names = c("date", "ue_rate")) %>%
          mutate(date = as.Date(date, format = "%Y-%m-%d"),
                 ue_rate = case_when(
                            date == "1953-05-01" | date == "1968-09-01" | date == "2006-10-01" ~ (ue_rate/100) - 0.0001,
                   TRUE~ ue_rate/100)
          )

min_date <- min(ue$date)

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  rename(index = price_plus_div) %>%
  filter(date >= min_date) %>%
  select(date, index) 

first_index <- pull(shiller[1, "index"])

shiller <- shiller %>%
              mutate(index = index/first_index)

df <- ue %>%
        left_join(shiller)

neighbor_lookback <- 16
ue_peak_lower_limit <- 0.07
ue_bottom_upper_limit <- 0.05
        
#Find UE peaks
for(i in (1 + neighbor_lookback):(nrow(df)-neighbor_lookback)){
  
  max_ue_neighbors <- max(df[(i-neighbor_lookback):(i-1), "ue_rate"], 
                          df[(i+1):(i+neighbor_lookback), "ue_rate"])
  
  min_ue_neighbors <- min(df[(i-neighbor_lookback):(i-1), "ue_rate"], 
                          df[(i+1):(i+neighbor_lookback), "ue_rate"])
  
  ue_rate <- df[i, "ue_rate"]
  
  if(ue_rate >= max_ue_neighbors & ue_rate > ue_peak_lower_limit){
    df[i, "ue_peak"] <- 1
  } else{
    df[i, "ue_peak"] <- 0
  }
  
  if(ue_rate <= min_ue_neighbors & ue_rate < ue_bottom_upper_limit){
    df[i, "ue_bottom"] <- 1
  } else{
    df[i, "ue_bottom"] <- 0
  }
  
}

for(j in 1:2){
  
  if (j == 1){
    limit <- ue_peak_lower_limit
    peak_bottom_filename <- "peaks"
    peak_bottom_note <- paste0("There have been ", sum(df$ue_peak, na.rm = TRUE), " times where unemployment peaked above ", 100*limit, "% over the time period shown.")
    title_string <- "Unemployment Peaks >"
    filtered_df <- df %>% filter(ue_peak == 1)
    dot_color <- "red"
  } else if(j == 2){
    limit <- ue_bottom_upper_limit
    peak_bottom_filename <- "bottoms"
    peak_bottom_note <- paste0("There have been ", sum(df$ue_peak, na.rm = TRUE), " times where unemployment bottomed below ", 100*limit, "% over the time period shown.")
    title_string <- "Unemployment Bottoms <"
    filtered_df <- df %>% filter(ue_bottom == 1)
    dot_color <- "blue"
  }
  
  if(j == 1){
    file_path <- paste0(out_path, "/ue_peaks_bottoms.jpeg")
    source_string <- paste0("Source:  FRED (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note:  The unemployment rate represents the number of unemployed as a percentage of the labor force.  ",
                                   "The data has been seasonally adjusted.  ",
                                   peak_bottom_note),
                            width = 75)
    
    to_plot <- df
    filtered_bottom <- df %>% filter(ue_bottom == 1)
    
    plot <- ggplot(to_plot, aes(x=date, y=ue_rate)) +
      geom_line() +
      geom_point(data=filtered_df, aes(x=date, y=ue_rate), color = dot_color, alpha = 0.7) +
      geom_point(data=filtered_bottom, aes(x=date, y=ue_rate), color = "blue", alpha = 0.7) +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("U.S. Unemployment Peaks and Bottoms")) +
      labs(x = "Date", y = "Unemployment Rate",
           caption = paste0(source_string, "\n", note_string))
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")  
  }
  
  file_path <- paste0(out_path, "/sp500_and_", peak_bottom_filename, ".jpeg")
  source_string <- str_wrap(paste0("Source:  FRED, http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"),
                            width = 75)
  note_string <- str_wrap(paste0("Note:  Real returns include reinvested dividends.  ",
                                 peak_bottom_note),
                          width = 75)
  
  plot <- ggplot(to_plot, aes(x=date, y=index)) +
    geom_line() +
    geom_point(data=filtered_df, aes(x=date, y=index), col = dot_color, alpha = 0.7) +
    scale_y_continuous(label = dollar, trans = log10_trans()) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("S&P 500 Growth and\nAll ", title_string, " ", 100*limit, "%")) +
    labs(x = "Date", y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")  
}

run_fwd_rets <- function(n_months_fwd, peak_bottom){
  
  tmp_df <- df %>%
    mutate(lead_date = lead(date, n_months_fwd))
  
  if(peak_bottom == "peak"){
    select_dates <- tmp_df %>%
      filter(ue_peak == 1) %>%
      select(date, lead_date)
    
    title_peak_bottom <- "Peak"
  } else if(peak_bottom == "bottom"){
    select_dates <- tmp_df %>%
      filter(ue_bottom == 1) %>%
      select(date, lead_date)
    
    title_peak_bottom <- "Bottom"
  }

  
  for(d in 1:nrow(select_dates)){
    
    my_date <- select_dates[d, "date"]
    future_date <- select_dates[d, "lead_date"]
    
    tmp <- tmp_df %>%
      filter(date > my_date, date <= future_date) %>%
      mutate(start_date = as.character(my_date)) %>%
      select(date, index, start_date)
    
    first_value <- tmp[1, "index"]
    
    tmp <- tmp %>%
      mutate(index = index/first_value,
             day = row_number())
    
    if(my_date == min(select_dates$date)){
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
  
  n_months_fwd_string <- str_pad(n_months_fwd, width = 3, side="left", pad = "0")
  
  n_months <- length(unique(to_plot$start_date))
  
  text_labels <- avg %>%
    filter(day == n_months_fwd) %>%
    mutate(label = "Average")
  
  last_day <- to_plot %>%
    filter(day == n_months_fwd)
  
  print(paste0("N-Months = ", n_months_fwd))
  print(max(last_day$index) - 1)
  print(min(last_day$index) - 1)
  
  file_path <- paste0(out_path, "/fwd_ret_", n_months_fwd_string, "_months_", peak_bottom, ".jpeg")
  source_string <- paste0("Source:  FRED, http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Over the next ", n_months_fwd, 
                                 " months, U.S. stocks were ", up_down, " by ", 
                                 round(100*final_avg, 1),
                                 "% on average.  Performance includes dividends and adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=day, y=index, col = as.factor(start_date))) + 
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_text_repel(data=text_labels, aes(x=day, y=index, label=label),
                    col = "red",
                    nudge_y = 0.02,
                    segment.colour = "transparent") +
    scale_color_manual(guide = FALSE, values = c(rep("gray", n_months-1), "red")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500 Over Next ", n_months_fwd, " Months\nFollowing Unemployment ", title_peak_bottom)) +
    labs(x = "Month" , y = "Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

ret_years <- c(1, 3, 5)

for(r in ret_years){
  run_fwd_rets(r*12, "peak")
  run_fwd_rets(r*12, "bottom")
}

# ############################  End  ################################## #

  
