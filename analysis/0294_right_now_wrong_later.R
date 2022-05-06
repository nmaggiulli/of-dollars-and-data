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
library(tidyverse)

folder_name <- "0294_right_now_wrong_later"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_date <- function(start_date, end_date){
  
  assign("start_dt", start_date, envir = .GlobalEnv)
  assign("end_dt", end_date, envir = .GlobalEnv)
  
  raw <- read.csv(paste0(importdir, "/0294_ycharts_spx/SPX_data.csv")) %>%
          mutate(date = as.Date(Period),
                 index_spx = `S.P.500.Level`) %>%
          select(date, index_spx) %>%
          arrange(date) %>%
          filter(date >= start_dt, date <= end_dt)
  
  dd <- drawdown_path(raw)
  
  first_index <- raw[1, "index_spx"]
  
  df <- raw %>%
    left_join(dd) %>%
    mutate(index_spx = index_spx/first_index,
           dd_number = 0) 
  
  min_year <- min(year(df$date))
  max_year <- max(year(df$date))
  
  # Create a set of drawdown percentages and lag lengths to loop through
  dd_percentages <- seq(-0.10, -0.2, -0.1)
  
  for(dd_pct in dd_percentages){
    # Set DD percent as a string
    dd_pct_string <- paste0(dd_pct*-100)
    dd_pct_name <- paste0(dd_pct_string, "% Drawdowns")
    
    # Start a DD counter at 1
    dd_count <- 1
    # Calculate when drawdown occurs
    for(i in 1:nrow(df)){
      pct <- df[i, "pct"]
      prior_dd_number <- df[(i-1), "dd_number"]
      
      if(i == 1){
        df[i, "dd_number"] <- 0
      } else if(pct < dd_pct & prior_dd_number == 0){
        df[i, "dd_number"] <- dd_count
        dd_count <- dd_count + 1
      } else if (pct == 0){
        df[i, "dd_number"] <- 0
      } else {
        df[i, "dd_number"] <- prior_dd_number
      }
    }
      
    dd_mins <- df %>%
                  filter(dd_number != 0) %>%
                  group_by(dd_number) %>%
                  summarise(pct = min(pct),
                            bottom = 1) %>%
                  ungroup() %>%
                  select(dd_number, pct, bottom)
      
    df_w_mins <- df %>%
                left_join(dd_mins) %>%
                arrange(desc(date))
    
    
    found_bottom <- 0
    
    for(i in 1:nrow(df_w_mins)){
      bottom <- df_w_mins[i, "bottom"]
      pct <- df_w_mins[i, "pct"]
      
      if(i > 1){
        future_dd_number <- df_w_mins[(i-1), "dd_number"]
      }
      
      if(!is.na(bottom)){
        found_bottom <- 1
      }
      
      if(found_bottom == 1){
        df_w_mins[i, "dd_number"] <- future_dd_number
        if(pct == 0){
          df_w_mins[i, "top"] <- 1
          found_bottom <- 0
        }
      }
    }
      
    df_w_mins <- df_w_mins %>% 
                    arrange(date)
    
    assign(paste0("final_df_", dd_pct_string), df_w_mins, envir = .GlobalEnv)
  }
  
  plot_tops_bottoms <- function(dd_pct_string, n_days, start_date, end_date){
    
    df_name <- paste0("final_df_", dd_pct_string)
    final_df <- get(df_name)
    
    tops <- final_df %>%
      filter(!is.na(top)) %>%
      select(date, index_spx) %>%
      gather(-date, key=key, value = value)
    
    bottoms <- final_df %>%
      filter(!is.na(bottom)) %>%
      select(date, index_spx) %>%
      gather(-date, key=key, value = value)
    
    #Create data with the appropriate lag
    in_out <- 1
    for(j in 1:nrow(final_df)){
      if(j > 1){
        ret <- final_df[j, "index_spx"]/final_df[(j-1), "index_spx"] - 1
      }
      
      if(j == 1){
        final_df[j, "lagged_var"] <- 1
      } else if (j <= n_days){
        final_df[j, "lagged_var"] <- final_df[(j-1), "lagged_var"] * (1 + ret)
      } else {
        top <- final_df[(j-n_days), "top"]
        bot <- final_df[(j-n_days), "bottom"]
        
        if (in_out == 1){
          final_df[j, "lagged_var"] <- final_df[(j-1), "lagged_var"] * (1 + ret)
        } else {
          final_df[j, "lagged_var"] <- final_df[(j-1), "lagged_var"]
        }
        
        if (!is.na(top) & in_out == 1){
          # sell
          in_out <- 0
        } else if (!is.na(bot) & in_out == 0){
          # buy
          in_out <- 1
        }
      } 
    }
    
    to_plot <- final_df %>%
      select(date, index_spx, lagged_var) %>%
      gather(-date, key=key, value=value)
    
    if(n_days < 10){
      n_days_string <- paste0("00", n_days)
    } else if(n_days < 100){
      n_days_string <- paste0("0", n_days)
    } else{
      n_days_string <- paste0(n_days)
    }
    
    if(n_days == 20){
      title_period_string <- "1 Month"
    } else if (n_days == 60){
      title_period_string <- "3 Months"
    } else if (n_days == 125){
      title_period_string <- "6 Months"
    } else if (n_days == 250){
      title_period_string <- "1 Year"
    } else{
      title_period_string <- paste0(n_days, " Days")
    }
    
    start_string <- date_to_string(start_dt)
    end_string <- date_to_string(end_dt)
    
    
    # Reset file path
    file_path <- paste0(out_path, "/reasonable_timing_dd_", dd_pct_string, "_", n_days_string, "_days_", start_string, "_", end_string, ".jpeg")
    
    # Set source/note
    source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
    note_string   <- str_wrap(paste0("Note: Does not includes dividends or inflation.  ",
                                      "Only shows drawdowns of ", dd_pct_string, "% or greater."), 
                              width = 85)
    
    start_yr <- year(start_dt)
    end_yr <- year(end_dt)
    
    plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
      geom_line() +
      geom_point(data=tops, aes(x=date, y=value), col="green") +
      geom_point(data=bottoms, aes(x=date, y=value), col="red") +
      scale_y_continuous(label = dollar, trans = log10_trans()) +
      scale_color_manual(values = c("black", "blue"), guide = "none") +
      of_dollars_and_data_theme +
      ggtitle(paste0("Market Timing vs. Buy & Hold\nWhen Missing the Top/Bottom by ", title_period_string, "\n",
                     start_yr, "-", end_yr)) +
      labs(x="Date", y="Growth of $1",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    assign("to_plot", to_plot, envir = .GlobalEnv)
  }
  
  plot_tops_bottoms(10, 125) 
  
}

plot_date(as.Date("1950-01-01"), as.Date("1960-01-01"))
plot_date(as.Date("1960-01-01"), as.Date("1970-01-01"))
plot_date(as.Date("1970-01-01"), as.Date("1980-01-01"))
plot_date(as.Date("1980-01-01"), as.Date("1990-01-01"))
plot_date(as.Date("1990-01-01"), as.Date("2000-01-01"))
plot_date(as.Date("2000-01-01"), as.Date("2010-01-01"))
plot_date(as.Date("2010-01-01"), as.Date("2020-01-01"))
plot_date(as.Date("1950-01-01"), as.Date("2020-01-01"))

 



# ############################  End  ################################## #