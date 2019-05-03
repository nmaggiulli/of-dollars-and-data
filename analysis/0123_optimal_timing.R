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

folder_name <- "0123_optimal_timing"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Set this to 1 to re-import and build the Dow data
read_raw <- 0

if(read_raw == 1){
  raw <- read.csv(paste0(importdir, "/0123_ycharts_spxtr/SPXTR_data.csv")) %>%
          mutate(date = as.Date(Period),
                 index_spxtr = `S.P.500.Total.Return.Level`) %>%
          select(date, index_spxtr) %>%
          arrange(date)
  
  dd <- drawdown_path(raw)
  
  first_index <- raw[1, "index_spxtr"]
  
  df <- raw %>%
    left_join(dd) %>%
    mutate(index_spxtr = index_spxtr/first_index,
           dd_number = 0) 
  saveRDS(df, paste0(localdir, "0123_spxtr_w_dd.Rds"))
} else{
  df <- readRDS(paste0(localdir, "0123_spxtr_w_dd.Rds"))
}

min_year <- min(year(df$date))
max_year <- max(year(df$date))

# create final results data frame
final_results <- data.frame(dd_pct = c(),
                            dd_pct_name = c(),
                            lag_length = c(),
                            avg_perf = c())

final_results_row_number <- 1

# Create a set of drawdown percentages and lag lengths to loop through
dd_percentages <- seq(-0.10, -0.2, -0.05)
lags <- c(0, 1, seq(5, 500, 5))

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
                summarize(pct = min(pct),
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
  
  # Find the number of drawdowns
  n_dds <- max(df_w_mins$dd_number)
  
  # Initial final perf vector
  final_perf_vector <- seq(1, n_dds)
  
  # Loop through each drawdown and calculate the performance
  for (lag in lags){
    final_results[final_results_row_number, "dd_pct"] <- dd_pct
    final_results[final_results_row_number, "dd_pct_name"] <- dd_pct_name
    final_results[final_results_row_number, "lag_length"] <- lag
    for(i in 1:n_dds){
      
      tmp <- filter(df_w_mins, dd_number == i) %>%
                mutate(row_num = row_number())
      
      nrow_tmp <- nrow(tmp)
      
      top <- filter(tmp, top == 1) %>% pull(index_spxtr)
      
      bot_row <- filter(tmp, bottom == 1) %>% pull(row_num)
      
      if(nrow_tmp < (lag + 1)){
        final_perf <- 0
      } else{
        top_sell <- tmp[(lag + 1), "index_spxtr"]
        
        # See if buy comes at end or not
        if(bot_row + lag >  nrow_tmp){
          bot_buy <- tmp[nrow_tmp, "index_spxtr"]
        } else{
          bot_buy <- tmp[(bot_row + lag), "index_spxtr"]
        }
        final_perf <-  top_sell/bot_buy - 1
      }
      final_perf_vector[i] <- final_perf
    }
    # if(dd_pct_string == "15"){
    #   print(paste0("Lag Length is ", lag, " days."))
    #   print(final_perf_vector)
    # }
    
    final_results[final_results_row_number, "avg_perf"] <- mean(final_perf_vector)
    final_results_row_number <- final_results_row_number + 1
  }
}

# Plot overall results
# Reset file path
file_path <- paste0(out_path, "/overall_results.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts, ", min_year, "-", max_year," (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Assumes you miss selling at the top and re-buying at the bottom by the same number of trading days."), 
                          width = 85)

text_labels <- data.frame()
text_labels[1, "avg_perf"] <- -0.40
text_labels[1, "label"] <- "Market Timing Underperforms"
text_labels[1, "lag_length"] <- 250
text_labels[2, "avg_perf"] <- 0.8
text_labels[2, "label"] <- "Market Timing Outperforms"
text_labels[2, "lag_length"] <- 250

plot <- ggplot(final_results, aes(x=lag_length, y=avg_perf, col = dd_pct_name)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  geom_text_repel(data=text_labels, aes(x=lag_length, y=avg_perf),
                  color = "black",
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1) +
  scale_y_continuous(label = percent, breaks = seq(-.5, 1.25, .25), limits = c(-0.5, 1.25)) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  scale_color_discrete() +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Market Timing Only Works\nIn Specific Circumstances")) +
  labs(x="Number of Days Missed From Top/Bottom", y="Average Outperformance",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Plot the 10pct and 20 pct tops and bottoms

plot_tops_bottoms <- function(dd_pct_string, n_days){
  
  df_name <- paste0("final_df_", dd_pct_string)
  final_df <- get(df_name)
  
  to_plot <- final_df %>%
              select(date, index_spxtr) %>%
              gather(-date, key=key, value=value)
  
  tops <- final_df %>%
    filter(!is.na(top)) %>%
    select(date, index_spxtr) %>%
    gather(-date, key=key, value = value)
  
  bottoms <- final_df %>%
    filter(!is.na(bottom)) %>%
    select(date, index_spxtr) %>%
    gather(-date, key=key, value = value)
  
  if(n_days == 0){
    # Reset file path
    file_path <- paste0(out_path, "/market_dd_pct_", dd_pct_string, ".jpeg")
    
    # Set note
    note_string   <- str_wrap(paste0("Note:  Market tops are in green and market bottoms are in red for the drawdown size listed.  ",
                                     "Includes dividends, but not adjusted for inflation."), 
                              width = 85)
    
    plot <- ggplot(to_plot, aes(x=date, y=value)) +
      geom_line() +
      geom_point(data=tops, aes(x=date, y=value), col="green") +
      geom_point(data=bottoms, aes(x=date, y=value), col="red") +
      scale_y_continuous(label = dollar) +
      scale_color_manual(values = c("black"), guide = FALSE) +
      of_dollars_and_data_theme +
      ggtitle(paste0("S&P 500 Tops and Bottoms\nWhen a ", dd_pct_string, "% Drawdown (or Greater) Occurs")) +
      labs(x="Date", y="Portfolio Value",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
  #Create data with the appropriate lag
  in_out <- 1
  for(j in 1:nrow(final_df)){
    if(j > 1){
      ret <- final_df[j, "index_spxtr"]/final_df[(j-1), "index_spxtr"] - 1
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
    select(date, index_spxtr, lagged_var) %>%
    gather(-date, key=key, value=value)
  
  if(n_days < 10){
    n_days_string <- paste0("00", n_days)
  } else if(n_days < 100){
    n_days_string <- paste0("0", n_days)
  } else{
    n_days_string <- paste0(n_days)
  }
  
  # Reset file path
  file_path <- paste0(out_path, "/timing_dd_", dd_pct_string, "_", n_days_string, "_days.jpeg")
  
  # Set source/note
  note_string   <- str_wrap(paste0("Note: Includes dividends, but not adjusted for inflation, taxes, or transaction costs.  ",
                                    "Only shows drawdowns of ", dd_pct_string, "% or greater."), 
                            width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_point(data=tops, aes(x=date, y=value), col="green") +
    geom_point(data=bottoms, aes(x=date, y=value), col="red") +
    scale_y_continuous(label = dollar, trans = log10_trans()) +
    scale_color_manual(values = c("black", "blue"), guide = FALSE) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Market Timing vs. Buy & Hold\nWhen Missing the Top/Bottom by ", n_days, " Days")) +
    labs(x="Date", y="Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  assign("to_plot", to_plot, envir = .GlobalEnv)
}

n_days_to_plot <- c(0, 5, 20, 60, 250)

for (days in n_days_to_plot){
  plot_tops_bottoms(20, days) 
}

plot_tops_bottoms(10, 0)  

create_gif(path = out_path,
           file_stub = paste0("market_dd_pct_*.jpeg"),
           speed_milliseconds = 180,
           out_name = paste0("_gif_market_dd.gif"))


# ############################  End  ################################## #