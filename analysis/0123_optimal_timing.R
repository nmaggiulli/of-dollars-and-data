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

raw <- read.csv(paste0(importdir, "/0123_ycharts_spxtr/SPXTR_data.csv")) %>%
        mutate(date = as.Date(Period),
               index_sp500_tr = `S.P.500.Total.Return.Level`) %>%
        select(date, index_sp500_tr) %>%
        arrange(date)

dd <- drawdown_path(raw)

first_index <- raw[1, "index_sp500_tr"]

df <- raw %>%
        left_join(dd) %>%
        mutate(index_sp500_tr = index_sp500_tr/first_index,
               ret = index_sp500_tr/lag(index_sp500_tr) - 1)
        
dd_pct <- -0.2
dd_counter <- 1

for(i in 1:nrow(dd)){
  pct <- df[i, "pct"]
  prior_dd_initial <- df[(i-1), "dd_initial"]
  
  if(i == 1){
    df[i, "dd_initial"] <- 0
  } else if(pct < dd_pct & prior_dd_initial == 0){
    df[i, "dd_initial"] <- dd_counter
    dd_counter <- dd_counter + 1
  } else if (pct == 0){
    df[i, "dd_initial"] <- 0
  } else {
    df[i, "dd_initial"] <- prior_dd_initial
  }
}

dd_mins <- df %>%
              filter(dd_initial != 0) %>%
              group_by(dd_initial) %>%
              summarize(pct = min(pct),
                        bottom = 1) %>%
              ungroup() %>%
              select(dd_initial, pct, bottom)

df_w_mins <- df %>%
            left_join(dd_mins) %>%
            arrange(desc(date))


found_bottom <- 0

for(i in 1:nrow(df_w_mins)){
  bottom <- df_w_mins[i, "bottom"]
  pct <- df_w_mins[i, "pct"]
  
  if(!is.na(bottom)){
    found_bottom <- 1
  }
  
  if(found_bottom == 1 & pct == 0){
    df_w_mins[i, "top"] <- 1
    found_bottom <- 0
  }
}

df_w_mins <- df_w_mins %>% 
                arrange(date)

df_matrix <- df_w_mins %>% 
              arrange(date) %>%
              select(-date) %>%
              as.matrix()

n_days_from_bottom_top <- c(1, 10, 20, 62, 125, 250)
  
for(i in 1:length(n_days_from_bottom_top)){
  n_days <- n_days_from_bottom_top[i]
  
  if(n_days < 10){
    n_days_string <- paste0("0", n_days)
  } else{
    n_days_string <- n_days
  }
  
  # Add an NA var and name it
  lagged_var <- rep(NA, nrow(df_matrix))
  df_matrix <- cbind(df_matrix, lagged_var)
  
  in_out <- 1
  for(j in 1:nrow(df_matrix)){
    ret <- df_matrix[j, "ret"]
    
    if(j == 1){
      df_matrix[j, "lagged_var"] <- 1
    } else if (j <= n_days){
      df_matrix[j, "lagged_var"] <- df_matrix[(j-1), "lagged_var"] * (1 + ret)
    } else {
      top <- df_matrix[(j-n_days), "top"]
      bot <- df_matrix[(j-n_days), "bottom"]
      
      if (in_out == 1){
        df_matrix[j, "lagged_var"] <- df_matrix[(j-1), "lagged_var"] * (1 + ret)
      } else {
        df_matrix[j, "lagged_var"] <- df_matrix[(j-1), "lagged_var"]
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
  colnames(df_matrix)[ncol(df_matrix)] <- paste0("lag_", n_days_string)
}

final_df <- df_matrix %>%
            as.data.frame() %>%
            mutate(date = df_w_mins$date)

to_plot <- final_df %>%
            select(date, index_sp500_tr, contains("lag_250")) %>%
            gather(-date, key=key, value = value)

tops <- final_df %>%
          filter(!is.na(top)) %>%
          select(date, index_sp500_tr) %>%
          gather(-date, key=key, value = value)

bottoms <- final_df %>%
              filter(!is.na(bottom)) %>%
              select(date, index_sp500_tr) %>%
              gather(-date, key=key, value = value)

ggplot(to_plot, aes(x=date, y=value, col= key)) +
  geom_line(alpha = 0.8) +
  geom_point(data=tops, aes(x=date, y=value), col="green") +
  geom_point(data=bottoms, aes(x=date, y=value), col="red") +
  scale_y_continuous(label = dollar) +
  #scale_color_manual(values = c("black", "blue")) +
  of_dollars_and_data_theme

# ############################  End  ################################## #