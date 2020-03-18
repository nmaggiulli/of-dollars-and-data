cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(tidyverse)

folder_name <- "0171_vix_spx"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

vix <- read.csv(paste0(importdir, "0171_ycharts_vix/VIX_data.csv"), 
                col.names = c("date","vix")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Plot Vix over time
to_plot <- vix

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/vix_over_time.jpeg")

# Set note and source string
source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                          width = 85)

plot <- ggplot(to_plot, aes(x=date, y=vix)) +
  geom_line() +
  geom_hline(yintercept = 40, linetype = "dashed") +
  of_dollars_and_data_theme +
  ggtitle(paste0("VIX Since 1990")) +
  labs(x = "Date" , y = "Level",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in SPXTR data
spxtr <- read.csv(paste0(importdir, "0171_ycharts_vix/SPXTR_data.csv"), 
                col.names = c("date","index_sp500", "index_sp500_tr")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

df <- spxtr %>%
        left_join(vix) %>%
        drop_na() %>%
        mutate(vix_bucket = case_when(
          vix < 20 ~ "<20",
          vix < 30 ~ "20-30",
          vix < 40 ~ "30-40",
          TRUE ~ "40+"
        ))

df$vix_bucket <- factor(df$vix_bucket, levels = c("<20", "20-30", "30-40",
                                                "40+"))
vix_clearing_level <- 25
for(i in 1:nrow(df)){
    
  vix_curr <- df[i, "vix"]
  if(vix_curr < vix_clearing_level){
    vix_clear <- 1
  }
  
  if(vix_curr > 40 & vix_clear == 1){
    df[i, "vix_first_40"] <- 1
    vix_clear <- 0
  } else{
    df[i, "vix_first_40"] <- 0
  }
}

# Plot S&P 500 and Vix first 40
to_plot <- df %>%
            select(date, index_sp500, vix_first_40)

points <- to_plot %>%
            filter(vix_first_40 == 1)
      
  
file_path <- paste0(out_path, "/spxtr_with_vix_40.jpeg")

# Set note and source string
source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                          width = 85)

note_string <- str_wrap(paste0("Note:  There were ", nrow(points), " times since 1990 when VIX first surpassed 40.  ",
                               "Assumes that the VIX 'clears' its memory after dropping below ", vix_clearing_level, "."))

plot <- ggplot(to_plot, aes(x=date, y=index_sp500)) +
  geom_line() +
  geom_point(data=points, aes(x=date, y=index_sp500), col = "red") +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 and First Time VIX Surpassed 40")) +
  labs(x = "Date" , y = "Index Level",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  

fwd_ret_days <- c(60, 120, 251, 753, 1255, 2510)

for(i in 1:length(fwd_ret_days)){
  fwd <- fwd_ret_days[i]
  
  if(fwd > 251){
    exponent <- 251/fwd
  } else{
    exponent <- 1
  }
  
  tmp <- df %>%
    mutate(fwd_ret = (lead(index_sp500_tr, fwd)/index_sp500_tr)^(exponent) - 1) %>%
    filter(!is.na(fwd_ret)) %>%
    group_by(vix_bucket) %>%
    summarize(min_fwd_ret = min(fwd_ret),
              mean_fwd_ret = mean(fwd_ret),
              pct25_fwd_ret = quantile(fwd_ret, probs = 0.25),
              pct50_fwd_ret = quantile(fwd_ret, probs = 0.5),
              pct75_fwd_ret = quantile(fwd_ret, probs = 0.75),
              fwd_ret_days = fwd) %>%
    ungroup() %>%
    arrange(vix_bucket)
  
  if(i == 1){
    final_results <- tmp
  } else{
    final_results <- bind_rows(final_results, tmp)
  }
}

plot_vix_table <- function(summary_type, var_name){
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/vix_", tolower(summary_type), "_fwd_ret.jpeg")
  
  # Set note and source string
  source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                            width = 85)
  note_string <- str_wrap(paste0("Note:  All forward returns > 1 year are annualized and include dividends.  VIX data starts in 1990."))
  
  to_plot <- final_results %>%
              mutate(fwd_ret_bucket = case_when(
                fwd_ret_days == 60 ~ "3 months",
                fwd_ret_days == 120 ~ "6 months",
                fwd_ret_days == 251 ~ "1 year",
                fwd_ret_days == 753 ~ "3 years",
                fwd_ret_days == 1255 ~ "5 years",
                fwd_ret_days == 2510 ~ "10 years",
                TRUE ~ "Error"
              )) %>%
              rename_(.dots = setNames(paste0(var_name), "fwd_ret"))
  
  to_plot$fwd_ret_bucket <- factor(to_plot$fwd_ret_bucket, levels = c("3 months",
                                                            "6 months",
                                                            "1 year",
                                                            "3 years",
                                                            "5 years",
                                          "10 years"))
  
  # Create heatmap
  plot <- ggplot(to_plot, aes(x=fwd_ret_bucket, y=vix_bucket)) +
    geom_tile(aes(fill=fwd_ret)) +
    geom_text(aes(label = paste0(100*round(fwd_ret, 2), "%")), size = 2) +
    scale_fill_gradient(low = "red", high = "green", guide = FALSE, limits = c(-0.5, 0.4)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(summary_type, " Forward Return Based on VIX")) +
    labs(x = "Forward Period" , y = "VIX Range",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_vix_table("Worst", "min_fwd_ret")
plot_vix_table("Average", "mean_fwd_ret")
plot_vix_table("Median", "pct50_fwd_ret")


# ############################  End  ################################## #