cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(readxl)
library(ggjoy)
library(tidyverse)

folder_name <- "0285_dd_vs_future_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "0233_dow_rolling_90_day_watermark/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index_dow")) %>%
            select(date, index_dow)

dd <- drawdown_path(raw)

dd_counter <- 1
for(i in 1:nrow(dd)){
  if(i == 1){
    dd[i, "dd_counter"] <- dd_counter    
  } else{
    if(dd[i, "pct"] == 0){
      dd_counter <- dd_counter + 1
      dd[i, "dd_counter"] <- dd_counter
    } else{
      dd[i, "dd_counter"] <- dd_counter
    }
  }
}

df <- raw %>%
        left_join(dd) %>%
        mutate(ret_1yr = lead(index_dow, 250)/index_dow - 1,
               ret_3yr = lead(index_dow, 750)/index_dow - 1)

dd_levels <- seq(0, -0.4, -0.1)

final_results <- data.frame()
counter <- 1
for(dd_level in dd_levels){
  min_dd_dates <- df %>%
                    filter(pct <= dd_level) %>%
                    group_by(dd_counter) %>%
                    summarise(date = min(date)) %>%
                    ungroup() %>%
                    select(date)
  
  dd_mins <- df %>%
              inner_join(min_dd_dates) %>%
              mutate(dd_level = paste0(100*dd_level, "%"))
  
  assign(paste0("dd_mins_", -100*dd_level), dd_mins, envir = .GlobalEnv)
  
  dd_rets <- dd_mins %>%
              summarise(ret_1yr_median = quantile(ret_1yr, probs = 0.5, na.rm = TRUE),
                        ret_3yr_median = quantile(ret_3yr, probs = 0.5, na.rm = TRUE),
                        n_obs = n())
  
  final_results[counter, "dd_level"] <- dd_level
  final_results[counter, "n_obs"] <- dd_rets$n_obs
  final_results[counter, "ret_1yr_median"] <- dd_rets$ret_1yr_median
  final_results[counter, "ret_3yr_median"] <- dd_rets$ret_3yr_median
  
  counter <- counter + 1
}


to_plot <- dd_mins_0 %>%
              bind_rows(dd_mins_10, dd_mins_20, dd_mins_30, dd_mins_40) %>%
              select(dd_level, contains("ret_"))

to_plot$dd_level <- factor(to_plot$dd_level, levels = rev(paste0(100*dd_levels, "%")))

plot_ret <- function(name){
  
  to_plot_tmp <- to_plot %>%
              rename_(.dots = setNames(name, "ret"))
  
  if (name == "ret_1yr"){
    chart_string <- "1-Year" 
  } else if (name == "ret_3yr"){
    chart_string <- "3-Year" 
  }
  
  file_path <- paste0(out_path, "/dow_fut_ret_by_dd_", name, ".jpeg")
  source_string <- paste0("Source: Bloomberg (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Returns shown do not include dividends and are not adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(data = to_plot_tmp, aes(x=ret, y=dd_level, fill = dd_level)) +
    geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_fill_discrete(guide = FALSE) +
    scale_x_continuous(label = percent) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dow Jones Future ", chart_string, " Returns\nBased on Drawdown Threshold")) +
    labs(x = paste0("Future ", chart_string, " Return"), y = "Drawdown Threshold",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  #Do 0% and 10% plots
  x_min <- -0.5
  x_max <- 1
  
  to_plot_tmp2 <- to_plot_tmp %>%
                    filter(dd_level == "0%")
  
  file_path <- paste0(out_path, "/dow_0pct_dd_", name, ".jpeg")
  
  plot <- ggplot(data = to_plot_tmp2, aes(x=ret)) +
    geom_density(fill = chart_standard_color) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(label = percent, limits = c(x_min, x_max)) +
    of_dollars_and_data_theme +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("Dow Jones Future ", chart_string, " Returns\nFrom All-Time Highs")) +
    labs(x = paste0("Future ", chart_string, " Return"), y = "Frequency",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  to_plot_tmp3 <- to_plot_tmp %>%
    filter(dd_level == "-10%")
  
  file_path <- paste0(out_path, "/dow_10pct_dd_", name, ".jpeg")
  
  plot <- ggplot(data = to_plot_tmp3, aes(x=ret)) +
    geom_density(fill = chart_standard_color) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(label = percent, limits = c(x_min, x_max)) +
    of_dollars_and_data_theme +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("Dow Jones Future ", chart_string, " Returns\nAfter a 10% Drawdown")) +
    labs(x = paste0("Future ", chart_string, " Return"), y = "Frequency",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_ret("ret_1yr")
plot_ret("ret_3yr")

# ############################  End  ################################## #

  
