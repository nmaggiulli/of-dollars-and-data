cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(tidyverse)

folder_name <- "0380_us_v_world"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0380_us_v_world/GrowthOfWealth_20240103114342.csv"),
                skip = 7, col.names = c("date", "index_world", "index_us")) %>%
            filter(!is.na(index_world)) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y"),
                   ret_world_10yr = (index_world/lag(index_world, 120))^(1/10) - 1,
                   ret_us_10yr = (index_us/lag(index_us, 120))^(1/10)  - 1,
                   ret_world_20yr = (index_world/lag(index_world, 240))^(1/20)  - 1,
                   ret_us_20yr = (index_us/lag(index_us, 240))^(1/20)  - 1,
                   world_win_10yr = ifelse(ret_world_10yr > ret_us_10yr, 1, 0),
                   world_win_20yr = ifelse(ret_world_20yr > ret_us_20yr, 1, 0),
                   diff_10yr = ret_us_10yr - ret_world_10yr,
                   diff_20yr = ret_us_20yr - ret_world_20yr
                   )

plot_years <- function(n_years, max_year){
  
  to_plot <- raw %>%
              select(date, contains("diff_"), contains("world_win_")) %>%
              rename(setNames(paste0("diff_", n_years, "yr"), "diff_col")) %>%
              filter(year(date) <= max_year,
                     !is.na(diff_col))
  
  if(n_years == 10){
    summary <- to_plot %>%
      summarise(world_win_10yr = mean(world_win_10yr, na.rm = TRUE),
                world_win_20yr = mean(world_win_20yr, na.rm = TRUE))
    
    print(summary)
  }
  
  y_max <- round_to_nearest(max(to_plot$diff_col), "up", 0.05)
  
  file_path <- paste0(out_path, "/us_v_world_outperformance_", n_years, "yr_", max_year, ".jpeg")
  source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap("Note: Returns shown include dividends but are not adjusted for inflation.",
                          width = 80)
  
  plot <- ggplot(data = to_plot, aes(x = date, y = diff_col)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    scale_y_continuous(label = percent_format(accuracy = 1), 
                       limits = c(-y_max, y_max), 
                       breaks = seq(-y_max,y_max, 0.05)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("U.S. Stock Outperformance (vs. World)\nOver ", n_years, " Years\n", year(min(raw$date)), "-", year(max(to_plot$date)))) +
    labs(x = paste0(n_years, " Years Ending"), y = "Annualized Outperformance",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_years(10, 2023)
plot_years(20, 2023)

plot_years(10, 2013)
plot_years(20, 2013)


# ############################  End  ################################## #