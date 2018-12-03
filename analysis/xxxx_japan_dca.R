cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(tools)
library(quantmod)
library(dplyr)

folder_name <- "xxxx_japan_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

jpy <- read.csv(paste0(importdir, "xxxx_japan_nikkei/nikk.csv"), skip = 1) %>%
        rename(date = Date,
               close = Close) %>%
        mutate(ret = ifelse(is.na(lag(close)), 0, close/lag(close) - 1),
               date = as.Date(date, "%m/%d/%Y")) %>%
        select(date, ret) 

plot_jpy_value <- function(date_start, n_years){
  
  date_start_string <- str_replace_all(date_start, "-", "_")
  
  filtered <- filter(jpy, date >= as.Date(date_start), date <= as.Date(date_start) + years(n_years))
  
  for (i in 1:nrow(filtered)){
    if(i == 1){
      filtered[i, "value"] <- 100
    } else{
      filtered[i, "value"] <- 100 + (filtered[(i-1), "value"] * (1+filtered[i, "ret"]))
    }
  }
  
  to_plot <- filtered %>%
              select(date, value)
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
  
  # Set the file_path
  file_path <- paste0(out_path, "/value_dca_", date_start_string, ".jpeg")
  
  # Set note and source string
  source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Assumes a monthly contribution of $100."), 
                            width = 85)
  
  first_year <- year(min(to_plot$date))
  last_year <- year(max(to_plot$date))
  
  plot <- ggplot(to_plot, aes(x=date, y=value)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dollar Cost Averaging into Japan\n", first_year, "-", last_year)) +
    labs(x = "Date", y="Portfolio Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Create drawdown plot
  dd <- drawdown_path(to_plot)
  
  # Set the file_path
  file_path <- paste0(out_path, "/dd_dca_", date_start_string, ".jpeg")
  
  plot <- ggplot(dd, aes(x = date, y = pct)) +
    geom_area(position = "identity", alpha = 0.4, fill = "red") +
    scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Drawdowns for Japanese DCA Strategy\n", first_year, "-", last_year)) +
    labs(x = "Date" , y = "Percentage of Value Lost",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_jpy_value("1970-01-01", 40)
plot_jpy_value("1980-01-01", 40)

# ############################  End  ################################## #