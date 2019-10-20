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

folder_name <- "0149b_japan_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

jpy <- read.csv(paste0(importdir, "0114_japan_nikkei/nikk.csv"), skip = 1) %>%
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
      filtered[i, "DCA"] <- 100
      filtered[i, "Contributions"] <- 100
    } else{
      filtered[i, "DCA"] <- 100 + (filtered[(i-1), "DCA"] * (1+filtered[i, "ret"]))
      filtered[i, "Contributions"] <- filtered[(i-1), "Contributions"] + 100
    }
  }
  
  to_plot <- filtered %>%
              select(-ret) %>%
              gather(-date, key=key, value=value) %>%
              select(date, key, value)
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
  
  total_cont <- 100 * nrow(filtered)
  
  # Set the file_path
  file_path <- paste0(out_path, "/value_dca_", date_start_string, ".jpeg")
  
  first_year <- year(min(to_plot$date))
  last_year <- year(max(to_plot$date))
  
  n_year_cont <- min(n_years, last_year - first_year + 1)
  
  # Set note and source string
  source_string <- str_wrap("Source: YCharts (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Monthly contribution of $100 are plotted (dashed line) alongside the portfolio value (solid line).  ",
                                   "In total, $", formatC(total_cont, format="f", big.mark = ",", digits = 0), " are contributed across ", n_year_cont, " years."), 
                            width = 85)
  
  min_dt_decade <- as.Date(paste0(year(min(to_plot$date)) - year(min(to_plot$date)) %% 10, "-01-01", format = "%Y-%m-%d"))
  max_dt_decade <- as.Date(paste0(year(max(to_plot$date)) - year(max(to_plot$date)) %% 10, "-01-01", format = "%Y-%m-%d"))
  
  last_point <- filter(to_plot, dplyr::row_number() == nrow(filtered), key == "DCA")
  
  plot <- ggplot(to_plot, aes(x=date, y=value, linetype = key)) +
    geom_line() +
    scale_linetype_manual(guide = FALSE, values = c("dashed", "solid")) +
    scale_y_continuous(label = dollar) +
    scale_x_date(breaks = seq.Date(min_dt_decade, max_dt_decade, "10 years"), date_labels = "%Y") +
    geom_point(data = last_point, 
                    aes(x=date, y=value),
                    col = "red") +
    geom_text_repel(data = last_point, 
                    aes(x=date, y=value),
                    col = "red",
                    label = paste0("$",formatC(last_point$value, format="f", big.mark = ",", digits = 0)),
                    max.iter = 3000,
                    nudge_y = 25000,
                    nudge_x = -200,
                    segment.color = "transparent") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Dollar-Cost Averaging into Japan\n", first_year, "-", last_year)) +
    labs(x = "Date", y="Portfolio Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Create drawdown plot
  dd <- drawdown_path(select(filtered, date, DCA))
  
  # Set the file_path
  file_path <- paste0(out_path, "/dd_dca_", date_start_string, ".jpeg")
  
  plot <- ggplot(dd, aes(x = date, y = pct)) +
    geom_area(position = "identity", fill = "red") +
    scale_y_continuous(label = percent) +
    scale_x_date(breaks = seq.Date(min_dt_decade, max_dt_decade, "10 years"), date_labels = "%Y") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Drawdowns for Japanese DCA Strategy\n", first_year, "-", last_year)) +
    labs(x = "Date" , y = "Percentage of Value Lost",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_jpy_value("1980-01-01", 38)

# ############################  End  ################################## #