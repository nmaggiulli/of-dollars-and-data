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
library(readxl)
library(tidyverse)

folder_name <- "xxxx_portfolio_comparisons"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0183_ycharts_portfolios/MVFIRX_SPY_GLD_TLT_data.csv"), 
                col.names = c("date", "index_gld", "index_sp500", "index_bond_st", "index_bond_lt")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

create_portfolio_data <- function(start_dt, end_dt,
   w_gld, w_sp500, w_bond_st, w_bond_lt, lbl){
  if(w_gld + w_bond_lt + w_bond_st + w_sp500 != 1){
    break
  }
  tmp <- raw %>%
          filter(date >= start_dt, date <= end_dt) %>%
          mutate(port = w_gld*index_gld + w_sp500*index_sp500 +
                   w_bond_st*index_bond_st + w_bond_lt*index_bond_lt,
                 label = lbl) %>%
          select(date, port, label)
  
  first_value <- tmp[1, "port"]
  
  tmp <- tmp %>%
            mutate(port = port/first_value)
  return(tmp)
}

plot_perf <- function(start_date, end_date){
  
  port_6040 <- create_portfolio_data(start_date, end_date, 0, 0.6, 0, 0.4, "60/40 Stock/Bond")
  port_8020 <- create_portfolio_data(start_date, end_date, 0, 0.8, 0, 0.2, "80/20 Stock/Bond")
  port_sp500 <- create_portfolio_data(start_date, end_date,0, 1, 0, 0, "S&P 500")
  port_perm <- create_portfolio_data(start_date, end_date, 0.25, 0.25, 0.25, 0.25, "Permanent Portfolio")
  
  to_plot <- port_6040 %>% bind_rows(port_8020, port_sp500, port_perm)
  
  legend_order <- to_plot %>%
    filter(date == max(to_plot$date)) %>%
    arrange(-port) %>%
    pull(label)
  
  to_plot$label <- factor(to_plot$label, levels = legend_order)
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  file_path <- paste0(out_path, "/compare_port_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=port, col = label)) +
    geom_line() +
    scale_color_discrete() +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "right") +
    ggtitle(paste0("Portfolio Performances\n",
                   format.Date(start_date, "%m/%d/%y"),
                   "-",
                   format.Date(end_date, "%m/%d/%y"))) +
    labs(x = "Date" , y = "Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  file_path <- paste0(out_path, "/zoom_port_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=port, col = label)) +
    geom_line() +
    scale_color_discrete() +
    scale_y_continuous(label = dollar) +
    scale_x_date(limits = c(as.Date("2019-12-31"), as.Date("2020-04-06"))) + 
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "right") +
    ggtitle(paste0("Portfolio Performances (2020 Only)\n",
                   format.Date(start_date, "%m/%d/%y"),
                   "-",
                   format.Date(end_date, "%m/%d/%y"))) +
    labs(x = "Date" , y = "Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do Drawdowns as well
  dd_6040 <- drawdown_path(select(port_6040, date, port)) %>%
                mutate(label = unique(port_6040$label))
  dd_8020 <- drawdown_path(select(port_8020, date, port)) %>%
    mutate(label = unique(port_8020$label))
  dd_sp500 <- drawdown_path(select(port_sp500, date, port)) %>%
    mutate(label = unique(port_sp500$label))
  dd_perm <- drawdown_path(select(port_perm, date, port)) %>%
    mutate(label = unique(port_perm$label))
  
  to_plot <- dd_6040 %>% bind_rows(dd_8020, dd_sp500, dd_perm)
  
  file_path <- paste0(out_path, "/dd_port_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- str_wrap(paste0("Source:  YCharts (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=pct, col = label)) +
    geom_line() +
    scale_color_discrete() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "right") +
    ggtitle(paste0("Portfolio Drawdowns\n",
                   format.Date(start_date, "%m/%d/%y"),
                   "-",
                   format.Date(end_date, "%m/%d/%y"))) +
    labs(x = "Date" , y = "Percentage of Value Lost",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_perf("2020-01-01", "2020-04-06")

plot_perf("2005-01-01", "2020-04-06")


# ############################  End  ################################## #