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

folder_name <- "0178_portfolio_comparisons"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0178_ycharts_portfolios/SPTS_SPTI_SPY_GLD_BC_TLT_data.csv"), 
                col.names = c("date","index_com", "index_gld", "index_bond_it",
                              "index_bond_lt", "index_bond_st", "index_sp500")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

create_portfolio_data <- function(start_dt, end_dt,
  w_com, w_gld, w_bond_it, w_bond_lt, w_bond_st, w_sp500, lbl){
  if(w_com + w_gld + w_bond_it + w_bond_lt + w_bond_st + w_sp500 != 1){
    break
  }
  tmp <- raw %>%
          filter(date >= start_dt, date <= end_dt) %>%
          mutate(port = w_com*index_com + w_gld*index_gld + w_bond_it*index_bond_it +
                   w_bond_st*index_bond_st + w_sp500*index_sp500 + w_bond_lt*index_bond_lt,
                 label = lbl) %>%
          select(date, port, label)
  
  first_value <- tmp[1, "port"]
  
  tmp <- tmp %>%
            mutate(port = port/first_value)
  return(tmp)
}

plot_perf <- function(start_date, end_date){
  port_6040 <- create_portfolio_data(start_date, end_date, 0, 0, 0, 0.4, 0, 0.6, "60/40 Stock/Bond")
  port_sp500 <- create_portfolio_data(start_date, end_date,0, 0, 0, 0, 0, 1, "S&P 500")
  port_perm <- create_portfolio_data(start_date, end_date,0, 0.25, 0, 0.25, 0.25, 0.25, "Permanent Portfolio")
  port_allw <- create_portfolio_data(start_date, end_date, 0.075, 0.075, 0.15, 0.4, 0, 0.3, "All Weather Portfolio")
  
  to_plot <- port_6040 %>% bind_rows(port_sp500, port_perm, port_allw)
  
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
}

plot_perf("2020-01-01", "2020-04-06")

plot_perf("2012-01-01", "2020-04-06")

plot_perf("2016-01-01", "2020-04-06")

# ############################  End  ################################## #