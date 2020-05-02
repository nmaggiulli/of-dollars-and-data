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

folder_name <- "0183_portfolio_comparisons"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

asset_list <- c("gld", "sp500", "bond_st", "bond_lt")
index_list <- paste0("index_", asset_list)

# Bring in YCharts recent daily data
ycharts <- read.csv(paste0(importdir, "0183_which_portfolio_for_you/MVFIRX_SPY_GLD_TLT_data.csv"), 
                col.names = c("date", index_list)) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

returns_2 <- read.csv(paste0(importdir, "0183_which_portfolio_for_you/Returns_20_GrowthOfWealth_20200501033235.csv"),
                      skip = 6, 
                      col.names = c("date", index_list)) %>%
              filter(index_gld != "", date != "") %>%
              mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Convert all character to numeric
returns_2[, 2:ncol(returns_2)] <- sapply( returns_2[, 2:ncol(returns_2)], as.numeric )

create_portfolio_data <- function(df, start_dt, end_dt,
   w_gld, w_sp500, w_bond_st, w_bond_lt, lbl, rebalance_limit){
  if(w_gld + w_bond_lt + w_bond_st + w_sp500 != 1){
    break
  }
  tmp <- df %>%
          filter(date >= start_dt, date <= end_dt) %>%
          mutate(label = lbl) 
  
  month_counter <- 1
  for(i in 1:nrow(tmp)){
    if(i == 1){
      for(a in asset_list){
       value_name <- paste0("value_", a)
       w_name <- paste0("w_", a)
       
       tmp[i, value_name] <- get(w_name)
      }
    } else{
      # If rebal
      if(month_counter == rebalance_limit){
        for(a in asset_list){
          value_name <- paste0("value_", a)
          index_name <-paste0("index_", a)
          w_name <- paste0("w_", a)
          
          tmp[i, value_name] <- tmp[(i-1), "port"] * get(w_name) * (tmp[i, index_name]/tmp[(i-1), index_name])
        }
        month_counter <- 0
      } else{
        for(a in asset_list){
          value_name <- paste0("value_", a)
          index_name <-paste0("index_", a)
          
          tmp[i, value_name] <- tmp[(i-1), value_name] * (tmp[i, index_name]/tmp[(i-1), index_name])
        }
      }
    }
    month_counter <- month_counter + 1
    tmp[i, "port"] <- tmp[i, "value_gld"] + tmp[i, "value_sp500"] + tmp[i, "value_bond_st"] + tmp[i, "value_bond_lt"]
  }
  
  first_value <- tmp[1, "port"]
  
  tmp <- tmp %>%
            mutate(port = port/first_value) %>%
            select(date, port, label)
  return(tmp)
}

plot_perf <- function(df, start_date, end_date, rebal_limit, rebal_period){
  
  if(substitute(df) == "ycharts"){
    source_name <- "YCharts"
  } else if (substitute(df) == "returns_2"){
    source_name <- "Returns 2.0"
  }
  
  port_perm <- create_portfolio_data(df, start_date, end_date, 0.25, 0.25, 0.25, 0.25, "Permanent Portfolio", rebal_limit)
  port_6040 <- create_portfolio_data(df, start_date, end_date, 0, 0.6, 0, 0.4, "60/40 Stock/Bond", rebal_limit)
  port_8020 <- create_portfolio_data(df, start_date, end_date, 0, 0.8, 0, 0.2, "80/20 Stock/Bond", rebal_limit)
  port_sp500 <- create_portfolio_data(df, start_date, end_date,0, 1, 0, 0, "S&P 500", rebal_limit)

  
  all_ports <- port_perm %>% bind_rows(port_6040, port_8020, port_sp500)
  
  to_plot <- all_ports
  
  legend_order <- to_plot %>%
    filter(date == max(to_plot$date)) %>%
    arrange(-port) %>%
    pull(label)
  
  to_plot$label <- factor(to_plot$label, levels = legend_order)
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  file_path <- paste0(out_path, "/compare_port_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- str_wrap(paste0("Source:  ", source_name," (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation.  ",
                                 "Portfolios are rebalanced every ", rebal_limit, " ", rebal_period, "."),
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
  
  if(source_name == "Returns 2.0"){
    years_list <- c(1, 5, 10, 20)
    
    for(y in years_list){
      n_months <- y * 12
      
      to_plot <- all_ports %>%
        mutate(port = ifelse(lead(label, n_months) == label,
                 (lead(port, n_months)/port)^(1/y) - 1,
                  NA)) %>%
                  filter(!is.na(port))
      
      to_plot$label <- factor(to_plot$label, levels = c("Permanent Portfolio",
                                                        "60/40 Stock/Bond",
                                                        "80/20 Stock/Bond",
                                                        "S&P 500"))
      
      y_string <- str_pad(y, side = "left", width = 2, pad = "0")
      
      n_periods <- nrow(to_plot)
      
      file_path <- paste0(out_path, "/boxplot_", y_string, "_", start_date_string, "_", end_date_string, ".jpeg")
      source_string <- str_wrap(paste0("Source:  ", source_name," (OfDollarsAndData.com)"),
                                width = 85)
      note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation.  ",
                                     "The data contain ",formatC(n_periods, format = "f",
                                                                  digits = 0, big.mark = ","), " overlapping ", y, "-year periods.  ",
                                     "Portfolios are rebalanced every ", rebal_limit, " ", rebal_period, "."),
                              width = 85)
      
      plot <- ggplot(to_plot, aes(x=label, y=port)) +
        geom_boxplot() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_y_continuous(label = percent_format(accuracy = 1)) +
        of_dollars_and_data_theme +
        ggtitle(paste0(y, "-Year Portfolio Performances\n",
                       format.Date(start_date, "%m/%d/%y"),
                       "-",
                       format.Date(end_date, "%m/%d/%y"))) +
        labs(x = "Portfolio" , y = "Annualized Return",
             caption = paste0("\n", source_string, "\n", note_string))
      
      # Save the plot
      ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    }
  }
  
  if(source_name == "YCharts"){
    file_path <- paste0(out_path, "/zoom_port_", start_date_string, "_", end_date_string, ".jpeg")
    source_string <- str_wrap(paste0("Source:  ", source_name," (OfDollarsAndData.com)"),
                              width = 85)
    note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation.  ",
                                   "Portfolios are rebalanced every ", rebal_limit, " ", rebal_period, "."),
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
  
  # Do Drawdowns as well
  dd_6040 <- drawdown_path(select(port_6040, date, port)) %>%
                mutate(label = unique(port_6040$label))
  dd_sp500 <- drawdown_path(select(port_sp500, date, port)) %>%
    mutate(label = unique(port_sp500$label))
  dd_perm <- drawdown_path(select(port_perm, date, port)) %>%
    mutate(label = unique(port_perm$label))
  
  to_plot <- dd_sp500 %>% bind_rows(dd_6040, dd_perm)
  
  file_path <- paste0(out_path, "/dd_port_", start_date_string, "_", end_date_string, ".jpeg")
  source_string <- str_wrap(paste0("Source:  ", source_name," (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends, but not adjusted for inflation.  ",
                                 "Portfolios are rebalanced every ", rebal_limit, " ", rebal_period, "."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=pct, fill = label)) +
    geom_area(position = "identity", alpha = 0.4) +
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

plot_perf(ycharts, "2020-01-01", "2020-04-28", 120, "session")
plot_perf(ycharts, "2005-01-01", "2020-04-28", 120, "sessions")
plot_perf(returns_2, "1970-01-31", "2020-03-31", 6, "months")


# ############################  End  ################################## #