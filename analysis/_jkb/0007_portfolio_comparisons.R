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

folder_name <- "/_jkb/0007_portfolio_comparisons"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#cccccc", "#969696", "#525252", "#252525")
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
   w_gld, w_sp500, w_bond_st, w_bond_lt, lbl, clr, rebalance_limit){
  if(w_gld + w_bond_lt + w_bond_st + w_sp500 != 1){
    break
  }
  tmp <- df %>%
          filter(date >= start_dt, date <= end_dt) %>%
          mutate(label = lbl,
                 color = clr) 
  
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
            select(date, port, label, color)
  return(tmp)
}

plot_perf <- function(df, start_date, end_date, rebal_limit, rebal_period){
  
  port_6040 <- create_portfolio_data(df, start_date, end_date, 0, 0.6, 0, 0.4, "60/40 Stock/Bond", bw_colors[1], rebal_limit)
  port_8020 <- create_portfolio_data(df, start_date, end_date, 0, 0.8, 0, 0.2, "80/20 Stock/Bond", bw_colors[2], rebal_limit)
  port_sp500 <- create_portfolio_data(df, start_date, end_date,0, 1, 0, 0, "S&P 500", bw_colors[3], rebal_limit)
  
  all_ports <- port_6040 %>% bind_rows(port_8020, port_sp500)
  
  to_plot <- all_ports
  
  legend_order <- to_plot %>%
    filter(date == max(to_plot$date)) %>%
    arrange(-port) %>%
    pull(label)
  
  color_order <- to_plot %>%
    filter(date == max(to_plot$date)) %>%
    arrange(-port) %>%
    pull(color)
  
  to_plot$label <- factor(to_plot$label, levels = legend_order)
  
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  file_path <- paste0(out_path, "/compare_port_", start_date_string, "_", end_date_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=port, col = label)) +
    geom_line() +
    scale_color_manual(values = color_order) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("Portfolios with More Bonds Crash Less\n",
                   format.Date(start_date, "%m/%d/%y"),
                   "-",
                   format.Date(end_date, "%m/%d/%y"))) +
    labs(x = "Date" , y = "Growth of $1")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_perf(ycharts, "2020-01-01", "2020-04-28", 120, "sessions")


# ############################  End  ################################## #