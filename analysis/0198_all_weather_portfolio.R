cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0198_all_weather_portfolio"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2024

raw <- read.csv(paste0(importdir, "0198_all_weather_historical/PeriodicReturns_20240701175447.csv"),
                skip = 6, col.names = c("date", "ret_bond_lt", "ret_bond_it", "ret_sp500", "ret_gld", "ret_cpi", "drop")) %>%
        select(-drop) %>%
        filter(date != "", ret_sp500 != "") %>%
        mutate(date = as.Date(date, format = "%m/%d/%Y"),
               ret_cpi = as.numeric(ret_cpi),
               ret_bond_lt = as.numeric(ret_bond_lt) - ret_cpi,
               ret_bond_it = as.numeric(ret_bond_it) - ret_cpi,
               ret_sp500 = as.numeric(ret_sp500) - ret_cpi,
               ret_gld = as.numeric(ret_gld) - ret_cpi,
               mt = month(date))

create_portfolio <- function(name, w_bond_lt, w_bond_it, w_sp500, w_gld){
  tmp <- raw
  
  assets <- c("bond_lt", "bond_it", "sp500", "gld")
  
  for(i in 1:nrow(tmp)){
    month <- tmp[i, "mt"]
    
    for(a in assets){
      if(a == "bond_lt"){
        weight <- w_bond_lt
      } else if (a == "bond_it"){
        weight <- w_bond_it
      } else if (a == "sp500"){
        weight <- w_sp500
      } else if (a == "gld"){
        weight <- w_gld
      } 
      
      val_name <- paste0("v_", a)
      ret_name <- paste0("ret_", a)
      
      if(i == 1){
        tmp[i, val_name] <- weight
      } else{
        if(month == 1){
          tmp[i, val_name] <- tmp[(i-1), "value_port"] * weight * (1 + tmp[i, ret_name])
        } else{
          tmp[i, val_name] <- tmp[(i-1), val_name] * (1 + tmp[i, ret_name])
        }
      }
    }
    v_cols   <- colnames(tmp)[grepl("v_", colnames(tmp))]
    tmp[i, "value_port"] <- sum(tmp[i, v_cols])
    tmp[i, "name"] <- name
    tmp[i, "decade"] <- year(floor_date(tmp[i, "date"], years(10)))
    if(i > 1){
      tmp[i, "ret_port"] <- tmp[i, "value_port"]/tmp[(i-1), "value_port"] - 1
    }
  }
  
  return(tmp %>% select(date, decade, value_port, ret_port, name))
}

all_weather <- create_portfolio("All Weather", 0.4, 0.15, 0.3, 0.15)
port_6040 <- create_portfolio("60/40 (Stock/Bond)", 0.6, 0, 0.4, 0)
port_sp500 <- create_portfolio("S&P 500", 0, 0, 1, 0)

calc_decade_rets <- function(df){
  months_per_decade <- df %>%
    filter(!is.na(ret_port)) %>%
    group_by(decade) %>%
    summarise(n_months = n()) %>%
    ungroup()
  
  tmp <- df %>%
    filter(!is.na(ret_port)) %>%
    left_join(months_per_decade) %>%
    mutate(ret_port = ret_port + 1) %>%
    group_by(decade, name, n_months) %>%
    summarise(cumulative_ret = (prod(ret_port))) %>%
    ungroup() %>%
    mutate(ret_annualized = cumulative_ret^(1/(n_months/12)) - 1) %>%
    select(decade, name, ret_annualized)
  
  return(tmp)
}

dec_aw <- calc_decade_rets(all_weather)
dec_6040 <- calc_decade_rets(port_6040)
dec_sp500 <- calc_decade_rets(port_sp500)

#Plot returns by decade
to_plot <- dec_aw %>%
            bind_rows(dec_6040) %>%
            bind_rows(dec_sp500) %>%
            filter(decade < 2020)

file_path <- paste0(out_path, "/all_weather_rl_rets_by_decade_", data_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0, 1973-2019 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: The 'All Weather Portfolio' uses a 15% allocation to Gold instead of a 7.5% allocation to Gold and a 7.5% allocation to Commodities.  Returns include dividends and have been adjusted for inflation."),
                        width = 85)

# Plot the results
plot <- ggplot(to_plot, aes(x = decade, y = ret_annualized, fill = name)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c("orange", "purple", "lightblue")) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(breaks = seq(1970, 2010, 10)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Real Annualized Returns by Decade")) +
  labs(x = "Decade" , y = "Annualized Real Return",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Plot growth of $1
file_path <- paste0(out_path, "/all_weather_rl_growth_of_dollar__", data_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0, 1973-", data_year, " (OfDollarsAndData.com)")

to_plot <- all_weather %>%
              bind_rows(port_6040) %>%
              bind_rows(port_sp500) %>%
            select(date, name, value_port)

last <- to_plot %>%
          filter(date == max(to_plot$date)) %>%
          mutate(label = paste0("$", round(value_port, 0)))

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = value_port, col = name)) +
  geom_line() +
  geom_text(data = last, aes(x=date, y=value_port, col=name, label=label), hjust = 0, show.legend = FALSE) +
  scale_color_manual(values = c("orange", "purple", "lightblue")) +
  scale_y_continuous(label = dollar) +
  scale_x_date(limits = c(min(to_plot$date), max(to_plot$date) + months(18))) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Real Growth of $1")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot DD
file_path <- paste0(out_path, "/all_weather_rl_dd__", data_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0, 1973-", data_year, " (OfDollarsAndData.com)")

to_plot <- drawdown_path(select(all_weather, date, value_port))

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = pct)) +
  geom_line(col = "purple") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Drawdowns for the All Weather Portfolio")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #