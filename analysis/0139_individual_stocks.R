cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(ggjoy)
library(tidyverse)

folder_name <- "0139_individual_stocks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read.csv(paste0(importdir, "0139_sp500_individual_stocks/ycharts_spx.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name,
         metric = Metric) %>%
  gather(-symbol, -name, -metric, key=key, value=value) %>%
  mutate(year = as.numeric(gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE))) %>%
  arrange(symbol, year) %>%
  filter(!is.na(value)) %>%
  mutate(spx_ret = value/lag(value) - 1) %>%
  filter(!is.na(spx_ret)) %>%
  select(year, spx_ret) 

raw <- read.csv(paste0(importdir, "0139_sp500_individual_stocks/ycharts_tr.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name,
         metric = Metric) %>%
  gather(-symbol, -name, -metric, key=key, value=value) %>%
  mutate(year = as.numeric(gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE))) %>%
  arrange(symbol, year) %>%
  filter(!is.na(value)) %>%
  mutate(ret = value/lag(value) - 1) %>%
  filter(!is.na(ret)) %>%
  left_join(spx) %>%
  mutate(above_market = ifelse(ret > spx_ret, 1, 0)) %>%
  select(year, symbol, name, ret, spx_ret, above_market) %>%
  filter(year < 2019)

n_years_data <- raw %>%
          group_by(symbol) %>%
          summarize(n_obs = n()) %>%
          ungroup() %>%
          select(symbol, n_obs)

full_data <- filter(n_years_data, n_obs == max(n_years_data$n_obs)) %>%
                inner_join(raw)

full_symbols <- full_data %>%
              select(symbol) %>%
              distinct()

# Simulation parameters
n_simulations <- 1000
portfolio_sizes <- c(5, 10, 20, 30, 50, 100, 200, 300)
set.seed(12345)

final_results <- data.frame(year = c(), 
                            mean_ret = c(),
                            binned_ret = c(),
                            simulation = c(),
                            portfolio_size = c(),
                            above_market = c())

for(p in portfolio_sizes){
  print(p)
  for(i in 1:n_simulations){
    s <- sample(full_symbols$symbol, p, replace = FALSE)
    
    tmp <- raw %>%
            filter(symbol %in% s) %>%
            group_by(year) %>%
            summarize(mean_ret = mean(ret),
                      spx_ret = mean(spx_ret)) %>%
            ungroup() %>%
            mutate(binned_ret = case_when(
              mean_ret > 0.5 ~ 0.5,
              mean_ret < -0.5 ~ -0.5,
              TRUE ~ mean_ret
            ),
                    simulation = i,
                   portfolio_size = p
                   )
    
    fnl <- tmp %>%
            summarize(p_ret = prod(1+mean_ret)^(1/nrow(tmp)) - 1,
                      spx_ret = prod(1+spx_ret)^(1/nrow(tmp)) - 1)
    
    tmp <- tmp %>%
              mutate(above_market = ifelse(fnl$p_ret > fnl$spx_ret, 1, 0),
                     annual_outperformance_full_period = fnl$p_ret - fnl$spx_ret)
    
    if(p == portfolio_sizes[1] & i == 1){
      final_results <- tmp
    } else{
      final_results <- bind_rows(final_results, tmp)
    }
  }
}

# Summarize above market stats
above_market_stats_year <- raw %>%
                              group_by(year) %>%
                              summarize(above_market = mean(above_market)) %>%
                              ungroup()

# Loop by start year
all_years <- unique(full_data$year)
above_market_stats_stock <- data.frame(start_year = c(), 
                                              market_outperformance_2018 = c())
above_market_stats_portfolio_size <- data.frame(start_year = c(), 
                                                portfolio_size = c(), 
                                                above_market = c(), 
                                                annual_outperformance = c())

for(y in all_years){
  print(y)
  ind <- full_data %>%
    filter(year >= y) %>%
    group_by(symbol) %>%
    summarize(p_ret = prod(1+ret)- 1,
              spx_ret = prod(1+spx_ret) - 1) %>%
    ungroup() %>%
    mutate(above_market = ifelse(p_ret>spx_ret, 1, 0)) %>%
    summarize(market_outperformance_2018 = mean(above_market)) %>%
    mutate(start_year = y) %>%
    select(start_year, market_outperformance_2018)
  
  n_years <- length(all_years) - which(all_years == y) + 1
  
  port <- final_results %>%
    filter(year >= y) %>%
    group_by(portfolio_size, simulation) %>%
    summarize(p_ret = prod(1+mean_ret)^(1/n_years) - 1,
              spx_ret = prod(1+spx_ret)^(1/n_years) - 1) %>%
    ungroup() %>%
    mutate(above_market = ifelse(p_ret>spx_ret, 1, 0),
           market_outperformance_2018 = p_ret - spx_ret,
           start_year = y) %>%
    group_by(start_year, portfolio_size) %>%
    summarize(above_market = mean(above_market),
              market_outperformance_2018 = mean(market_outperformance_2018)) %>%
    ungroup() %>%
    select(start_year, portfolio_size, above_market, market_outperformance_2018)
  
  if(y == all_years[1]){
    above_market_stats_stock <- ind
    above_market_stats_portfolio_size <- port
  } else{
    above_market_stats_stock <- bind_rows(above_market_stats_stock, ind)
    above_market_stats_portfolio_size <- bind_rows(above_market_stats_portfolio_size, port)
  }
}

overall_summary <- final_results %>%
                      group_by(year, portfolio_size) %>%
                      summarize(avg_ret = mean(mean_ret),
                                sd_ret = sd(mean_ret)) %>%
                      ungroup() %>%
                      left_join(spx)

# Plot by portfolio size
for(p in portfolio_sizes){
  p_string <- str_pad(p, 3, pad = "0")
  
  to_plot <- final_results %>%
                filter(portfolio_size == p)
  
  source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
  note_string   <- str_wrap(paste0("Note:  Stocks are selected from the S&P 500 and only include those with data for all years.  Returns shown include dividends."), 
                            width = 85)
  
  file_path <- paste0(out_path, "/dist_returns_portfolio_", p_string, "_stocks.jpeg")

  plot <- ggplot(data = to_plot, aes(x=binned_ret, y=as.factor(year))) +
    geom_joy_gradient(rel_min_height = 0.01, scale = 3, fill = "blue") +
    scale_x_continuous(label = percent, limit = c(-0.6, 0.6), breaks = seq(-0.6, 0.6, 0.2)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Return Distribution by Year\n", p, "-Stock Equal Weight Portfolio")) +
    labs(x = "1-Year Return", y = "Year",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do annual outperformance
  file_path <- paste0(out_path, "/outperf_portfolio_", p_string, "_stocks.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x=annual_outperformance_full_period)) +
    geom_density(fill = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(label = percent, limit = c(-0.2, 0.2)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Annual Outperformance Compared to S&P 500\n", p, "-Stock Equal Weight Portfolio")) +
    labs(x = "Annualized Return", y = "Frequency",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_gif(out_path,
           paste0("dist_returns_portfolio_*.jpeg"),
           100,
           0,
           paste0("_gif_dist_portfolio_size_returns.gif"))

create_gif(out_path,
           paste0("outperf_portfolio_*.jpeg"),
           100,
           0,
           paste0("_gif_outperf_portfolio_size_returns.gif"))


# ############################  End  ################################## #