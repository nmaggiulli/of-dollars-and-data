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
library(tidyverse)

folder_name <- "0190_ycharts_ind_stock_compare"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Set seed for randomization
set.seed(12345)
n_sims <- 1000

raw <- readRDS(paste0(localdir, "0190_russell_3000_stocks_ycharts.Rds"))

stocks_full_data <- raw %>%
                group_by(symbol) %>%
                summarise(n_dates = n()) %>%
                ungroup()

stocks_full_data <- stocks_full_data %>%
                      filter(n_dates == max(stocks_full_data$n_dates)) %>%
                      select(symbol)

raw_index <- read.csv(paste0(importdir, "0190_ycharts_russell_3000_ind/timeseries_6-13-2020_russell3000.csv"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  arrange(symbol, date) %>%
  select(date, symbol, value) %>%
  filter(!is.na(value))

first_value_russell <- raw_index %>%
                          filter(date == min(raw$date)) %>%
                          pull(value)

df_index <- raw_index %>%
              mutate(index_russell = value/first_value_russell) %>%
              select(date, index_russell)

all_stocks <- raw %>%
                inner_join(stocks_full_data) %>%
                filter(date == min(raw$date)) %>%
                arrange(symbol) %>%
                mutate(stock_num = row_number()) %>%
                rename(first_value = value) %>%
                select(-date)

all_stock_nums <- unique(all_stocks$stock_num)

df <- raw %>%
        inner_join(all_stocks) %>%
        mutate(value = value/first_value) %>%
        select(-first_value)

n_stocks_list <- c(2)

stock_sims <- data.frame(n_stock = c(), simulation = c())
stock_sim_counter <- 1
for(n_stocks in n_stocks_list){
  print(n_stocks)
  for(i in 1:n_sims){
    sim_stocks <- sort(sample(all_stock_nums, n_stocks, replace=TRUE))
    
    stock_sims[stock_sim_counter, "n_stock"] <- n_stocks
    stock_sims[stock_sim_counter, "simulation"] <- i
    stock_sims[stock_sim_counter, "sim_stocks"] <- paste(sim_stocks, collapse = ";")
    stock_sim_counter <- stock_sim_counter + 1
  }
}

stock_sims[1001, "n_stock"] <- 2
stock_sims[1001, "simulation"] <- 1001
stock_sims[1001, "sim_stocks"] <- "733;1383"


for(n_stocks in n_stocks_list){
  print(paste0("new number of stocks = ", n_stocks))
  for(i in 1:nrow(stock_sims)){
    if(i %% 50 == 0){
      print(i)
    }
    
    sim_stocks <- as.numeric(unlist(strsplit(filter(stock_sims, n_stock == n_stocks & simulation == i) %>% select(sim_stocks) %>% as.character(), split = ";")))
    
    rets <- df %>%
              filter(stock_num %in% sim_stocks) %>%
              group_by(date) %>%
              summarise(port = sum(value)/n_stocks,
                        sim = i) %>%
              ungroup()
    
    if(i == 1){
      final_results <- rets
    } else{
      final_results <- final_results %>% bind_rows(rets)
    }
  }
}

to_plot <- final_results %>%
                    left_join(df_index) %>%
                    mutate(net_ret = (index_russell/port)^(1/5) - 1)

file_path <- paste0(out_path, "/simulations_russell3000.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note:  Includes dividends but not adjusted for inflation.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=date, y=net_ret, col = as.factor(sim))) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_color_discrete(guide = FALSE) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Russell 3000 Annualized Outperformance vs.\nSimulated ", n_stocks, "-Stock Portfolios")) +
  labs(x="Date", y="Russell Annualized Outperformance",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

final_date <- to_plot %>%
  filter(date == max(to_plot$date)) %>%
  rename(simulation = sim) %>%
  left_join(stock_sims)

pct_50 <- quantile(final_date$net_ret, probs = 0.5)
pct_75 <- quantile(final_date$net_ret, probs = 0.75)

file_path <- paste0(out_path, "/outperf_dist_final.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note:  Includes dividends but not adjusted for inflation.")
                         , width = 85)

plot <- ggplot(final_date, aes(x=net_ret)) +
  geom_density(fill = chart_standard_color, col = chart_standard_color) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle(paste0("Russell 3000 Annualized Outperformance vs.\nSimulated ", n_stocks, "-Stock Portfolios")) +
  labs(x="Russell Annualized Outperformance", y="Density",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #