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
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(readxl)
library(tidyverse)

folder_name <- "_jkb/0010_crash_buying_opportunity"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")

# Percent recovery plot
percent_loss_gain <- data.frame(loss = seq(0.01, 0.5, 0.01)) %>%
                      mutate(gain = 1/(1-loss) - 1) 

file_path <- paste0(out_path, "/gain_needed_to_recover_loss.jpeg")

plot <- ggplot(percent_loss_gain, aes(x = loss, y = gain)) + 
  geom_smooth(se = FALSE, col = bw_colors[2]) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("% Gain Needed to Fully Recover From % Loss")) +
  labs(x = "Loss" , y = "Gain Needed to Recover")  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot annualized returns
shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
  select(date, price_plus_div) %>%
  filter(date >= "1920-01-01")

dd <- drawdown_path(shiller)

dd_num <- 1
for(i in 1:nrow(dd)){
  dd_curr <- dd[i, "pct"]
  
  dd[i, "dd_num"] <- dd_num
  
  if(dd_curr == 0){
    dd_num <- dd_num + 1
  }
}

dd_final <- dd %>%
  left_join(shiller)

dd_tops <- dd_final %>%
  filter(pct == 0) %>%
  group_by(dd_num) %>%
  summarise(recovery_price = max(price_plus_div),
            recovery_date = date) %>%
  ungroup()

dd_lengths <- dd %>%
  group_by(dd_num) %>%
  summarise(n_months = n(),
            min_dd = min(pct)) %>%
  ungroup() %>%
  filter(min_dd < -0.2) %>%
  left_join(dd_tops) %>%
  select(-n_months)

plot_dd_pct <- function(dd_pct){
  
  dd_w_recovery <- dd_final %>%
    inner_join(dd_lengths) %>%
    mutate(n_years = (interval(date, recovery_date) %/% months(1))/12,
           annualized_recovery_ret = (recovery_price/price_plus_div)^(1/n_years) - 1,
           recovery_bucket = case_when(
             annualized_recovery_ret < 0.05 ~ "0%-5%",
             annualized_recovery_ret < 0.1 ~ "5%-10%",
             annualized_recovery_ret < 0.15 ~ "10%-15%",
             annualized_recovery_ret < 0.2 ~ "15%-20%",
             annualized_recovery_ret < 0.25 ~ "20%-25%",
             annualized_recovery_ret < 0.3 ~ "25%-30%",
             TRUE ~ ">30%"
           )) %>%
    filter(n_years != 0, date >= "1900-01-01", pct < dd_pct)
  
  tmp <- dd_w_recovery %>%
    group_by(recovery_bucket) %>%
    summarise(pct = n()/nrow(dd_w_recovery)) %>%
    ungroup()
  
  recovery_buckets <- c("0%-5%", "5%-10%","10%-15%", "15%-20%", "20%-25%", "25%-30%", ">30%")
  
  to_plot <- data.frame(recovery_bucket = recovery_buckets) %>%
    full_join(tmp) %>%
    mutate(pct = ifelse(is.na(pct), 0, pct))
  
  
  to_plot$recovery_bucket <- factor(to_plot$recovery_bucket, levels = recovery_buckets)
  
  dd_string <- -100*dd_pct
  
  file_path <- paste0(out_path, "/dd_", dd_string, "_pct_rets.jpeg")
  
  # Plot the results
  plot <- ggplot(to_plot, aes(x = recovery_bucket, y = pct)) +
    geom_bar(stat="identity", fill = bw_colors[2]) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       limits = c(0, 0.5)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Annualized Returns After Buying\nDuring ", dd_string, "%+ Drawdown")) +
    labs(x = "Annualized Return" , y = "Frequency")
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

dd_pcts <- seq(-0.2, -0.50, -0.05)

for(d in dd_pcts){
  plot_dd_pct(d)
}

# Plot 
calc_dca_growth <- function(start_date, end_date){
  
  sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date>= start_date, date <= end_date) %>%
    select(date, price_plus_div)
  
  ending_val <- pull(sp500[nrow(sp500), "price_plus_div"])
  
  
  to_plot <- sp500 %>%
    mutate(dca_growth = ending_val/price_plus_div * 100) %>%
    select(date, dca_growth)
  
  return(to_plot)
}

to_plot <- calc_dca_growth("1929-09-01", "1936-11-01") %>%
          mutate(label = "Sep 1929-Nov 1936")

file_path <- paste0(out_path, "/dca_growth_1929.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=dca_growth)) + 
  geom_bar(stat = "identity", fill = "black", width = 31) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Growth of Each $100 Payment\ninto U.S. Stocks")) +
  labs(x = "Date" , y = "Final Amount")  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in JPY data
jpy <- read.csv(paste0(importdir,"/_jkb/0010_buying_during_a_crisis/NIKKEI225_fred.csv")) %>%
  mutate(date = as.Date(DATE),
         index_jpy = as.numeric(NIKKEI225),
         ret_jpy = index_jpy/lag(index_jpy, 1) - 1) %>%
  select(date, index_jpy, ret_jpy) %>%
  drop_na %>%
  filter(date >= "1980-01-01", date <= "2020-12-31") 

for(i in 1:nrow(jpy)){
  if(i == 1){
    jpy[i, "market_value"] <- 1
    jpy[i, "basis"] <- 1
  } else{
    jpy[i, "market_value"] <- jpy[(i-1), "market_value"] * (1 + jpy[i, "ret_jpy"]) + 1
    jpy[i, "basis"] <- jpy[(i-1), "basis"] + 1
  }
}

to_plot <- jpy
          
file_path <- paste0(out_path, "/jpy_1980_onward.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=index_jpy)) + 
  geom_line(col = bw_colors[2]) +
  scale_y_continuous(label = comma) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("The Japanese Stock Market Was\nBelow Its Highs For Over Three Decades")) +
  labs(x = "Date" , y = "Index Value")  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- to_plot %>%
            select(date, market_value, basis) %>%
            gather(-date, key=key, value=value) %>%
            mutate(key = case_when(
              key == "market_value" ~ "Market Value",
              TRUE ~ "Cost Basis"
            ))

# DCA into Japan
file_path <- paste0(out_path, "/jpy_1980_onward_dca.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
  geom_line() +
  scale_color_manual(values = bw_colors) +
  scale_y_continuous(label = dollar) + 
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Portfolio Value vs. Cost Basis\n$1 Per Day into Japanese Stocks")) +
  labs(x = "Date" , y = "Value")  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #