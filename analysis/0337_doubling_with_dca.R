cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(xtable)
library(tidyverse)

folder_name <- "0337_doubling_with_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Do some data analysis to establish a long-term growth rate
raw_1970 <- read.csv(paste0(importdir, "/0337_doubling_with_dca/GrowthOfWealth_20230203114912.csv"),
                     skip = 7, 
                     row.names = NULL,
                     col.names = c("date", "index_sp500",	"index_world", "cpi", "index_bond"))  %>%
  filter(!is.na(index_sp500)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(date = as.Date(paste0(year(date), "-", month(date), "-01"), format = "%Y-%m-%d")) %>%
  gather(-date, key=key, value=value) 

first_values <- raw_1970 %>%
  filter(date == min(raw_1970$date)) %>%
  select(-date) %>%
  rename(first_value = value)

df <- raw_1970 %>%
  left_join(first_values) %>%
  mutate(value = value/first_value) %>%
  select(-first_value) %>%
  spread(key = key, value = value) %>%
  mutate(index_sp500_real = index_sp500/cpi,
         index_world_real = index_world/cpi,
         index_bond_real = index_bond/cpi) %>%
  select(date, contains("_real"))

w_stock <- 0.8
w_bond <- 1 - w_stock

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "value_world"] <- w_stock
    df[i, "value_bond"] <- w_bond
  } else{
    ret_world <- df[i, "index_world_real"]/df[(i-1), "index_world_real"] - 1
    ret_bond <- df[i, "index_bond_real"]/df[(i-1), "index_bond_real"] - 1
    mt <- month(df[i, "date"])
    
    if(mt == 1){
      df[i, "value_world"] <- df[(i-1), "value_port"] * w_stock * (1 + ret_world)
      df[i, "value_bond"] <- df[(i-1), "value_port"] * w_bond * (1 + ret_bond)
    } else{
      df[i, "value_world"] <- df[(i-1), "value_world"] * (1 + ret_world)
      df[i, "value_bond"] <- df[(i-1), "value_bond"] * (1 + ret_bond)
    }
    
    df[i, "value_port"] <- df[i, "value_world"] + df[i, "value_bond"]
  }
}

df <- df %>%
        mutate(ret_10yr = (value_port/lag(value_port, 120))^(1/10) - 1,
               ret_20yr = (value_port/lag(value_port, 240))^(1/20) - 1,
               ret_30yr = (value_port/lag(value_port, 360))^(1/30) - 1)

summary <- df %>%
              summarise(median_10yr_ret = quantile(ret_10yr, probs = 0.5, na.rm = TRUE),
                        median_20yr_ret = quantile(ret_20yr, probs = 0.5, na.rm = TRUE),
                        median_30yr_ret = quantile(ret_30yr, probs = 0.5, na.rm = TRUE))

# After reviewing summary, I will chose a 5% real growth rate for an 80/20 portfolio.

calculate_doubling_time <- function(annual_ret, pmt_percent){
  current_amount <- 100000
  final_amount <- 2*current_amount
  dca_pmt <- current_amount*pmt_percent
  
  monthly_ret <- (1+ annual_ret)^(1/12) - 1
  monthly_pmt <- dca_pmt/12
  n_months <- 0
  
  while(current_amount < final_amount){
    current_amount <- current_amount * (1 + monthly_ret) + monthly_pmt
    n_months <- n_months + 1
  }
  
  return(n_months/12)
}

all_pmt_pcts <- c(seq(0, 0.1, 0.01), 0.15, 0.2, 0.3)
all_rets <- seq(0.05, 0.07, 0.01)

final_results <- data.frame()
counter <- 1
for(i in 1:length(all_pmt_pcts)){
  pct <- all_pmt_pcts[i]
  
  for(r in all_rets){
    final_results[counter, "pmt_pct"] <- 100*pct
    final_results[counter, "ret_pct"] <- paste0(100*r, "% Real Return")
    final_results[counter, "doubling_years"] <- calculate_doubling_time(r, pct)
    counter <- counter + 1
  }
}

final_results <- final_results %>%
                  arrange(ret_pct, pmt_pct)
  

file_path <- paste0(out_path, "/doubling_time_by_wsr_returns.jpeg")
source_string <- str_wrap(paste0("Source: Simulated data (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Wealth savings rate is your expected annual savings amount over your starting assets. ",
                               "Simulation assumes that the annual savings amount does not change over time."),
                          width = 85)

plot <- ggplot(final_results, aes(x=pmt_pct, y=doubling_years, col = as.factor(ret_pct))) +
  geom_line() +
  scale_color_discrete() +
  scale_y_continuous(breaks = seq(0, 16, 2), limits = c(0, 16)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Number of Years to Double Your Money\nBased on Wealth Savings Rate and Annual Return")) +
  labs(x = "Wealth Savings Rate (%)" , y = "Doubling Time (in Years)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Create wide table for article
wide <- final_results %>%
        mutate(doubling_years = paste0(round(doubling_years, 1))) %>%
        spread(key=ret_pct, value = doubling_years) %>%
          mutate(`Wealth Savings Rate` = paste0(pmt_pct, "%")) %>%
          select(`Wealth Savings Rate`, contains("Real"))

print(xtable(wide), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/doubling_time_by_wsr.html"))

# ############################  End  ################################## #