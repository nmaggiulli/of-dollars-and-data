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
library(tidylog)
library(zoo)
library(ggjoy)
library(tidyverse)

folder_name <- "0260_dip_buy_threshold"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
        filter(date >= "1920-01-01", date <= "2020-12-31") %>%
        select(date, price_plus_div)

dd <- drawdown_path(raw)

run_dip_buying <- function(start_date, end_date, dd_threshold){
  tmp <- raw %>%
          filter(date >= start_date,
                 date <= end_date) %>%
          left_join(dd)
  
  monthly_amount <- 100
  cash_saving <- 1
  
  for(i in 1:nrow(tmp)){
    if(i == 1){
      tmp[i, "value_dca"] <- monthly_amount
      tmp[i, "value_cash"] <- monthly_amount
      tmp[i, "value_dip"] <- 0
    } else{
      dd <- tmp[(i-1), "pct"]
      ret <- tmp[i, "price_plus_div"]/tmp[(i-1), "price_plus_div"] - 1
      
      if(dd < dd_threshold){
        cash_saving <- 0
        tmp[i, "value_cash"] <- 0
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret) + tmp[(i-1), "value_cash"] * (1 + ret) + monthly_amount
      } else if (dd == 0){
        cash_saving <- 1
        tmp[i, "value_cash"] <- tmp[(i-1), "value_cash"] + monthly_amount
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret)
      } else if (cash_saving == 1){
        tmp[i, "value_cash"] <- tmp[(i-1), "value_cash"] + monthly_amount
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret)
      } else if (cash_saving == 0){
        tmp[i, "value_cash"] <- 0
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret) + monthly_amount
      }
      tmp[i, "value_dca"] <- tmp[(i-1), "value_dca"] * (1 + ret) + monthly_amount
    }
  }
  return(tmp)
}

n_years <- 20

jan_only <- raw %>%
              filter(month(date) == 1)

start_dates <- pull(jan_only[1:(nrow(jan_only)- n_years + 1), "date"])
dd_thresholds <- seq(-0.1, -0.5, -0.1)

final_results <- data.frame()

counter <- 1
for(s in 1:length(start_dates)){
  start_dt <- start_dates[s]
  print(start_dt)
  end_dt <- as.Date(start_dt + months((12*n_years)-1))
  print(end_dt)
  
  for(d in dd_thresholds){
    fnl <- run_dip_buying(start_dt, end_dt, d)
    
    final_results[counter, "start_date"] <- start_dt
    final_results[counter, "end_date"] <- end_dt
    final_results[counter, "dd_threshold"] <- d
    final_results[counter, "final_dca"] <- fnl[nrow(fnl), "value_dca"]
    final_results[counter, "final_dip"] <- fnl[nrow(fnl), "value_dip"] + fnl[nrow(fnl), "value_cash"]
    counter <- counter + 1
  }
}

final_results <- final_results %>%
            mutate(btd_win = ifelse(final_dip > final_dca, 1, 0),
                   btd_outperf = final_dip/final_dca - 1,
                   dd_threshold = paste0(100*dd_threshold, "%"))

summary <- final_results %>%
            group_by(dd_threshold) %>%
            summarise(mean_btd_win = mean(btd_win),
                      median_btd_outperf = quantile(btd_outperf, probs = 0.5)) %>%
            ungroup()

to_plot <- final_results

file_path <- paste0(out_path, "/btd_outperf_by_dd.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x= btd_outperf, y=as.factor(dd_threshold), fill = as.factor(dd_threshold))) +
  geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_discrete(guide = "none") +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Buy the Dip Outperformance Over DCA\nby Dip Threshold")) +
  labs(x="Outperformance over DCA (%)", y="Dip Threshold",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #