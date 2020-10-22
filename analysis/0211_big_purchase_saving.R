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

folder_name <- "0211_big_purchase_saving"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Sim paramaters
monthly_payment <- 1000
required_savings <- monthly_payment*48

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
  select(date, price_plus_div) %>%
  mutate(ret = price_plus_div/lag(price_plus_div) - 1) %>%
  filter(date >= "1920-01-01")

dates_to_run <- shiller %>%
                  select(date) %>%
                  distinct() %>%
                  filter(month(date) %in% c(1, 4, 7, 10), year(date) < 2017) %>%
                  pull(date)

final_results <- data.frame()

counter <- 1
for(d in dates_to_run){
  print(as.Date(d, format = "%Y-%m-%d"))
  tmp_shiller <- shiller %>% filter(date >= d)
  
  current_savings <- 0
  month_counter <- 1
  
  while(current_savings < required_savings & month_counter < nrow(tmp_shiller)){
    ret <- tmp_shiller[month_counter, "ret"]
    
    if(current_savings == 0){
      current_savings <- monthly_payment * (1 + ret)
    } else{
      current_savings <- (current_savings + monthly_payment) * (1 + ret)
    }
    
    month_counter <- month_counter + 1
  }
  
  if(month_counter >= nrow(tmp_shiller)){
    month_counter <- -999
  }
  
  final_results[counter, "start_date"] <- as.Date(d, format = "%Y-%m-%d")
  final_results[counter, "n_months_to_goal"] <- month_counter
  
  counter <- counter + 1
}

to_plot <- final_results %>%
              mutate(time_bucket = case_when(
                n_months_to_goal <33 ~ "<32",
                n_months_to_goal <37 ~ "32-36",
                n_months_to_goal <41 ~ "37-40",
                n_months_to_goal <45 ~ "41-44",
                n_months_to_goal <49 ~ "45-48",
                n_months_to_goal <53 ~ "49-52",
                n_months_to_goal <57 ~ "53-56",
                n_months_to_goal <61 ~ "57-60",
                n_months_to_goal <65 ~ "61-64",
                TRUE ~ ">65"
              )) %>%
              group_by(time_bucket) %>%
              summarize(pct = n()/ nrow(final_results)) %>%
              ungroup()

to_plot$time_bucket <- factor(to_plot$time_bucket, levels = c("<32", "32-36",
                                                              "37-40", "41-44","45-48", "49-52",
                                                              "53-56", "57-60", "61-64", ">65"))

to_plot <- to_plot %>% 
            arrange(time_bucket)

for(i in 1:nrow(to_plot)){
  if(i == 1){
    to_plot[i, "pct_cumulative"] <- to_plot[i, "pct"]
  } else{
    to_plot[i, "pct_cumulative"] <- to_plot[(i-1), "pct_cumulative"] + to_plot[i, "pct"]
  }
}

file_path <- paste0(out_path, "/distribution_of_saving_times.jpeg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x = time_bucket, y = pct)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Distribution of Saving Times\nWhen Fully Invested in Stocks")) +
  labs(x = "Number of Months" , y = paste0("Percentage"),
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/cumulative_saving_times.jpeg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x = time_bucket, y = pct_cumulative)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Cumulative Distribution of Savings Times\nWhen Fully Invested in Stocks")) +
  labs(x = "Number of Months" , y = paste0("Cumulative Percentage"),
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #