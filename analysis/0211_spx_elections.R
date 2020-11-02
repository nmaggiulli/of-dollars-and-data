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

folder_name <- "0211_spx_elections"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

election_dates <- read_excel(paste0(importdir, "0211_spx_elections/election_dates_since_1950.xlsx")) %>%
                    mutate(election_day = 1)

election_years <- seq(1952, 2020, 4)

spx <- read.csv(paste0(importdir, "0211_spx_elections/SPX_data.csv"),
                col.names = c("date","index_sp500")) %>%
  mutate(date = as.Date(date),
         yr = year(date),
         mt = month(date),
         dy = day(date)) %>%
  arrange(date) %>%
  left_join(election_dates) %>%
  mutate(
    ret = index_sp500/lag(index_sp500) - 1,
    ret_5day = index_sp500/lag(index_sp500, 5) - 1,
  ) %>%
  filter(!is.na(ret)) %>%
  mutate(sd_5 = rollapply(ret, 5, FUN = sd, align = "right", fill = NA),
         sd_20 = rollapply(ret, 20, FUN = sd, align = "right", fill = NA),
         election_day = ifelse(is.na(election_day), 0, 1),
         election_year = ifelse(year(date) %in% election_years, "Election Year", "Non-Election Year"),
         presidential_election_day = ifelse(election_day == 1 & election_year == "Election Year", 1, 0),
         election_day_lag3 = ifelse(lag(election_day, 3) == 1, 1, 0),
         presidential_election_day_lag3 = ifelse(election_day_lag3 == 1 & election_year == "Election Year", 1, 0))

for(y in year(election_dates$date)){
  
  tmp <- spx %>%
    filter(year(date) == y)
  
  if(y != 2020){
    eday <- election_dates %>% filter(year(date) == y) %>% pull(date)
    eday_row <- which(tmp$date == eday)
  } else{
    eday_row <- 216
  }

  for(i in 1:nrow(tmp)){
    tmp[i, "days_until"] <- i - eday_row
  }
  
  if(y == min(year(spx$date))){
    stacked <- tmp
  } else{
    stacked <- stacked %>% bind_rows(tmp)
  }
}

date_start <- -160
date_end <- 31
low_y <- 0.006
high_y <- 0.012 

plot_vol_chart <- function(name, filter_string, title, var){
  to_plot <- stacked %>%
    filter_(filter_string) %>%
    group_by(days_until, election_year) %>%
    summarize(mean_sd_20 = mean(sd_20, na.rm = TRUE),
              median_ret_5day = quantile(ret_5day, na.rm = TRUE, probs = 0.5),
              median_sd_20 = quantile(sd_20, na.rm = TRUE, probs = 0.5),
              median_sd_5 = quantile(sd_5, na.rm = TRUE, probs = 0.5),
              n_years = n()) %>%
    ungroup() %>%
    filter(days_until > date_start,
           days_until < date_end)
  
  source_string <- "Source:  YCharts"
  
  if(var == "sd"){
    
  file_path <- paste0(out_path, "/spx_volatility_election_year_", name, ".jpeg")

  plot <- ggplot(to_plot, aes(x=days_until, y=mean_sd_20, col = election_year)) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(label = percent_format(accuracy = 0.01), limits = c(low_y, high_y)) +
    scale_color_manual(values = c("blue", "grey")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("S&P 500 30-Day Average Standard Deviation\nin Election and Non-Election Years\n", title)) +
    labs(x = "Trading Days Before/After Election" , y = "30-Day Standard Deviation",
         caption = paste0("\n", source_string))
  
  } else if(var == "ret_5day"){
    file_path <- paste0(out_path, "/spx_ret5day_election_year_", name, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x=days_until, y=median_ret_5day, col = election_year)) +
      geom_line() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_y_continuous(label = percent_format(accuracy = 0.01)) +
      scale_color_manual(values = c("blue", "grey")) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("S&P 500 7-Day Median Return\nin Election and Non-Election Years\n", title)) +
      labs(x = "Trading Days Before/After Election" , y = "7-Day Median Return",
           caption = paste0("\n", source_string))
  } else if(var == "median_sd_5"){
    file_path <- paste0(out_path, "/spx_median_vol5_election_year_", name, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x=days_until, y=median_sd_5, col = election_year)) +
      geom_line() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_y_continuous(label = percent_format(accuracy = 0.01)) +
      scale_color_manual(values = c("blue", "grey")) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("S&P 500 7-Day Median Standard Deviation\nin Election and Non-Election Years\n", title)) +
      labs(x = "Trading Days Before/After Election" , y = "Median Standard Deviation",
           caption = paste0("\n", source_string))
  } else if(var == "median_sd_20"){
    file_path <- paste0(out_path, "/spx_median_vol20_election_year_", name, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x=days_until, y=median_sd_20, col = election_year)) +
      geom_line() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_y_continuous(label = percent_format(accuracy = 0.01)) +
      scale_color_manual(values = c("blue", "grey")) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("S&P 500 30-Day Median Standard Deviation\nin Election and Non-Election Years\n", title)) +
      labs(x = "Trading Days Before/After Election" , y = "Median Standard Deviation",
           caption = paste0("\n", source_string))
  }
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

vars <- c("sd", "ret_5day", "median_sd_5", "median_sd_20")

for(v in vars){
  plot_vol_chart("pre2020", "year(date) < 2020", "1950-2019", v)
  plot_vol_chart("2020", "year(date) < 2021", "With 2020 included", v) 
  plot_vol_chart("no_1987", "year(date) != 1987", "Excluding 1987", v) 
}


to_plot <- stacked %>%
            filter(days_until >= 0,
                   days_until <= 5) %>%
            group_by(days_until, election_year) %>%
            summarize(mean_sd_5 = quantile(sd_5, na.rm = TRUE, probs = 0.5)) %>%
            ungroup()

file_path <- paste0(out_path, "/spx_vol_median_5day_pre_post.jpeg")
source_string <- "Source:  YCharts"

plot <- ggplot(to_plot, aes(x=days_until, y=mean_sd_5, col = election_year)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 0.01)) +
  scale_color_manual(values = c("blue", "grey")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 7-Day Median Standard Deviation\nin Election and Non-Election Years")) +
  labs(x = "Trading Days Since Election Day" , y = "7-Day Standard Deviation",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do some t-tests
t.test(spx$sd_20~spx$election_day)
spx_no1987 <- spx %>% filter(year(date) != 1987)
t.test(spx_no1987$sd_20~spx_no1987$presidential_election_day)

# Test rets
t.test(spx$ret_5day~spx$presidential_election_day)

# Test 7-day vol 3 days after the election
t.test(spx$sd_5~spx$presidential_election_day_lag3)


# ############################  End  ################################## #