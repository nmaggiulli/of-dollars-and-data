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
library(survey)
library(lemon)
library(mitools)
library(zoo)
library(Hmisc)
library(tidyverse)

folder_name <- "0167_fed_cut_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

fed_cuts <- read_excel(paste0(importdir, "0167_fed_rate_cuts/fed_rate_cuts_since_1994.xlsx")) %>%
              mutate(date = as.Date(date),
                     cut = case_when(rate < lag(rate) ~ 1, 
                                     rate > lag(rate) ~ 0,
                                     TRUE ~ 0)) %>%
              select(date, cut, emergency_cut)

real_gdp <- read_excel(paste0(importdir, "0167_fed_rate_cuts/real_gdp_fred_1994.xls"), skip = 10) %>%
              mutate(date_qtr = as.Date(observation_date)) %>%
              rename(real_gdp = GDPC1) %>%
              mutate(real_gdp_change = real_gdp/lag(real_gdp) - 1) %>%
              select(date_qtr, real_gdp, real_gdp_change)

first_year_fred <- year(min(real_gdp$date_qtr))
last_year_fred <- year(max(real_gdp$date_qtr))

spx <- read.csv(paste0(importdir, "/0167_fed_rate_cuts/SPX_data.csv"),
                       col.names = c("date","index_sp500")) %>%
  mutate(date = as.Date(date),
         date_qtr = as.Date(as.yearqtr(date) - 1/4, frac = 1) + days(1)) %>%
  arrange(date) %>%
  left_join(fed_cuts) %>%
  left_join(real_gdp) %>%
  filter(date >= "1994-01-01")

dd <- drawdown_path(select(spx, date, index_sp500)) 

gdp_dd <- drawdown_path(select(spx, date, real_gdp) %>% filter(!is.na(real_gdp))) %>%
            rename(gdp_dd = pct)

spx <- spx %>%
        left_join(dd) %>%
        left_join(gdp_dd)

first_year <- year(min(spx$date))
last_year <- year(max(spx$date))

run_bar_plot <- function(n_session_fwd, cut, lower_limit, upper_limit){
  df <- spx %>%
          mutate(ret = lead(index_sp500, n_session_fwd)/index_sp500 - 1,
                 gdp_growth = lead(real_gdp, n_session_fwd)/real_gdp - 1) %>%
          filter(!is.na(ret))
  
  avg_all <- df %>%
              summarize(median_ret = quantile(ret, probs = 0.5),
                        median_gdp = quantile(gdp_growth, probs = 0.5, na.rm = TRUE)) %>%
              mutate(key = "All Days")
  
  if(cut == 0){
    cut_title <- "Following Fed Rate Change"
    filtered_fed <- df %>%
      filter(!is.na(emergency_cut))
    
    avg_fed <- filtered_fed %>%
      summarize(median_ret = quantile(ret, probs = 0.5),
                median_gdp = quantile(gdp_growth, probs = 0.5, na.rm = TRUE)) %>%
      mutate(key = cut_title)
    
    note_add <- paste0(nrow(filtered_fed), " changes in the federal funds rate since 1994.")
  } else if(cut == 1){
    cut_title <- "Following Fed Cut"
    filtered_fed <- df %>%
                    filter(cut == 1)
    
    avg_fed <- filtered_fed %>%
      summarize(median_ret = quantile(ret, probs = 0.5),
                median_gdp = quantile(gdp_growth, probs = 0.5, na.rm = TRUE)) %>%
      mutate(key = cut_title)
    
    note_add <- paste0(nrow(filtered_fed), " rate cuts to the federal funds rate since 1994.")
  } else if(cut == 2){
    cut_title <- "Following Fed Emergency Cut"
    filtered_fed <- df %>%
      filter(emergency_cut == 1)
    
    avg_fed <- filtered_fed %>%
      summarize(median_ret = quantile(ret, probs = 0.5),
                median_gdp = quantile(gdp_growth, probs = 0.5, na.rm = TRUE)) %>%
      mutate(key = cut_title)
    
    note_add <- paste0(nrow(filtered_fed), " emergency rate cuts to the federal funds rate since 1994.")
  } else if(cut == 3){
    cut_title <- "Following Fed Rate Increase"
    filtered_fed <- df %>%
      filter(cut == 0)
    
    avg_fed <- filtered_fed %>%
      summarize(median_ret = quantile(ret, probs = 0.5),
                median_gdp = quantile(gdp_growth, probs = 0.5, na.rm = TRUE)) %>%
      mutate(key = cut_title)
    
    note_add <- paste0(nrow(filtered_fed), " rate increases to the federal funds rate since 1994.")
  }
  
  to_plot <- avg_all %>%
                bind_rows(avg_fed)
  
  n_session_fwd_string <- str_pad(n_session_fwd, side = "left", width=3, pad="0")
  
  # Plot bar averages
  file_path <- paste0(out_path, "/avg_vs_fed_", cut, "_", n_session_fwd_string, "_sessions.jpeg")
  source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  There were ", note_add), width = 85)

  text_labels <- to_plot %>%
                  mutate(label = paste0(round(100*median_ret, 1), "%"))
  
  plot <- ggplot(to_plot, aes(x=key, y=median_ret, fill = key)) + 
    geom_bar(stat = "identity") +
    geom_text(data=text_labels, aes(x=key, y = median_ret, col = key, label = label), 
              vjust = ifelse(text_labels$median_ret >0, -0.2, 1.2)) +
    scale_fill_manual(values = c("#10253F", "#ff0000"), guide = FALSE) +
    scale_color_manual(values = c("#10253F", "#ff0000"), guide = FALSE) +
    scale_y_continuous(label = percent, limits = c(lower_limit, upper_limit)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Median Return Over Next ", n_session_fwd, " Sessions\nFor All Days and ", cut_title, "s")) +
    labs(x = "Period" , y = "Median Return",
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Plot GDP diffs
  if(n_session_fwd >= 250){
    file_path <- paste0(out_path, "/gdp_", cut, "_", n_session_fwd_string, "_sessions.jpeg")
    source_string <- paste0("Source:  FRED, ", first_year_fred, "-", last_year_fred, " (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note:  There were ", note_add), width = 85)
    
    text_labels <- to_plot %>%
      mutate(label = paste0(round(100*median_gdp, 1), "%"))
    
    plot <- ggplot(to_plot, aes(x=key, y=median_gdp, fill = key)) + 
      geom_bar(stat = "identity") +
      geom_text(data=text_labels, aes(x=key, y = median_gdp, col = key, label = label), 
                vjust = ifelse(text_labels$median_gdp >0, -0.2, 1.2)) +
      scale_fill_manual(values = c("#10253F", "#ff0000"), guide = FALSE) +
      scale_color_manual(values = c("#10253F", "#ff0000"), guide = FALSE) +
      scale_y_continuous(label = percent) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Median Real GDP Growth Over Next Year\nFor All Days and ", cut_title, "s")) +
      labs(x = "Period" , y = "Median Real GDP Growth",
           caption = paste0("\n", source_string, "\n", note_string))  
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

session_lengths <- c(5, 10, 20, 60, 90, 120, 250)
cut_types <- c(0, 1, 2, 3)

for(c in cut_types){
  if(c == 0){
    l_limit <- -0.01
    u_limit <- 0.15
    cut_word <- "changes"
  } else if(c == 1){
    l_limit <- -0.15
    u_limit <- 0.15
    cut_word <- "cuts"
  } else if(c == 2){
    l_limit <- -0.15
    u_limit <- 0.15
    cut_word <- "emergency_cuts"
  } else if(c == 3){
    l_limit <- -0.01
    u_limit <- 0.15
    cut_word <- "increases"
  }
  
  for(s in session_lengths){
    run_bar_plot(s, c, l_limit, u_limit)
  }
  create_gif(path = out_path, 
             file_stub = paste0("avg_vs_fed_", c, "_*.jpeg"), 
             speed_milliseconds = 120, 
             out_name = paste0("_all_avg_vs_fed_", cut_word, ".gif"))
}

# Plot price
file_path <- paste0(out_path, "/price_vs_fed_changes.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")

to_plot <- spx %>%
  select(date, index_sp500, real_gdp, real_gdp_change, pct, gdp_dd)

points <- spx %>%
            mutate(type = case_when(cut == 1 ~ "Cut",
                                    cut == 0 ~ "Raise",
                                    TRUE ~ "")
                   ) %>%
            filter(type != "")
  
note_string <- str_wrap(paste0("Note:  Red dots correspond to decreases in the federal funds rate while green dots correspond to increases in the federal funds rate.  ",
                               "There were ", nrow(points), " changes to the federal funds rate since 1994."), 
                        width = 85)

  plot <- ggplot(to_plot, aes(x=date, y=index_sp500)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=index_sp500, col = type)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("The S&P 500 and\nWhen the Fed Changed Rates")) +
  labs(x = "Date" , y = "S&P 500 Index",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot real GDP
file_path <- paste0(out_path, "/real_gdp_vs_fed_changes.jpeg")
source_string <- paste0("Source:  FRED, ", first_year_fred, "-", last_year_fred, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=date, y=real_gdp)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=real_gdp, col = type)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Real GDP and\nWhen the Fed Changed Rates")) +
  labs(x = "Date" , y = "U.S. Real GDP\n(in billions)",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot DD
file_path <- paste0(out_path, "/dd_vs_fed_changes.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=date, y=pct)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=pct, col = type)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Drawdowns and\nWhen the Fed Changed Rates")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot GDP DD
file_path <- paste0(out_path, "/dd_gdp_vs_fed_changes.jpeg")
source_string <- paste0("Source:  FRED, ", first_year_fred, "-", last_year_fred, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=date, y=gdp_dd)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=gdp_dd, col = type)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Real GDP Drawdowns and\nWhen the Fed Changed Rates")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot real gdp change
file_path <- paste0(out_path, "/real_gdp_change_vs_fed_changes.jpeg")
source_string <- paste0("Source:  FRED, ", first_year_fred, "-", last_year_fred, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=date, y=real_gdp_change)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=real_gdp_change, col = type)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Real GDP Growth and\nWhen the Fed Changed Rates")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #