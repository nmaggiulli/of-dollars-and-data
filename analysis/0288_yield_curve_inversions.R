cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(readxl)
library(ggjoy)
library(tidyverse)

folder_name <- "0288_yield_curve_inversions"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            filter(date >= "1970-01-01") %>%
        select(date, price_plus_div)

inversion_dates <- c(
                     as.Date("1978-08-01"),
                     as.Date("1980-09-01"),
                     as.Date("1988-12-01"),
                     as.Date("2000-02-01"),
                     as.Date("2005-12-01"),
                     as.Date("2019-08-01")
                     )

plot_since_inversion <- function(n_years){
  
  if(n_years > 2){
    n_inversions <- length(inversion_dates) - 1 
  } else{
    n_inversions <- length(inversion_dates)
  }
  
  n_months <- n_years * 12 - 1
  end_date <- as.Date("2021-12-01") - years(n_years)
  
  all_dates <- seq.Date(as.Date("1970-01-01"), end_date, "month")
  
  for(i in 1:length(all_dates)){
    start_dt <-  all_dates[i]
    end_dt <- all_dates[i] + months(n_months)
    
    tmp <- raw %>%
      filter(date >= start_dt, date <= end_dt)
    
    first_value <- pull(tmp[1, "price_plus_div"])
    
    tmp <- tmp %>%
              mutate(index = price_plus_div/first_value,
                     month = row_number(),
                     start_date = start_dt,
                     end_date = end_dt)
    
    if(i == 1){
      stack <- tmp
    } else{
      stack <- stack %>% bind_rows(tmp)
    }
  }
  
  to_plot <- stack %>%
                mutate(
                  inversion = case_when(
                    start_date %in% inversion_dates ~ 1,
                    TRUE ~ 0
                  ),
                  start_date = case_when(
                  start_date %in% inversion_dates ~ start_date + years(200),
                  TRUE ~ start_date
                )
                ) %>%
            arrange(start_date, end_date)
  
  gray_vector <- rep("gray", length(all_dates) - n_inversions)
  red_vector <- rep("red", n_inversions)

  file_path <- paste0(out_path, "/growth_since_inversion_", n_years, "_years.jpeg")
  source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: All figures include dividends and are adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=month, y=index, col = as.factor(start_date))) +
    geom_line() +
    scale_x_continuous(breaks = seq(3, n_months + 1, 3)) +
    scale_y_continuous(label = dollar) +
    scale_color_manual(guide = "none", values = c(gray_vector, red_vector)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500 Growth\nAfter Yield Curve Inversions")) +
    labs(x = "Month", y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  summary <- to_plot %>%
              filter(month == n_months + 1) %>%
              mutate(ret = index - 1) %>%
              group_by(inversion) %>%
              summarise(n_years = n_years,
                        n_obs = n(),
                        median_ret = quantile(ret, probs = 0.5)) %>%
              ungroup()
  
  assign(paste0("summary_", n_years), summary, envir = .GlobalEnv)
}

plot_since_inversion(1)
plot_since_inversion(2)
plot_since_inversion(5)

# ############################  End  ################################## #

  
