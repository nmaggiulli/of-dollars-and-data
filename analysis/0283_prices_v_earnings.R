cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(tidyverse)

folder_name <- "0283_prices_vs_earnings"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

snapshot_date <- as.Date("2011-01-01")
end_date <- as.Date("2021-09-01")

sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            filter(!is.na(real_earn_scaled), date >= "1920-01-01") %>%
            select(date, real_earn_scaled, price_plus_div)

analysis_dates <- seq.Date(as.Date("1920-01-01"), as.Date("2000-01-01"), "10 years") 
snapshot_dates <- rep(snapshot_date, length(analysis_dates))
                    
analysis_dates <- c(analysis_dates, snapshot_date, as.Date("2000-01-01"), as.Date("1990-01-01"))
snapshot_dates <- c(snapshot_dates, rep(end_date, 3))
                    
final_results <- data.frame()
counter <- 1

for(i in 1:length(analysis_dates)){
  a_date <- analysis_dates[i]
  s_date <- snapshot_dates[i]
  
  n_months <- (year(s_date) - year(a_date)) * 12
  
  orig_price <- filter(sp500, date == a_date) %>% pull(price_plus_div)
  orig_earn <- filter(sp500, date == a_date) %>% pull(real_earn_scaled)
  
  snap_price <- filter(sp500, date == s_date) %>% pull(price_plus_div)
  snap_earn <- filter(sp500, date == s_date) %>% pull(real_earn_scaled)

  final_results[counter, "analysis_date"] <- a_date
  final_results[counter, "snap_date"] <- s_date
  final_results[counter, "ret_price"] <- (snap_price/orig_price)^(12/n_months)  - 1
  final_results[counter, "ret_earn"] <- (snap_earn/orig_earn)^(12/n_months)  - 1
  
  counter <- counter + 1
}

to_plot <- final_results %>%
              filter(snap_date == snapshot_date) %>%
              select(analysis_date, ret_price, ret_earn) %>%
              rename(`Earnings` = ret_earn,
                     `Prices` = ret_price) %>%
              gather(-analysis_date, key=key, value = value) %>%
              mutate(start_decade = year(analysis_date))

source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: All figures include dividends and are adjusted for inflation."),
                        width = 85)

snap_string <- date_to_string(snapshot_date)

file_path <- paste0(out_path, "/prices_vs_earnings_", snap_string, ".jpeg")

plot <- ggplot(data = to_plot, aes(x = as.factor(start_decade), y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("green", "black")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  ggtitle(paste0("Annualized Changes in Prices and Earnings\nThrough January ", year(snapshot_date))) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "Start Decade" , y = "Annualized Change",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #

  
