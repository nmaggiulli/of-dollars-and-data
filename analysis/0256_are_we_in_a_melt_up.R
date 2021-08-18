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
library(tidyverse)

folder_name <- "0256_are_we_in_a_melt_up"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Read in ycharts data
dot_com_peak <- as.Date("2000-03-24")
dot_com_bottom <- as.Date("2003-03-11")

raw <- read.csv(paste0(importdir, "0256_ycharts_spx_1995_2000/SPX_data.csv"),
                col.names = c("date", "index")) %>%
          mutate(date = as.Date(date),
                 month = month(date),
                 year = year(date)) %>%
          arrange(date)

spx <- raw %>%
        filter(date >= as.Date("1995-01-01"), date <= dot_com_bottom)

dd <- spx %>%
        drawdown_path()

to_plot <- dd %>%
            filter(date <= dot_com_peak)

file_path <- paste0(out_path, "/dd_spx_1995_2000.jpeg")
source_string <- paste0("Source: YCharts")

plot <- ggplot(to_plot, aes(x= date, y=pct)) +
  geom_area(fill = "red") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Drawdowns\nDuring the DotCom Bubble")) +
  labs(x="Date", y="Percentage of Value Lost",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Plot cumulative pct change
first <- spx[1, "index"]

to_plot <- spx %>%
            mutate(pct_change = index/first - 1) %>%
            filter(date <= dot_com_peak)

file_path <- paste0(out_path, "/pct_change_spx_1995_2000.jpeg")
source_string <- paste0("Source: YCharts")

text_labels <- data.frame()
text_labels[1, "date"] <- to_plot[nrow(to_plot), "date"]
text_labels[1, "pct_change"] <- to_plot[nrow(to_plot), "pct_change"]
text_labels[1, "label"] <- paste0("+", round(100*to_plot[nrow(to_plot), "pct_change"]), "%")

plot <- ggplot(to_plot, aes(x= date, y=pct_change)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=pct_change, label = label), 
            family = "my_font",
            col = "black",
            nudge_y = 0.1,
            nudge_x = -15) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Total Percentage Change\nJan 1995- Mar 2000")) +
  labs(x="Date", y="Percentage Change",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do again for decline
post_peak <- spx %>%
  filter(date >= dot_com_peak)

first_post_peak <- post_peak[1, "index"]

to_plot <- post_peak %>%
            mutate(pct_change = index/first_post_peak - 1) 


file_path <- paste0(out_path, "/pct_change_spx_2000_2003.jpeg")
source_string <- paste0("Source: YCharts")

text_labels <- data.frame()
text_labels[1, "date"] <- to_plot[nrow(to_plot), "date"]
text_labels[1, "pct_change"] <- to_plot[nrow(to_plot), "pct_change"]
text_labels[1, "label"] <- paste0(round(100*to_plot[nrow(to_plot), "pct_change"]), "%")


plot <- ggplot(to_plot, aes(x= date, y=pct_change)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=pct_change, label = label), 
            family = "my_font",
            col = "red",
            nudge_y = -0.02,
            nudge_x = 0) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Total Percentage Change\nMar 2000- Mar 2003")) +
  labs(x="Date", y="Percentage Change",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Overall pct change
to_plot <- spx %>%
  mutate(pct_change = index/first - 1)

file_path <- paste0(out_path, "/pct_change_spx_1995_2003.jpeg")
source_string <- paste0("Source: YCharts")

text_labels <- data.frame()
text_labels[1, "date"] <- to_plot[nrow(to_plot), "date"]
text_labels[1, "pct_change"] <- to_plot[nrow(to_plot), "pct_change"]
text_labels[1, "label"] <- paste0("+", round(100*to_plot[nrow(to_plot), "pct_change"]), "%")

plot <- ggplot(to_plot, aes(x= date, y=pct_change)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=pct_change, label = label), 
            family = "my_font",
            nudge_y = -0.07,
            nudge_x =  0) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Total Percentage Change\nJan 1995- Mar 2003")) +
  labs(x="Date", y="Percentage Change",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do ATH analysis
ath <- raw %>%
        mutate(year = year(date),
               month = month(date))

for(i in 1:nrow(ath)){
  if(i == 1){
    ath[i, "ath"] <- 1
    ath_tmp <- ath[i, "index"]
  } else{
    if(ath[i, "index"] > ath_tmp){
      ath[i, "ath"] <- 1
      ath_tmp <- ath[i, "index"]
    } else{
      ath[i, "ath"] <- 0
    }
  }
}

ath_summary <- ath %>%
                filter(month <= 8) %>%
                group_by(year) %>%
                summarise(ath = sum(ath)) %>%
                ungroup()

# Stats about hist returns
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  select(date, price_plus_div) %>%
  mutate(ret_18m = price_plus_div/lag(price_plus_div, 18) - 1,
         ret_60m = price_plus_div/lag(price_plus_div, 60) - 1) %>%
  drop_na %>%
  filter(year(date) >= 1920)

pct_cutoff_18m <- 0.40
pct_cutoff_60m <- 1.23

n_months_above <- nrow(sp500_ret_pe %>% filter(ret_18m > pct_cutoff_18m))
print(paste0("Percentage of 18m returns above ", 100*pct_cutoff_18m, "% is: ", round(100*n_months_above/nrow(sp500_ret_pe), 1), "%."))

n_months_above <- nrow(sp500_ret_pe %>% filter(ret_60m > pct_cutoff_60m))
print(paste0("Percentage of 60m returns above ", 100*pct_cutoff_60m, "% is: ", round(100*n_months_above/nrow(sp500_ret_pe), 1), "%."))



# ############################  End  ################################## #