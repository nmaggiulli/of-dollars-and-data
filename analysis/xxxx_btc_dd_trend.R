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

folder_name <- "xxxx_btc_dd_trend"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_day_trend <- 20

df <- readRDS(paste0(localdir, "0027_quandl_bitcoin.Rds")) %>%
        arrange(desc(date))

# Find low matermark
absolute_minumum <- 10^8

for(i in 1:nrow(df)){
  current_p <- df[i, "index_btc"]
  if (current_p < absolute_minumum){
    df[i, "low_watermark"] <- current_p
    absolute_minumum <- current_p
  } else{
    df[i, "low_watermark"] <- absolute_minumum
  }
}

df <- df %>%
        arrange(date) %>%
        mutate(watermark_over_index = low_watermark/index_btc - 1,
          index_equal_watermark = ifelse(watermark_over_index == 0, 1, 0))

btc_dd <- drawdown_path(df, dd_counts = 1)

df <- df %>%
        left_join(btc_dd)

to_plot <- df

file_path <- paste0(out_path, "/btc_watermark_over_index.jpeg")
source_string <- paste0("Source: Quandl")

plot <- ggplot(to_plot, aes(x=date, y=watermark_over_index)) + 
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("BTC Future Decline")) +
  labs(x = "Date" , y = "Watermark over Index",
       caption = paste0(source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/dist_btc_watermark_over_index.jpeg")
source_string <- paste0("Source: Quandl")

plot <- ggplot(to_plot, aes(-watermark_over_index)) + 
  geom_density() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("BTC Future Decline Distribution")) +
  labs(x = "Future Decline Percentage" , y = "Frequency",
       caption = paste0(source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- df %>%
  left_join(btc_dd) %>%
  select(date, low_watermark, index_btc) %>%
  rename(`Low Watermark` = low_watermark,
         `Bitcoin` = index_btc) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/btc_index_watermark.jpeg")
source_string <- paste0("Source: Quandl")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
  geom_line() +
  scale_color_manual(values = c("black", "red")) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("BTC Price and Low Watermark")) +
  labs(x = "Date" , y = "Price (Log Scale)",
       caption = paste0(source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

print(mean(df$pct, na.rm = TRUE))
print(mean(df$index_equal_watermark, na.rm = TRUE))
print(mean(df$watermark_over_index, na.rm = TRUE))

# ############################  End  ################################## #