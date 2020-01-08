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
library(gganimate)
library(tidylog)
library(tidyverse)

folder_name <- "0159_bottom_length"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0159_dow_bottom_length/dow_stockcharts.csv"), skip = 1,
                col.names = c("date", "open", "high", "low", "index_dow", "volume")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  select(date, index_dow)

first_year <- min(year(raw$date))

df <- raw %>%
        arrange(desc(date))

# Find low matermark
absolute_minumum <- 10^8

for(i in 1:nrow(df)){
  current_p <- df[i, "index_dow"]
  if (current_p < absolute_minumum){
    df[i, "low_watermark"] <- current_p
    absolute_minumum <- current_p
  } else{
    df[i, "low_watermark"] <- absolute_minumum
  }
}

df <- df %>%
        arrange(date)

daily_cash <- 1

#DCA vs absolute bottom strategy
for(i in 1:nrow(df)){
  
  if(i == 1){
    df[i, "value_dca"] <- daily_cash
    df[i, "value_ab_cash"] <- daily_cash
    df[i, "value_ab_vest"] <- 0
    df[i, "avg_dca"] <- NA
    df[i, "avg_ab"] <- NA
  } else{
    ret <- df[i, "index_dow"]/df[(i-1), "index_dow"]
    df[i, "avg_dca"] <- mean(df[1:i, "index_dow"], na.rm = TRUE)
    df[i, "avg_ab"] <- mean(df[1:i, "low_watermark"], na.rm = TRUE)
    
    df[i, "value_dca"] <- (df[(i-1), "value_dca"] + daily_cash) * ret
    
    if(df[(i-1), "index_dow"] > df[(i-1), "low_watermark"]){
      df[i, "value_ab_cash"] <- df[(i-1), "value_ab_cash"] + daily_cash
      df[i, "value_ab_vest"] <- df[(i-1), "value_ab_vest"] * ret
    } else{
      df[i, "ab_purchase"] <- 1
      
      df[i, "value_ab_vest"] <- (df[(i-1), "value_ab_vest"] + df[(i-1), "value_ab_cash"] + daily_cash) * ret
      df[i, "value_ab_cash"] <- 0
    }
  }
  
  df[i, "value_ab"] <- df[i, "value_ab_cash"] + df[i, "value_ab_vest"]
  df[i, "pct_ab_over_dca"] <- df[i, "value_ab"]/df[i, "value_dca"]
}

summary <- df %>%
            arrange(date) %>%
            mutate(lower_future = ifelse(low_watermark < index_dow, 1, 0),
                   lag_days_to_bottom = lag(value_ab_cash))

bottoms <- summary %>%
            filter(value_ab_cash == 0)

n_bottoms <- nrow(bottoms)

# Plot Dow vs. watermark
to_plot <- df %>%
            select(date, index_dow, low_watermark) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dow_lower_watermark.jpeg")
source_string <- "Source:  StockCharts, 1970-2019 (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  scale_color_manual(guide = FALSE, values = c("black", "red")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Index and Low Watermark")) +
  labs(x="Date", y="Index Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Another one
to_plot <- df %>%
  select(date, index_dow, low_watermark) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dow_lower_watermark_buys.jpeg")
source_string <- "Source:  StockCharts, 1970-2019 (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_point(data = select(bottoms, date, low_watermark), aes(x=date, y=low_watermark), col = "blue", size = 1) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  scale_color_manual(guide = FALSE, values = c("black", "red")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Index and Low Watermark")) +
  labs(x="Date", y="Index Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot AB vs DCA
to_plot <- df %>%
  select(date, value_dca, value_ab) %>%
  rename(`DCA` = value_dca,
         `Absolute-Bottom` = value_ab) %>%
  gather(-date, key=key, value=value)

final_diff <- df[nrow(df), "value_ab"] - df[nrow(df), "value_dca"]
final_diff_pct <- df[nrow(df), "value_ab"]/df[nrow(df), "value_dca"] - 1

text_labels <- data.frame(date = c(as.Date("2014-07-01"), as.Date("2014-07-01")),
                          value = c(df[nrow(df), "value_ab"] + 2000, 60000),
                          key = c("Absolute-Bottom", "DCA"),
                          label = c(paste0("Absolute Bottom\n+$", formatC(final_diff, digits = 0, format = "f", big.mark = ","), 
                                         "\n(+",
                                         round(100*final_diff_pct, 1), "%)"),
                                    "DCA")
)

file_path <- paste0(out_path, "/dow_dca_vs_ab.jpeg")
source_string <- "Source:  StockCharts, 1970-2019 (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text_repel(data = text_labels,
                  aes(x = date,
                      y = value,
                      col = key,
                      label = text_labels$label,
                      family = "my_font"),
                  size = 3,
                  segment.colour = "transparent",
                  max.iter  = 1
  ) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("red", "blue"), guide = FALSE) +
  of_dollars_and_data_theme +
  ggtitle(paste0("DCA vs. Absolute-Bottom Buying Strategy")) +
  labs(x="Date", y="Portfolio Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot average prices for each strategy
to_plot <- df %>%
            select(date, avg_dca, avg_ab) %>%
            rename(DCA = avg_dca,
                   `Absolute-Bottom` = avg_ab) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/avg_price_dca_vs_ab.jpeg")
source_string <- "Source:  StockCharts, 1970-2019 (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  scale_color_manual(values = c("red", "blue")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Average Purchase Prices\nDCA vs. Absolute-Bottom Buying Strategy")) +
  labs(x="Date", y="Average Index Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

print(paste0(round(100*mean(summary$lower_future), 1), "% of days will see a lower value in the future."))
print(paste0("There are only ",
             n_bottoms,
             " absolute bottoms (", 
             round(100*(n_bottoms/nrow(summary)), 1),
             "% of all days) since ", 
             first_year, "."))

print(paste0("The average number of days before a bottom is ", mean(df$value_ab_cash), "."))
print(paste0("The median number of days before a bottom is ", quantile(df$value_ab_cash, probs = 0.5), "."))

# ############################  End  ################################## #