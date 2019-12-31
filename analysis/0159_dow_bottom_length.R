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
  } else{
    ret <- df[i, "index_dow"]/df[(i-1), "index_dow"]
    
    df[i, "value_dca"] <- (df[(i-1), "value_dca"] + daily_cash) * ret
    
    if(df[(i-1), "index_dow"] > df[(i-1), "low_watermark"]){
      df[i, "value_ab_cash"] <- df[(i-1), "value_ab_cash"] + daily_cash
      df[i, "value_ab_vest"] <- df[(i-1), "value_ab_vest"] * ret
    } else{
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