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

folder_name <- "0192_why_is_gold_valuable"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

start_dt <- "1974-08-01"
end_dt <- "2022-12-31"
today_string <- date_to_string(Sys.Date())

sp500 <-  readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  rename(index_sp500 = price_plus_div) %>%
  mutate(mt = month(date),
         yr = year(date)) %>%
  filter(date >= start_dt, date <= end_dt) %>%
  select(mt, yr, index_sp500)

gld <- read.csv(paste0(importdir, "0192_why_is_gold_valuable/gld.csv"),
                col.names = c("date", "index_gld")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         mt = month(date),
         yr = year(date)) %>%
    arrange(date) %>%
    filter(date >= start_dt, date <= end_dt)

last_date <- gld %>%
        group_by(yr, mt) %>%
        summarise(date = max(date)) %>%
        ungroup()

gld <- gld %>%
          inner_join(last_date) %>%
          mutate(ret_gld = index_gld/lag(index_gld) - 1)

cpi <- read.csv(paste0(importdir, "0192_why_is_gold_valuable/fred_cpi.csv")) %>%
        mutate(date = as.Date(DATE, format = "%Y-%m-%d"),
               mt = month(date),
               yr = year(date),
               index_cpi = CPIAUCSL) %>%
        filter(date >= start_dt, date <= end_dt) %>%
        select(mt, yr, index_cpi)

df <- gld %>%
        left_join(cpi) %>%
        left_join(sp500) %>%
        mutate(date = as.Date(paste0(yr, "-", mt, "-01"), format = "%Y-%m-%d"),
               index_gld = index_gld/gld[1, "index_gld"],
               index_cpi = index_cpi/cpi[1, "index_cpi"],
               index_sp500 = index_sp500/pull(sp500[1, "index_sp500"])) %>%
        select(date, contains("index_"))

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "index_gld_real"] <- 1
  } else{
    ret_gld <- df[i, "index_gld"]/df[(i-1), "index_gld"] - 1
    ret_cpi <- df[i, "index_cpi"]/df[(i-1), "index_cpi"] - 1
    
    df[i, "index_gld_real"] <- df[(i-1), "index_gld_real"] * (1 + ret_gld - ret_cpi)
  }
}

to_plot <- df %>%
              rename(Gold = index_gld,
                     `Gold (real)` = index_gld_real) %>%
            select(!(contains("index_"))) %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/gld_vs_gld_real_", today_string, ".jpeg")
source_string <- paste0("Source:  FRED, Stockcharts (OfDollarsAndData.com)")

text_labels <- data.frame()

text_labels[1, "date"] <- as.Date("2022-12-31")
text_labels[1, "key"] <- "Gold"
text_labels[1, "value"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold") %>% pull(value) - 2
text_labels[1, "label"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold") %>% mutate(label = paste0(key, "\n$", round(value, 2))) %>% pull(label)
text_labels[2, "date"] <- as.Date("2022-01-31")
text_labels[2, "key"] <- "Gold (real)"
text_labels[2, "value"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold (real)") %>% pull(value) + 1.5
text_labels[2, "label"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold (real)") %>% mutate(label = paste0(key, "\n$", round(value, 2))) %>% pull(label)
                
plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, 
                  aes(x=date, 
                      y=value, 
                      col = key,
                      label = label),
            size = 3,
            family = "my_font") + 
  scale_color_manual(guide = FALSE, values = c("blue", "gold")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1")) +
  labs(x="Date", y="Growth of $1",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- df %>%
  filter(!is.na(index_sp500)) %>%
  rename(`Gold (real)` = index_gld_real,
         `S&P 500` = index_sp500) %>%
  select(!(contains("index_"))) %>%
  gather(-date, key=key, value=value)

text_labels <- data.frame()

text_labels[1, "date"] <- as.Date("2022-12-30")
text_labels[1, "key"] <- "S&P 500"
text_labels[1, "value"] <- to_plot %>% filter(date == max(to_plot$date), key == "S&P 500") %>% pull(value) -4
text_labels[1, "label"] <- to_plot %>% filter(date == max(to_plot$date), key == "S&P 500") %>% mutate(label = paste0(key, "\n$", round(value, 2))) %>% pull(label)
text_labels[2, "date"] <- as.Date("2022-01-31")
text_labels[2, "key"] <- "Gold (real)"
text_labels[2, "value"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold (real)") %>% pull(value) + 4
text_labels[2, "label"] <- to_plot %>% filter(date == max(to_plot$date), key == "Gold (real)") %>% mutate(label = paste0(key, "\n$", round(value, 2))) %>% pull(label)

file_path <- paste0(out_path, "/gld_vs_sp500_", today_string, ".jpeg")
source_string <- paste0("Source:  FRED, Stockcharts, Robert Shiller (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: S&P 500 return includes dividends."))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, 
            aes(x=date, 
                y=value, 
                col = key,
                label = label),
            size = 2.7,
            family = "my_font") + 
  scale_color_manual(guide = FALSE, values = c("gold", "green")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Growth of $1\nGold vs. S&P 500")) +
  labs(x="Date", y="Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Get gold returns by decade (average by year)
to_plot <- df %>%
            filter(month(date) == 1) %>%
            mutate(ret_gld_real = index_gld_real/lag(index_gld_real, 1) - 1,
                   ret_sp500_real = index_sp500/lag(index_sp500, 1) - 1,
                   decade =  paste0(as.character(year(floor_date(date, years(10)))))) %>%
            group_by(decade) %>%
            summarise(mean_ret_gld_real = mean(ret_gld_real, na.rm = TRUE),
                      mean_ret_sp500_real = mean(ret_sp500_real, na.rm= TRUE)) %>%
            ungroup()

file_path <- paste0(out_path, "/gld_real_return_by_decade_", today_string, ".jpeg")
source_string <- paste0("Source:  FRED, Stockcharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: 1970 decade data starts in 1975 and the 2020 decade data ends in 2022."))

plot <- ggplot(to_plot, aes(x=decade, y=mean_ret_gld_real)) +
  geom_bar(stat = "identity", fill = "gold") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Gold Average Return by Decade")) +
  labs(x="Decade", y="Average Annual Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# And SPX corr analysis
#Corr analysis
spx <- read.csv(paste0(importdir, "0192_why_is_gold_valuable/SPX_data_1970.csv")) %>%
  mutate(date = as.Date(Period),
         mt = month(date),
         yr = year(date)) %>%
  rename(index_sp500 = `S.P.500...SPX..Level`) %>%
  arrange(date)

last_date <- spx %>%
  group_by(yr, mt) %>%
  summarise(date = max(date)) %>%
  ungroup()

spx_monthly <- spx %>%
  inner_join(last_date) %>%
  mutate(ret_sp500 = index_sp500/lag(index_sp500, 1) - 1) %>%
  select(mt, yr, ret_sp500)

gld_w_spx <- gld %>%
  select(date, ret_gld) %>%
  mutate(mt = month(date),
         yr = year(date)) %>%
  left_join(spx_monthly) %>%
  filter(!is.na(ret_gld), !is.na(ret_sp500))

for(i in 1:nrow(gld_w_spx)){
  if(i >= 36){
    gld_w_spx[i, "cor_36m"] <- cor(gld_w_spx[(i-35):i, "ret_gld"], gld_w_spx[(i-35):i, "ret_sp500"])
  } else{
    gld_w_spx[i, "cor_36m"] <- NA
  }
}

full_cor <- cor(gld_w_spx$ret_gld, gld_w_spx$ret_sp500)

file_path <- paste0(out_path, "/gold_spx_cor_", today_string, ".jpeg")
source_string <- paste0("Source: YCharts, 1970-2022 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: The full period correlation for the monthly returns between Gold and the S&P 500 is ", round(full_cor, 2), "."),
                        width = 85)

plot <- ggplot(gld_w_spx, aes(x=date, y=cor_36m)) + 
  geom_line() +
  of_dollars_and_data_theme +
  ggtitle(paste0("Rolling 36-Month Correlation of Monthly Returns\nBetween Gold and the S&P 500")) +
  labs(x = "Date" , y = "36-Month Correlation",
       caption = paste0(source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #