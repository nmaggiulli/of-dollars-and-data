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
end_dt <- "2020-12-31"

sp500 <-  readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  rename(index_sp500 = price_plus_div) %>%
  mutate(mt = month(date),
         yr = year(date)) %>%
  filter(date >= start_dt, date <= end_dt) %>%
  select(mt, yr, index_sp500)

gld <- read.csv(paste0(importdir, "0192_why_is_gold_valuable/gld.csv"), skip = 1) %>%
  mutate(date = as.Date(Date, format = "%m/%d/%y"),
         mt = month(date),
         yr = year(date),
         index_gld = Close) %>%
    filter(date >= start_dt, date <= end_dt) %>%
    select(mt, yr, index_gld)

cpi <- read.csv(paste0(importdir, "0192_why_is_gold_valuable/fred_cpi.csv")) %>%
        mutate(date = as.Date(DATE, format = "%m/%d/%y"),
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

file_path <- paste0(out_path, "/gld_vs_gld_real.jpeg")
source_string <- paste0("Source:  FRED, Stockcharts (OfDollarsAndData.com)")

text_labels <- to_plot %>% 
                filter(date == max(to_plot$date))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text_repel(data=text_labels, 
                  aes(x=date, 
                      y=value, 
                      col = key, 
                      label = paste0(key, "\n$", round(value, 2))), 
                  size = 2.5,
                  nudge_x = -100,
                  segment.color = "transparent") + 
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

file_path <- paste0(out_path, "/gld_vs_sp500.jpeg")
source_string <- paste0("Source:  FRED, Stockcharts, Robert Shiller (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: S&P 500 return includes dividends."))

text_labels <- to_plot %>% 
  filter(date == max(to_plot$date))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text_repel(data=text_labels, 
                  aes(x=date, 
                      y=value, 
                      col = key, 
                      label = paste0(key, "\n$", round(value, 2))), 
                  size = 2.5,
                  segment.color = "transparent",
                  nudge_y = ifelse(text_labels$key == "S&P 500", -2.5, 0)) + 
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
            summarize(mean_ret_gld_real = mean(ret_gld_real, na.rm = TRUE),
                      mean_ret_sp500_real = mean(ret_sp500_real, na.rm= TRUE)) %>%
            ungroup() %>%
            filter(decade != 2020)

file_path <- paste0(out_path, "/gld_real_return_by_decade_ex2020.jpeg")
source_string <- paste0("Source:  FRED, Stockcharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: 1970 decade data starts in 1975."))

plot <- ggplot(to_plot, aes(x=decade, y=mean_ret_gld_real)) +
  geom_bar(stat = "identity", fill = "gold") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Real Gold Average Return by Decade")) +
  labs(x="Decade", y="Average Annual Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #