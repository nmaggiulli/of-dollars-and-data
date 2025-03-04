cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(jsonlite)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "0445_spx_btc_mcrib"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

start_analysis_date <- "2010-07-17"
end_analysis_date <- "2024-12-31"
run_sim <- 0

raw_spx <- read.csv(paste0(importdir, "0445_spx_btc_daily/SPX_data.csv")) %>%
        rename(date = Period,
               index = `S.P.500...SPX..Level`) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
        filter(date >= start_analysis_date, date < end_analysis_date) %>%
        arrange(date) %>%
        mutate(year = year(date),
               date = as.Date(date),
               ret_spx = index/lag(index) - 1) %>%
        select(date, index, year, ret_spx) %>%
        filter(!is.na(ret_spx))

#Bring in BTC data
new_btc <- read.csv(paste0(importdir, "0445_spx_btc_daily/IBTCUSD_data.csv")) %>%
  rename(date = Period,
         btc = `Bitcoin.Price..I.BTCUSD.`) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  arrange(date)

old_btc <- read.csv(paste0(importdir, "0445_spx_btc_daily/bitcoin_2010-07-17_2024-06-28.csv")) %>%
            rename(btc = Close) %>%
            mutate(date = as.Date(Start)) %>%
            arrange(date) %>%
            select(date, btc)

raw_btc <- old_btc %>%
            bind_rows(new_btc) %>%
            mutate(ret_btc = btc/lag(btc) - 1) %>%
            select(date, ret_btc)

raw <- raw_spx %>%
          inner_join(raw_btc)
          
# Calculate max year
max_year <- max(raw$year)

mcrib_dates <- data.frame(start = c("2010-11-02", "2011-10-24", "2012-12-17",
                                    "2013-10-15", "2014-11-05", "2015-09-15",
                                    "2016-11-09", "2017-11-09", "2018-10-29",
                                    "2019-10-07", "2020-12-02", "2021-10-08",
                                    "2022-10-17", "2023-11-11", "2024-12-03"
                                    ),
                          end = c("2010-12-05", "2011-11-14", "2013-01-15",
                                  "2013-12-15", "2014-12-31", "2015-11-30", 
                                  "2016-12-31", "2017-12-31", "2018-12-31",
                                  "2019-12-31", "2020-12-31", "2022-01-23",
                                  "2022-11-20", "2023-12-10", "2024-12-31"
                                  )) %>%
                          mutate(n_days = as.Date(end)-as.Date(start),
                                 sim_start = as.Date(paste0(year(start), "-01-01")),
                                 sim_end = as.Date(paste0(year(start), "-12-31")) - n_days) %>%
                          filter(sim_end < end_analysis_date)

for(i in 1:nrow(mcrib_dates)){
  s <- mcrib_dates[i, "start"]
  e <- mcrib_dates[i, "end"]
  date_seq <- data.frame(date=seq.Date(as.Date(s), as.Date(e), by="day"))
  
  if(i == 1){
    all_dates <- date_seq
  } else{
    all_dates <- bind_rows(all_dates, date_seq)
  }
  
  if (i == nrow(mcrib_dates)){
    all_dates <- all_dates %>%
                    mutate(mcrib = 1)
  }
}

df_mcrib <- raw %>%
              left_join(all_dates) %>%
              mutate(mcrib = ifelse(is.na(mcrib), "Without McRib", "With McRib"),
                     mcrib_dummy = ifelse(mcrib == "With McRib", 1, 0))

n_pct_days <- nrow(all_dates)/nrow(df_mcrib)

t.test(df_mcrib$ret_spx~df_mcrib$mcrib_dummy)
t.test(df_mcrib$ret_btc~df_mcrib$mcrib_dummy)

# Set note and source string
source_string <- str_wrap("Source: YCharts.com, Kaggle, http://www.jeffreysward.com/editorials/mcrib.htm (OfDollarsAndData.com)",
                          width = 80)
note_string   <- str_wrap(paste0("Note:  Only includes McRib release data from 2010-", max_year, ".  The McRib was available on ", 100*round(n_pct_days, 3), "% of all trading days from 2010-", max_year, "."), 
                          width = 80)

# Do SPX analysis
file_path <- paste0(out_path, "/mcrib_spx_days.jpeg")

to_plot <- df_mcrib %>%
            group_by(mcrib) %>%
            summarise(ret_spx = mean(ret_spx, na.rm = TRUE)) %>%
            ungroup() %>%
            arrange(mcrib) %>%
            mutate(label = paste0(round(100*ret_spx,2), "%"))

plot <- ggplot(to_plot, aes(x=mcrib, y=ret_spx, fill = mcrib)) +
          geom_bar(stat="identity") +
          geom_text(data=to_plot, aes(x=mcrib, y=ret_spx+0.00004, label=label),
                    col = "black") +
          scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
          of_dollars_and_data_theme +
          scale_y_continuous(label = percent_format(accuracy = 0.01)) +
          ggtitle("The S&P 500 Has a Higher Daily Return\nWhen the McRib is Available") +
          labs(x = "McRib Status", y = "Average Daily Return (%)",
               caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# Do BTC analysis
file_path <- paste0(out_path, "/mcrib_btc_days.jpeg")

to_plot <- df_mcrib %>%
  group_by(mcrib) %>%
  summarise(ret_btc = mean(ret_btc, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mcrib) %>%
  mutate(label = paste0(round(100*ret_btc,2), "%"))

plot <- ggplot(to_plot, aes(x=mcrib, y=ret_btc, fill = mcrib)) +
  geom_bar(stat="identity") +
  geom_text(data=to_plot, aes(x=mcrib, y=ret_btc+0.00015, label=label),
            col = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(label = percent_format(accuracy = 0.01)) +
  ggtitle("Bitcoin Has a Higher Daily Return\nWhen the McRib is Available") +
  labs(x = "McRib Status", y = "Average Daily Return (%)",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #
