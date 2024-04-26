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
library(Hmisc)
library(xtable)
library(tidyverse)

folder_name <- "0399_election_years"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0399_election_years/SPX_data.csv")) %>%
          mutate(date = as.Date(Period),
                 index_sp500 = `S.P.500...SPX..Level`) %>%
          arrange(date) %>%
          mutate(day_of_week = weekdays(date),
                 month = month(date),
            election_year = case_when(
            year(date) %% 4 == 0 ~ 1,
            TRUE ~ 0
            )
          ) %>%
          filter(year(date) <= 2023) %>%
          select(date, month, day_of_week, index_sp500, election_year)

df <- raw

post_election <- 0
for(i in 1:nrow(df)){
  month <- df[i, "month"]
  day_of_week <- df[i, "day_of_week"]
  day <- day(df[i, "date"])
  
  if(month >= 11){
    if(month == 11 & day_of_week == "Monday" & day < 8){
      df[i, "post_election"] <- 0
      post_election <- -1
    } else{
      if(post_election == -1){
        df[i, "post_election"] <- 0
        post_election <- 1
      } else{
        df[i, "post_election"] <- post_election
      }
    }
  } else{
    post_election <- 0
    df[i, "post_election"] <- 0
  }
}

post_election_data <- df %>%
                        filter(post_election == 1) %>%
                        mutate(year = year(date))

post_start <- post_election_data %>%
                group_by(year) %>%
                summarise(date = min(date)) %>%
                ungroup() %>%
                inner_join(post_election_data) %>%
                rename(start_date = date,
                       start_index = index_sp500) %>%
                select(year, start_date, start_index, election_year)

post_end <- post_election_data %>%
                group_by(year) %>%
                summarise(date = max(date)) %>%
                ungroup() %>%
                inner_join(post_election_data) %>%
                rename(end_date = date,
                       end_index = index_sp500) %>%
                select(year, end_date, end_index)

post_election_merge <- post_start %>%
                          left_join(post_end) %>%
                          mutate(ret = end_index/start_index - 1)

post_election_summary <- post_election_merge %>%
                          group_by(election_year) %>%
                          summarise(n_years = n(),
                            ret_25pct = quantile(ret, probs = 0.25),
                            ret_50pct = quantile(ret, probs = 0.5),
                            ret_75pct = quantile(ret, probs = 0.75),
                            mean_ret = mean(ret)) %>%
                          ungroup()

to_plot <- post_election_data %>%
              left_join(post_start) %>%
              mutate(pct_change = index_sp500/start_index - 1) %>%
              group_by(year) %>%
              mutate(day = row_number()) %>%
              ungroup() %>%
              filter(day <= 35)

#Plot post election
file_path <- paste0(out_path, "/all_post_election_years_1950_2023.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Includes years without a Presidential election, where the Tuesday following the first Monday in November is assumed to be an election day. ",
                                "Performance does not include dividends and is not adjusted for inflation.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=day, y=pct_change, col = as.factor(year))) +
  geom_line() +
  scale_color_discrete(guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Stock Performance After Election Day\nBy Year")) +
  labs(x="Days After Election Day", y="Percentage Change",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do average of election to non-election years
to_plot_avg <- to_plot %>%
              group_by(election_year, day) %>%
              summarise(pct_change = mean(pct_change)) %>%
              ungroup() %>%
              mutate(election_year = case_when(
                election_year == 1 ~ "Election Year",
                TRUE ~ "Non-election Year"
              ))

file_path <- paste0(out_path, "/avg_post_election_years_1950_2023.jpeg")

text_labels <- data.frame()

text_labels[1, "day"] <- 15
text_labels[1, "pct_change"] <- .015
text_labels[1, "election_year"] <- "Election Year"
text_labels[1, "label"] <- text_labels[1, "election_year"] 

text_labels[2, "day"] <- 25
text_labels[2, "pct_change"] <- .005
text_labels[2, "election_year"] <- "Non-election Year"
text_labels[2, "label"] <- text_labels[2, "election_year"] 

plot <- ggplot(to_plot_avg, aes(x=day, y=pct_change, col = as.factor(election_year))) +
  geom_line() +
  geom_text(data = text_labels,
            aes(x=day, y=pct_change, col = as.factor(election_year),
                label = label)) +
  scale_color_manual(values = c("red", "gray"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Average U.S. Stock Performance (Post Election)\nElection vs. Non-Election Year")) +
  labs(x="Days After Election Day", y="Percentage Change",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Trump and Biden
to_plot_tb <- to_plot %>%
                filter(election_year == 1, year >= 2016) %>%
                mutate(key = case_when(
                  year == 2016 ~ "Trump (2016)",
                  TRUE ~ "Biden (2020)"
                ),
                label = paste0("+", round(100*pct_change, 0), "%"))

file_path <- paste0(out_path, "/trump_biden_post_election.jpeg")

text_labels <- data.frame()

text_labels[1, "day"] <- 15
text_labels[1, "pct_change"] <- .07
text_labels[1, "key"] <- "Biden (2020)"
text_labels[1, "label"] <- text_labels[1, "key"] 
text_labels[2, "day"] <- 35
text_labels[2, "pct_change"] <- to_plot_tb %>% filter(day == 35, key == "Biden (2020)") %>% pull(pct_change) + 0.005
text_labels[2, "key"] <- "Biden (2020)"
text_labels[2, "label"] <- to_plot_tb %>% filter(day == 35, key == "Biden (2020)") %>% pull(label)

text_labels[3, "day"] <- 25
text_labels[3, "pct_change"] <- .03
text_labels[3, "key"] <- "Trump (2016)"
text_labels[3, "label"] <- text_labels[3, "key"] 
text_labels[4, "day"] <- 35
text_labels[4, "pct_change"] <- to_plot_tb %>% filter(day == 35, key == "Trump (2016)") %>% pull(pct_change) + 0.005
text_labels[4, "key"] <- "Trump (2016)"
text_labels[4, "label"] <- to_plot_tb %>% filter(day == 35, key == "Trump (2016)") %>% pull(label)

plot <- ggplot(to_plot_tb, aes(x=day, y=pct_change, col = as.factor(key))) +
  geom_line() +
  geom_text(data = text_labels,
            aes(x=day, y=pct_change, col = as.factor(key),
                label = label)) +
  scale_color_manual(values = c("blue", "red"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Stock Performance\nBiden vs. Trump")) +
  labs(x="Days After Election Day", y="Percentage Change",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #