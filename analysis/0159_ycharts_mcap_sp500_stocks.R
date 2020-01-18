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
library(zoo)
library(tidyverse)
library(tidylog)

folder_name <- "0159_ycharts_mcap_sp500_stocks"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "0159_ycharts_mcap_sp500_stocks/Historical Market Cap Data.xlsx"), skip = 5) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(date = as.Date(as.numeric(key), origin = "1900-01-01"),
         year = year(date),
         month = month(date),
         name = str_remove_all(str_remove_all(name, " Corp"), " Inc")) %>%
  filter(symbol != "", !is.na(value), symbol != "GOOG") %>%
  rename(mcap_millions = value) %>%
  select(date, year, month, symbol, name, mcap_millions) %>%
  arrange(symbol, date) %>%
  filter(year(date) >= 2010) %>%
  arrange(date, -mcap_millions) %>%
  group_by(date) %>%
  mutate(rank = row_number(),
         name = case_when(name == "International Business Machines" ~ "IBM",
                          name == "Alphabet" ~ "Alphabet (Google)",
                          TRUE ~ name),
         faamg = ifelse(name %in% c("Facebook", "Apple", "Amazon.com", "Microsoft", "Alphabet (Google)"), 1, 0)) %>%
  ungroup()

# Plot FAAMG market share over time
all_mcap <- raw %>%
              group_by(date) %>%
              summarize(mcap_total = sum(mcap_millions)) %>%
              ungroup()

to_plot <- raw %>%
                filter(faamg == 1) %>%
                group_by(date) %>%
                summarize(mcap_faamg = sum(mcap_millions)) %>%
                ungroup() %>%
                left_join(all_mcap) %>%
                mutate(pct_faamg = mcap_faamg/mcap_total)

file_path <- paste0(out_path, "/mcap_faamg.jpeg")
source_string <- "Source:  YCharts, 2010-2019 (OfDollarsAndData.com)"

number_labels <- to_plot %>%
  filter(date == max(to_plot$date) | date == min(to_plot$date)) %>%
  mutate(label = paste0(round(100*pct_faamg, 1), "%"))

plot <- ggplot(to_plot, aes(x=date, y=pct_faamg)) +
  geom_line() +
  geom_text_repel(data = number_labels, aes(x=date, y = pct_faamg),
                  label = number_labels$label,
                  size = 3,
                  nudge_y = 0.005,
                  segment.color = "transparent") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("FAAMG Stocks % Share of the S&P 500")) +
  labs(x="Date", y="Percentage of S&P 500",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do plot by top 2, 5, and 10 companies in S&P 500
tops <- c(2, 5, 10)
for(t in tops){
  top_n <- raw %>%
    filter(rank <= t)
  
  top_n_sum <- top_n %>%
    group_by(date) %>%
    summarize(mcap_top_n = sum(mcap_millions)) %>%
    ungroup() %>%
    select(date, mcap_top_n) 
  
  tmp <- raw %>%
    group_by(date) %>%
    summarize(mcap_year = sum(mcap_millions)) %>%
    ungroup() %>%
    left_join(top_n_sum) %>%
    mutate(pct_top_n = mcap_top_n/mcap_year) %>%
    rename_(.dots = setNames("pct_top_n", paste0("pct_top_", t))) %>%
    select(date, contains("pct_"))
  
  if(t == min(tops)){
    mcap_by_year <- tmp
  } else{
    mcap_by_year <- mcap_by_year %>%
                      left_join(tmp)
  }
}

to_plot <- mcap_by_year %>%
            gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/mcap_tiers.jpeg")
source_string <- "Source:  YCharts, 2010-2019 (OfDollarsAndData.com)"

text_labels <- to_plot %>%
                  filter(date == "2017-12-02") %>%
                  mutate(label = case_when(
                    grepl("_2", key) ~ "Top 2",
                    grepl("_5", key) ~ "Top 5",
                    grepl("_10", key) ~ "Top 10",
                    TRUE ~ ""
                  ))

number_labels <- to_plot %>%
  filter(date == max(to_plot$date) | date == min(to_plot$date)) %>%
  mutate(label = paste0(round(100*value, 1), "%"))

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  geom_text_repel(data = text_labels, aes(x=date, y = value, col = key),
                  label = text_labels$label,
                  nudge_y = 0.02,
                  segment.color = "transparent") +
  geom_text_repel(data = number_labels, aes(x=date, y = value, col = key),
                  label = number_labels$label,
                  size = 3,
                  nudge_y = 0.005,
                  segment.color = "transparent") +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Share of Largest Companies in the S&P 500")) +
  labs(x="Date", y="Percentage of S&P 500",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot top 10 companies by date over time
file_path <- paste0(out_path, "/mcap_top_10.jpeg")

to_plot <- top_n %>%
            mutate(month = month(date),
                   mcap_billions = mcap_millions/1000,
                   label = paste0("$", formatC(mcap_billions, digits = 0, format = "f", big.mark = ","))) %>%
            filter(month == 12) %>%
            mutate(tech = case_when(
              name %in% c("Alphabet (Google)", "Apple", "Microsoft", "Amazon.com", "Facebook") ~ 1,
              TRUE ~ 0
            ))

unique(to_plot$name)

plot <- ggplot(to_plot, aes(x=rank, y=mcap_billions, fill = as.factor(tech))) +
  geom_bar(stat = "identity") +
  geom_text(data=to_plot, aes(x=rank, y=mcap_billions, label = label, col = as.factor(tech), hjust=-0.1), size = 3) +
  geom_text(aes(y = 0, label = paste(name, " "), col = as.factor(tech)), vjust = 0.2, hjust = 1, size = 3) +
  scale_color_manual(guide = FALSE, values = c("black", "blue")) +
  scale_fill_manual(guide = FALSE, values = c("black", "blue")) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(label = dollar, limits = c(0, 1250)) +
  scale_x_reverse(breaks = seq(1, 10)) +
  of_dollars_and_data_theme +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = margin(1, 2, 1, 3.5, "cm"),
        plot.caption     = element_text(hjust = 0, family = "my_font", size = 8)) +
  ggtitle(paste0("Market Cap of 10 Largest Companies in S&P 500\n{closest_state}")) +
  labs(x=" ", y="Market Cap (in billions)",
       caption = paste0(source_string)) +
  transition_states(year, transition_length = 4, state_length = 4)

animate <- 0

if(animate == 1){
  anim <- animate(plot, fps = 7)
  
  anim_save(filename = paste0("top_10_companies_2010_2019.gif"), animation = anim, path = out_path)
}


# ############################  End  ################################## #