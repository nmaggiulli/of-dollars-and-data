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
library(zoo)
library(Hmisc)
library(tidyverse)

folder_name <- "0168_the_worst_of_days"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read.csv(paste0(importdir, "/0168_worst_days/SPX_data.csv"),
                       col.names = c("date","index_sp500")) %>%
  mutate(date = as.Date(date),
         date_qtr = as.Date(as.yearqtr(date) - 1/4, frac = 1) + days(1)) %>%
  arrange(date) %>%
  mutate(ret = index_sp500/lag(index_sp500) - 1)

first_year <- year(min(spx$date))
last_year <- year(max(spx$date))

ret_cutoff <- -0.05

to_plot <- spx %>%
                  filter(ret < ret_cutoff) %>%
                  arrange(-ret) %>%
                  mutate(date = as.character(date),
                         label = paste0(round(100*ret, 1), "%"),
                         flag = ifelse(date == "2020-03-09", 1, 0))

text_labels <- to_plot %>%
                filter(ret == max(to_plot$ret) | ret == min(to_plot$ret) | date == "2020-03-09")

file_path <- paste0(out_path, "/spx_worst_days_bar.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  There were ", 
                               nrow(to_plot), 
                               " trading days with a ", -100*ret_cutoff, "%+ decline since ", first_year, "."), 
                        width = 85)

plot <- ggplot(to_plot, aes(x=reorder(date, ret), y=ret, fill = as.factor(flag))) + 
  geom_bar(stat = "identity") +
  geom_text(data=text_labels, aes(x=reorder(date, ret), y=ret, label = label), 
            col = ifelse(text_labels$date == as.Date("2020-03-09"), "red", chart_standard_color), 
            vjust = 1.3, 
            hjust = 0.38, 
            size = 2.3) +
  scale_fill_manual(values = c(chart_standard_color, "red"), guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("All ", 100*ret_cutoff, "%+ Days for S&P 500 Since ", first_year)) +
  labs(x = "Date" , y = "1-Day Return",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

all_worst_dates <- spx %>%
                mutate(lead_date = lead(date, 250)) %>%
                filter(ret < ret_cutoff) %>%
                select(date, lead_date)

for(d in 1:nrow(all_worst_dates)){
  
  my_date <- all_worst_dates[d, "date"]
  future_date <- all_worst_dates[d, "lead_date"]
  
  tmp <- spx %>%
    filter(date > my_date, date <= future_date) %>%
    mutate(start_date = as.character(my_date)) %>%
    select(date, index_sp500, start_date)
  
  first_value <- tmp[1, "index_sp500"]
  
  tmp <- tmp %>%
    mutate(index_sp500 = index_sp500/first_value,
           day = row_number())
  
  if(my_date == min(all_worst_dates$date)){
    to_plot <- tmp
  } else{
    to_plot <- bind_rows(to_plot, tmp)
  }
}


avg <- to_plot %>%
  group_by(day) %>%
  summarise(index_sp500 = mean(index_sp500, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(start_date = "2100-01-01")

final_avg <- avg[nrow(avg), "index_sp500"] - 1

to_plot <- to_plot %>%
  bind_rows(avg)

n_days <- length(unique(to_plot$start_date))

text_labels <- avg %>%
  filter(day == 250) %>%
  mutate(label = "Average")

last_day <- to_plot %>%
  filter(day == 250)

file_path <- paste0(out_path, "/fwd_rwt_1_year_after_worst_days.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  There were ", 
                               nrow(all_worst_dates), 
                               " trading days with a ", -100*ret_cutoff, "%+ decline since ", first_year, "."), 
                        width = 85)

plot <- ggplot(to_plot, aes(x=day, y=index_sp500, col = as.factor(start_date))) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text_repel(data=text_labels, aes(x=day, y=index_sp500, label=label),
                  col = "red",
                  nudge_y = 0.02,
                  segment.colour = "transparent") +
  scale_color_manual(guide = FALSE, values = c(rep("gray", n_days-1), "red")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Over Next Year\nFollowing a ", 100*ret_cutoff, "%+ Day")) +
  labs(x = "Session" , y = "Growth of $1",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

## Plot price and drop days
to_plot <- spx %>%
            mutate(worst = ifelse(ret < ret_cutoff, 1, 0))

points <- to_plot %>%
            filter(worst == 1)

file_path <- paste0(out_path, "/spx_and_worst_days.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  There were ", 
                               nrow(all_worst_dates), 
                               " trading days with a ", -100*ret_cutoff, "%+ decline since ", first_year, "."), 
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=index_sp500)) + 
  geom_line() +
  geom_point(data=points, aes(x=date, y=index_sp500), col = "red", alpha = 0.5) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 and All ", 100*ret_cutoff, "%+ Days")) +
  labs(x = "Date" , y = "Index Value",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #