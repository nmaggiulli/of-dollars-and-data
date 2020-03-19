cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(readxl)
library(tidyverse)

folder_name <- "0172_dip_buyers_now"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index"))

dd <- drawdown_path(dow, 1)

dd_cutoff <- -0.30

dd_groups <- dd %>%
              filter(pct < dd_cutoff) %>%
              group_by(dd_count) %>%
              summarize(below_pct_date = min(date),
                        end_date = max(date),
                        min_dd = min(pct)) %>%
              ungroup() %>%
              select(dd_count, below_pct_date, end_date, min_dd) %>%
              distinct()

dd_starts <- dd %>%
              group_by(dd_count) %>%
              summarize(start_date = min(date)) %>%
              ungroup() %>%
              select(dd_count, start_date) %>%
              distinct()
              

dd_groups <- dd_groups %>%
              left_join(dd_starts) %>%
              mutate(label = paste0(format.Date(start_date, format = "%b %Y"), 
                                   "-", 
                                   format.Date(end_date, format = "%b %Y")))

dd_label_order <- dd_groups %>%
                    arrange(start_date) %>%
                    select(label) %>%
                    as.vector()

dd_groups$label <- factor(dd_groups$label, levels = pull(dd_label_order))

dd_below_cutoff <- dd %>%
                      inner_join(dd_groups) %>%
                      filter(date >= below_pct_date)

for(i in 1:nrow(dd_below_cutoff)){
  if(i == 1){
    dd_below_cutoff[i, "day"] <- 1
  } else{
    if(dd_below_cutoff[i, "dd_count"] == dd_below_cutoff[(i-1), "dd_count"]){
      dd_below_cutoff[i, "day"] <- dd_below_cutoff[(i-1), "day"] + 1
    } else{
      dd_below_cutoff[i, "day"] <- 1
    }
  }
}

to_plot <- dd_below_cutoff

dd_cutoff_string <- paste0(-100*dd_cutoff)

# Set note and source string
source_string <- str_wrap("Source: StockCharts (OfDollarsAndData.com)",
                          width = 85)
note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends."),
                        width = 85)

file_path <- paste0(out_path, "/dow_dd_", dd_cutoff_string, "_pct.jpeg")

plot <- ggplot(to_plot, aes(x=day, y=pct, col = as.factor(below_pct_date))) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-0.8, 0, 0.1)) +
  scale_x_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Dow Drawdown Greater Than ", -100*dd_cutoff, "%")) +
  labs(x = "Day" , y = "Percentage Off High",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/dow_dd_", dd_cutoff_string, "_pct_subset.jpeg")

plot <- ggplot(to_plot, aes(x=day, y=pct, col = as.factor(below_pct_date))) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-0.8, 0, 0.1)) +
  scale_x_continuous(label = comma, limits = c(0, 1000)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Dow Drawdown Greater Than ", -100*dd_cutoff, "%")) +
  labs(x = "Day" , y = "Percentage Off High",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

dd_start_below_cutoff <- dd %>%
  inner_join(dd_groups) %>%
  filter(date >= start_date)

for(i in 1:nrow(dd_start_below_cutoff)){
  if(i == 1){
    dd_start_below_cutoff[i, "day"] <- 1
  } else{
    if(dd_start_below_cutoff[i, "dd_count"] == dd_start_below_cutoff[(i-1), "dd_count"]){
      dd_start_below_cutoff[i, "day"] <- dd_start_below_cutoff[(i-1), "day"] + 1
    } else{
      dd_start_below_cutoff[i, "day"] <- 1
    }
  }
}

to_plot <- dd_start_below_cutoff 

points <- to_plot %>%
            filter(pct == min_dd) %>%
            select(day, pct, label)

file_path <- paste0(out_path, "/all_dow_dd_panel_", dd_cutoff_string, "_pct.jpeg")

plot <- ggplot(to_plot, aes(x=day, y=pct)) +
  geom_line() +
  geom_point(data=points, aes(x=day, y=pct), color = "red", alpha = 0.5) +
  facet_wrap(~label) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-1, 0, 0.25)) +
  scale_x_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Dow Drawdown Greater Than ", -100*dd_cutoff, "%")) +
  labs(x = "Day" , y = "Percentage Off High",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #