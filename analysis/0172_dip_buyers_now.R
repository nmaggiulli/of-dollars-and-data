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
library(lemon)
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
                        min_dd = min(pct)) %>%
              ungroup() %>%
              select(dd_count, below_pct_date, min_dd) %>%
              distinct()

dd_starts <- dd %>%
              group_by(dd_count) %>%
              summarize(start_date = min(date),
                        end_date = max(date)) %>%
              ungroup() %>%
              select(dd_count, start_date, end_date) %>%
              distinct()

dd_groups <- dd_groups %>%
              left_join(dd_starts) %>%
              mutate(label = paste0(format.Date(start_date, format = "%b %Y"), 
                                   "-", 
                                   format.Date(end_date, format = "%b %Y")),
                     start_month = as.Date(paste0(year(start_date), "-", month(start_date), "-01")),
                    end_month = as.Date(paste0(year(end_date), "-", month(end_date), "-01")))

dd_label_order <- dd_groups %>%
                    arrange(start_date) %>%
                    select(label) %>%
                    as.vector()

dd_groups$label <- factor(dd_groups$label, levels = pull(dd_label_order))

dd_cutoff_string <- paste0(-100*dd_cutoff)

# Set note and source string
source_string <- str_wrap("Source: Bloomberg (OfDollarsAndData.com)",
                          width = 85)
note_string <- str_wrap(paste0("Note:  Dow price data does not include dividends."),
                        width = 85)

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
            select(date, day, pct, label)

file_path <- paste0(out_path, "/all_dow_dd_panel_", dd_cutoff_string, "_pct.jpeg")

plot <- ggplot(to_plot, aes(x=day, y=pct)) +
  geom_line() +
  geom_hline(yintercept = -0.3, linetype="dashed") +
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

file_path <- paste0(out_path, "/all_dow_dd_panel_", dd_cutoff_string, "_pct_subset.jpeg")

plot <- ggplot(to_plot, aes(x=day, y=pct)) +
  geom_line() +
  geom_hline(yintercept = -0.3, linetype="dashed") +
  geom_point(data=points, aes(x=day, y=pct), color = "red", alpha = 0.5) +
  facet_wrap(~label) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-.7, -0.1, 0.2), 
                     limits = c(-.9, 0)) +
  scale_x_continuous(label = comma, limits = c(0, 2000)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Dow Drawdown Greater Than ", -100*dd_cutoff, "%")) +
  labs(x = "Day" , y = "Percentage Off High",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

calc_dca_growth <- function(start_date, end_date){
  
  sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
    filter(date>= start_date, date <= end_date) %>%
    select(date, price_plus_div)
  
  ending_val <- pull(sp500[nrow(sp500), "price_plus_div"])
  
  
  to_plot <- sp500 %>%
    mutate(dca_growth = ending_val/price_plus_div * 100) %>%
    select(date, dca_growth)
  
 return(to_plot)
}

for(i in 1:(nrow(dd_groups)-1)){
  dd_start <- pull(dd_groups[i, "start_month"])
  dd_end <- pull(dd_groups[i, "end_month"])
  lbl <- pull(dd_groups[i, "label"])
  
  tmp <- calc_dca_growth(dd_start, dd_end) %>%
            mutate(label = lbl)
  
  if(i == 1){
    stacked <- tmp
  } else{
    stacked <- bind_rows(stacked, tmp)
  }
}

to_plot <- stacked

file_path <- paste0(out_path, "/dca_growth_all_", -100*dd_cutoff, "pct_dd.jpeg")
source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
note_string <- paste0("Note:  Performance data adjusted for dividends and inflation.") 

plot <- ggplot(to_plot, aes(x=date, y=dca_growth)) + 
  geom_bar(stat = "identity", fill = "black", width = 31) +
  facet_rep_wrap(label ~ ., scales = "free", repeat.tick.labels = c("left", "bottom")) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Growth of Each $100 Payment\ninto U.S. Stocks")) +
  labs(x = "Date" , y = "Final Amount",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot_1929 <- to_plot %>%
            filter(label == "Sep 1929-Nov 1954")

file_path <- paste0(out_path, "/dca_growth_1929_pct_dd.jpeg")

plot <- ggplot(to_plot_1929, aes(x=date, y=dca_growth)) + 
  geom_bar(stat = "identity", fill = "black", width = 31) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Final Growth of Each $100 Payment\ninto U.S. Stocks")) +
  labs(x = "Date" , y = "Final Amount",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Percent recovery plot
percent_loss_gain <- data.frame(loss = seq(0.01, 0.5, 0.01)) %>%
                      mutate(gain = 1/(1-loss) - 1)

file_path <- paste0(out_path, "/gain_to_recover_loss.jpeg")
source_string <- "Source:  Simulated Data (OfDollarsAndData.com)" 

plot <- ggplot(percent_loss_gain, aes(x=loss, y=gain)) + 
  geom_smooth(se = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("% Gain Needed to Fully Recover From % Loss")) +
  labs(x = "Loss" , y = "Gain Needed",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #