cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(readxl)
library(tidyverse)

folder_name <- "_fl/0013_nyt_vs_jkb_book_sales"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/", folder_name, "/jkb_sales_extrapolation.xlsx"),
                  sheet = "raw")

to_plot <- raw %>%
            select(week, contains("sales_")) %>%
            rename(JKB = sales_jkb,
                   `NYT nonfiction bestsellers` = sales_nyt) %>%
            gather(-week, key = key, value = value)

file_path <- paste0(out_path, "/jkb_vs_nyt_nonfiction_bestseller_weekly.jpg")
source_string <- paste0("Source: Yucesoy et al. EPJ Data Science (2018) (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Shows the median weekly sales value of JKB and NYT nonfiction bestsellers."), 
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=week, y=value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("blue", "gray")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("JKB vs. NYT Nonfiction Bestsellers\nWeekly Sales")) +
  labs(x = "Week", y = "Weekly Sales",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do cumulative sales
to_plot <- raw %>%
  select(week, contains("cumulative_")) %>%
  rename(JKB = cumulative_jkb_proj,
         `NYT nonfiction bestsellers` = cumulative_nyt) %>%
  select(-cumulative_pct_jkb) %>%
  gather(-week, key = key, value = value)

file_path <- paste0(out_path, "/jkb_vs_nyt_nonfiction_bestseller_cumulative.jpg")
note_string <- str_wrap(paste0("Note: Shows the cumulative sales value of NYT nonfiction bestsellers and the projected cumulative sales of JKB."), 
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=week, y=value, col = key)) +
  geom_line() +
  geom_vline(xintercept = 20, linetype = "dashed", col = "black") +
  scale_y_continuous(label = comma) +
  scale_color_manual(values = c("blue", "gray")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("JKB vs. NYT Nonfiction Bestsellers\nCumulative Sales")) +
  labs(x = "Week", y = "Cumulative Sales",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do % of total sales
to_plot <- raw %>%
  select(week, cumulative_pct_jkb) 

file_path <- paste0(out_path, "/jkb_cumulative_pct.jpg")
note_string <- str_wrap(paste0("Note: Shows the projected percentage total of 1-year sales over time."), 
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=week, y=cumulative_pct_jkb)) +
  geom_line(col = "black") +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("JKB Projected Percentage of Total 1-Year Sales")) +
  labs(x = "Week", y = "Projected Percentage of Total 1-Year Sales",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #