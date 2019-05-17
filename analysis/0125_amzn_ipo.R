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
library(tidyverse)

folder_name <- "0125_stop_financial_pornography"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

amzn <- read.csv(paste0(importdir, "0125_ycharts_amzn/amzn_data.csv")) %>%
          mutate(date = as.Date(Period),
                 index = `Amazon.com.Inc.Total.Return.Price`) %>%
          select(date, index) %>%
          arrange(date)

first_value <- amzn[1, "index"]

to_plot <- amzn %>%
            mutate(index = (index/first_value) * 1000)

dd <- drawdown_path(to_plot)

# Reset file path
file_path <- paste0(out_path, "/amzn_since_ipo.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Includes total return, but not adjusted for inflation."), 
                          width = 85)

# Dates of interest
tech_bubble_high <- as.Date("1999-12-10")
tech_bubble_low <- as.Date("2001-09-28")

text_labels <- data.frame(date = c(tech_bubble_high, tech_bubble_low))

text_labels[1, "index"] <- filter(to_plot, date == tech_bubble_high) %>% pull(index)
text_labels[1, "label"] <- paste0("$", formatC(round(text_labels[1, "index"],0), 
                                               big.mark = ",", format = "f", digits = 0))
text_labels[2, "index"] <- filter(to_plot, date == tech_bubble_low) %>% pull(index)
text_labels[2, "label"] <- paste0("$",  formatC(round(text_labels[2, "index"],0), 
                                                big.mark = ",", format = "f", digits = 0))

text_labels <- text_labels %>%
                mutate(date = as.Date(date))

plot <- ggplot(to_plot, aes(x=date, y=index)) +
  geom_line() +
  geom_point(data=text_labels, aes(x=date, y=index), col = ifelse(text_labels$date == tech_bubble_high, "green","red")) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  geom_text_repel(data=text_labels, aes(x=date, y=index),
                  color = ifelse(text_labels$date == tech_bubble_high, "green","red"),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$date == tech_bubble_high, 0.15, -0.15)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Holding Amazon Since its IPO Would Have\nBeen Quite a Challenge")) +
  labs(x="Date", y="Growth of $1000",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #