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

folder_name <- "0128_amzn_nasdaq"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0129_ycharts_amzn_nasdaq/AMZN_IXIC_data.csv"))

colnames(raw) <- c("date", "index_amzn", "index_nq")

raw <- raw %>%
        filter(!is.na(index_amzn), !is.na(index_nq)) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
        filter(date >= "1999-05-28") %>%
        arrange(date)

dd_amzn <- drawdown_path(select(raw, date, index_amzn)) %>%
              mutate(ticker = "AMZN")
dd_nq <- drawdown_path(select(raw, date, index_nq)) %>%
              mutate(ticker = "IXIC")

min_amzn <- dd_amzn %>%
              filter(pct == min(dd_amzn$pct)) 

to_plot <- bind_rows(dd_amzn, dd_nq) %>%
              filter(date <= pull(min_amzn, date))

# Plot DD for each
file_path <- paste0(out_path, "/amzn_nq_to_bottom_dd.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Not adjusted for dividends or inflation."), 
                          width = 85)

date_of_interest <- as.Date("2001-07-20")
text_labels <- data.frame(date = c(date_of_interest,date_of_interest))

text_labels[1, "pct"] <- filter(to_plot,date ==date_of_interest, ticker == "AMZN") %>% pull(pct)
text_labels[1, "ticker"] <- paste0("AMZN")
text_labels[1, "label"] <- paste0("Amazon")

text_labels[2, "pct"] <- filter(to_plot, date == date_of_interest, ticker == "IXIC") %>% pull(pct)
text_labels[2, "ticker"] <- paste0("IXIC")
text_labels[2, "label"] <- paste0("Nasdaq")

plot <- ggplot(to_plot, aes(x=date, y=pct, col = ticker)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent, limits = c(-1, 0)) +
  scale_x_date(date_labels = "%m/%y") +
  geom_text_repel(data=text_labels, aes(x=date, y=pct, col = ticker),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$label == "Amazon", 0.05, 0.08)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Amazon Lost Nearly 95% Following the\nAmazon.bomb Article")) +
  labs(x="Date", y="Value Lost (%)",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Plot what happened after the bottom
to_plot <- raw %>%
            filter(date > pull(min_amzn, date))

post_amzn <- to_plot[1, "index_amzn"]
post_nq <- to_plot[1, "index_nq"]

to_plot <- to_plot %>%
              mutate(AMZN = index_amzn/post_amzn,
                     IXIC = index_nq/post_nq) %>%
              select(date, AMZN, IXIC) %>%
              gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/amzn_nq_from_bottom.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Not adjusted for dividends or inflation."), 
                          width = 85)

date_of_interest <- as.Date("2015-01-02")
text_labels <- data.frame(date = c(date_of_interest,date_of_interest))

text_labels[1, "value"] <- filter(to_plot,date ==date_of_interest, key == "AMZN") %>% pull(value)
text_labels[1, "key"] <- paste0("AMZN")
text_labels[1, "label"] <- paste0("Amazon")

text_labels[2, "value"] <- filter(to_plot, date == date_of_interest, key == "IXIC") %>% pull(value)
text_labels[2, "key"] <- paste0("IXIC")
text_labels[2, "label"] <- paste0("Nasdaq")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_labels = "%m/%y") +
  geom_text_repel(data=text_labels, aes(x=date, y=value, col = key),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$label == "Amazon", 85, 20)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Since its Bottom, Amazon Has Grown 300x!")) +
  labs(x="Date", y="Growth of $1",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #