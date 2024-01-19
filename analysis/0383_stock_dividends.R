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
library(tidyverse)

folder_name <- "0383_stock_dividends"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

sp500_ret_pe <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds"))

gd <- sp500_ret_pe %>%
        filter(date >= "1929-09-01",
               date <= "1937-03-01")

first <- gd %>%
            head(1) %>%
            select(real_div, real_price) %>%
            rename(first_div = real_div,
                   first_price = real_price) %>%
              mutate(joinkey = 1)

to_plot <- gd %>%
              mutate(joinkey = 1) %>%
              left_join(first) %>%
              mutate(`Real Dividends` = real_div/first_div - 1,
                     `Real Prices` = real_price/first_price - 1) %>%
              select(date, `Real Dividends`, `Real Prices`) %>%
              gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/great_depression_change_in_price_v_div.jpeg")
source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures shown are adjusted for inflation."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1935-10-01")
text_labels[2, "date"] <- as.Date("1935-10-01")

text_labels[1, "value"] <- 0
text_labels[2, "value"] <- -0.75

text_labels[1, "key"] <- "Real Dividends"
text_labels[2, "key"] <- "Real Prices"
text_labels[1, "label"] <- "Real Dividends"
text_labels[2, "label"] <- "Real Prices"

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=date, y=value, col = key, label = label),
            family = my_font) +
  scale_y_continuous(label = percent, breaks = seq(0, -0.8, -0.1)) +
  scale_x_date(date_labels = "%m/%Y") +
  scale_color_manual(values = c("blue", "black"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Change in Real U.S. Stock Price & Dividends\n", 
                 format.Date(min(to_plot$date), "%b %Y"), 
                             " - ",
                 format.Date(max(to_plot$date), "%b %Y"))) +
  labs(x = "Date", y = "Percentage Change",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #