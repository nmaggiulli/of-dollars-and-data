cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0327_sp500_year_losses"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(year(date) >= 1900) %>%
                    select(date, price_plus_div) %>%
                    mutate(ret_1yr = lead(price_plus_div, 12)/price_plus_div - 1)

year_rets <- sp500_ret_pe %>%
              filter(month(date) == 1) %>%
              drop_na() %>%
              mutate(decade = floor(year(date)/10) * 10,
                     pos_ret = ifelse(ret_1yr > 0, 1, 0)) %>%
              filter(decade < 2020)

overall_pos <- mean(year_rets$pos_ret)

to_plot <- year_rets %>%
                group_by(decade) %>%
                summarise(pos_ret = mean(pos_ret)) %>%
                ungroup()

file_path <- paste0(out_path, "/decade_prob_pos_return.jpg")
source_string <- paste0("Source: Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and is adjusted for inflation."), 
                        width = 80)

plot <- ggplot(data = to_plot, aes(x=as.factor(decade), y=pos_ret)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Percentage of Years with a Positive Return\nU.S. Stocks")) +
  labs(x = paste0("Decade"), y = "Percentage of Positive Years",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #