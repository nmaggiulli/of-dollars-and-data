cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "0511_debasement_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in sp500 Shiller 
sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  filter(date >= as.Date("2020-01-01"))

first <- pull(sp500_ret_pe[1, "price_plus_div"])
first_p <- pull(sp500_ret_pe[1, "price"])
first_cpi <- pull(sp500_ret_pe[1, "cpi"])

df <- sp500_ret_pe %>%
          mutate(pct_change_real = price_plus_div/first-1,
                 price_growth = price/first_p,
                 cpi_change = cpi/first_cpi) %>%
          select(date, pct_change_real, price_growth, cpi_change)

to_plot <- df

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/sp500_2020_2026_may_pct_change.jpeg")
source_string <- "Source: Shiller data (OfDollarsAndData.com)"
note_string <- "Note: Performance includes dividends and is adjusted for inflation."

plot <- ggplot(to_plot, aes(x = date, y = pct_change_real)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Total Inflation-Adjusted Return")) +
  labs(x = "Year" , y = "Percentage Change",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #