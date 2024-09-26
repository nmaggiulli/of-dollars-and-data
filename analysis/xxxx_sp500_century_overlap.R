cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(ggrepel)

folder_name <- "xxxx_sp500_century_overlap"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, cape != "NA")

first_year <- min(sp500_ret_pe$date)
last_year <- max(sp500_ret_pe$date)

# Convert the cape to a numeric
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)

create_period_return <- function(start_year, end_year){

  initial_value     <- filter(sp500_ret_pe, date == start_year) %>%
    pull(price_plus_div)
  
  ret_yr            <- filter(sp500_ret_pe, date >= start_year, date <= end_year) %>%
                        mutate(price = price_plus_div/initial_value, 
                               period = row_number(),
                               start_date = start_year) %>%
                        select(period, price, start_date, date)
  return(ret_yr)
}

ret_1900 <- create_period_return("1900-01-01", "1999-12-01")
ret_2000 <- create_period_return("2000-01-01", max(sp500_ret_pe$date))

max_year <- year(max(ret_2000$date))

n_years <- 100

to_plot <- bind_rows(ret_1900, ret_2000)

# Set the file_path for the next output
file_path = paste0(out_path, "/returns_by_century.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Annualized real returns include reinvested dividends.") 

plot <- ggplot(data = to_plot, aes(x = period, y = price, col = as.factor(start_date))) +
  geom_line(alpha = 0.5) +
  scale_x_continuous() +
  scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000)) +
  scale_color_discrete(guide = FALSE) +
  geom_text_repel(data = filter(to_plot, date == max(ret_2000$date)),
                  aes(x = period, 
                      y = 1,
                      col = as.factor(start_date),
                      label = paste0("2000-", max_year),
                      family = "my_font"),
                  nudge_y = -.3,
                  nudge_x = 7
  ) +
  geom_text_repel(data = filter(to_plot, date == max(ret_1900$date)),
                  aes(x = period, 
                      y = price,
                      col = as.factor(start_date),
                      label = "1900-1999",
                      family = "my_font")
  ) +
  ggtitle(paste0("U.S. Stocks During the 20th and 21st Century")) +
  of_dollars_and_data_theme +
  labs(x = "Month" , y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))



# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #