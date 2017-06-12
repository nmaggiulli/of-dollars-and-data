cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)

########################## Start Program Here ######################### #

# Read in data for the Japanese CPI and stock market 
jpy_cpi <- readRDS(paste0(localdir, "28-fred-cpi-japan.Rds")) %>%
            select(year, index)

# Bring in stock market data, change the date format, and adjust for CPI
jpy_quandl <- readRDS(paste0(localdir, "28-quandl-japan.Rds")) %>%
              mutate(close_price = `Close Price`,
                     date = Date,
                     year = year(date),
                     month = month(date),
                     day = day(date)) %>%
              select(date, close_price, year, month, day) %>%
              group_by(year, month) %>%
              arrange(year, month, day) %>%
              filter(row_number() == 1, date < "2017-01-01") %>%
              ungroup() %>%
              mutate(date = year + month/100) %>%
              select(date, year, close_price) %>%
              left_join(jpy_cpi) %>%
              mutate(real_price = close_price/index * 100) %>%
              select(date, real_price)

sp500 <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                filter(cape != "NA", Date < 2017.01, Date >= 1960.01) %>%
                mutate(date = Date) %>%
                select(date, real_price)

# Set some parameters for the DCA
monthly_savings <- 100

# Years vector
years_vector <- seq(1960, 2002, 2)

for (i in 1:length(years_vector)){
  prices_jpy <- filter(jpy_quandl, date >= years_vector[i])
  prices_us  <- filter(sp500, date >= years_vector[i])
  
  for (j in 1:nrow(prices_jpy)){
    if (j == 1){
      prices_jpy[j, "japan"]       <- monthly_savings
      prices_jpy[j, "cost_basis"]  <- monthly_savings
      prices_us[j, "us"]           <- monthly_savings
    } else {
      prices_jpy[j, "japan"]      <- prices_jpy[(j-1), "japan"] * (prices_jpy[j, "real_price"] / prices_jpy[(j-1), "real_price"]) + monthly_savings
      prices_jpy[j, "cost_basis"] <- prices_jpy[(j-1), "cost_basis"] + monthly_savings
      prices_us[j, "us"]          <- prices_us[(j-1), "us"] * (prices_us[j, "real_price"] / prices_us[(j-1), "real_price"]) + monthly_savings
    }
  }
  
  prices_combined <- prices_jpy %>%
                      select(-real_price) %>%
                      left_join(prices_us) %>%
                      select(-real_price) %>%
                      bind_cols(data.frame(months = rep(seq(1, 12), nrow(prices_jpy)/12))) %>%
                      mutate(date = as.Date(paste0(round(date, 0), "-", months, "-01"))) %>%
                      select(-months)
  
  # Reshape the data
  to_plot <- prices_combined %>%
              gather(key, value, -date) 

  
  # Set file path
  file_path = paste0(exportdir, "28-quandl-japan/plot-", years_vector[i] ,".jpeg")
  
 plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
   geom_line() +
   scale_color_discrete(guide = FALSE) +
   scale_y_continuous(label = dollar) + 
   of_dollars_and_data_theme +
   labs(x = "Date", y = "Value") +
   ggtitle(paste0("Dollar Cost Averaging in Japan vs. U.S.\nStarting in ", years_vector[i]))
 
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source: Quandl, FRED, Robert Shiller (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Shows real price changes with a monthly investment of $", monthly_savings, ".")
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                         gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                         gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 10 loop -0 *.jpeg all_plots.gif


# ############################  End  ################################## #

  
