cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

# Read in data for sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
  filter(!is.na(cape))

# Set simple moving average month for trend
sma_months <- 10

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
  
  if (i >= sma_months){
    sp500_ret_pe[i, "sma"] <- colMeans(sp500_ret_pe[(i - sma_months + 1):i, "price_plus_div"])
    if (i == sma_months){
      sp500_ret_pe[i, "trend"] <- sp500_ret_pe[i, "price_plus_div"]
      sp500_ret_pe[i, "cash"]  <- 1
    } else {
      if (sp500_ret_pe[(i - 1), "price_plus_div"] > sp500_ret_pe[(i - 1), "sma"]){
        sp500_ret_pe[i, "cash"] <- 0
        sp500_ret_pe[i, "trend"] <- sp500_ret_pe[(i - 1), "trend"] * (1 + (sp500_ret_pe[i, "price_plus_div"]/sp500_ret_pe[(i - 1), "price_plus_div"] - 1))
      } else {
        sp500_ret_pe[i, "cash"]  <- 1
        sp500_ret_pe[i, "trend"] <- sp500_ret_pe[(i - 1), "trend"]
      }
    }
  }
}

# Change the date to a date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, cape, price_plus_div, cash, sma, trend) %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")),
    year = year(date))

# Set output path
file_path <- paste0(exportdir, "55-trend-testing/10-month-trend-test.jpeg")

plot <- ggplot(to_plot, aes(x = flip, y = sum, col = as.factor(sim))) +
          geom_line() +
          scale_color_discrete(guide = FALSE) +
          scale_x_continuous(breaks = seq(1, 10)) +
          scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
          ggtitle(paste0("Equally Random Coin Flip Sequences"))  +
          of_dollars_and_data_theme +
          labs(x = "Flip" , y = "Cumulative Heads")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Shows two simulations of flipping a fair coin 10 times.")

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

# ############################  End  ################################## #