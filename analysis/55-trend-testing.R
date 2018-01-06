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
sp500_full    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
  filter(!is.na(cape)) %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")),
    year = year(date))

# Create a function to plot the sma vs buy and hold
plot_sma <- function(start_year, end_year, sma_months){

  sp500_ret_pe <- filter(sp500_full, 
                         year(date) >= start_year,
                         year(date) <= end_year)
  
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
  # Also drop data before the SMA
  sp500_ret_pe <- select(sp500_ret_pe, date, cape, price_plus_div, cash, sma, trend) %>%
                    filter(!is.na(trend))
  
  # Find first value for trend and buy and hold
  first_trend <- as.numeric(sp500_ret_pe[1, "trend"])
  first_bh    <- as.numeric(sp500_ret_pe[1, "price_plus_div"])
  
  # Create plotting data and set index to 100
  to_plot <- sp500_ret_pe %>%
                mutate(`Buy and Hold` = (price_plus_div/first_bh) * 100,
                       `Trend` = (trend/first_trend) * 100) %>%
                select(date, `Buy and Hold`, `Trend`) %>%
                gather(key=key, value=value, -date) 
  
  # Set adjust_x for the "Trend" label
  if (end_year - start_year > 50){
    adjust_x <- 10
  } else{
    adjust_x <- 5
  }
  
  # Set output path
  file_path <- paste0(exportdir, "55-trend-testing/trend-v-buy-hold-",start_year, "-", end_year, "-", sma_months, "-month-sma.jpeg")
  
  plot <- ggplot(to_plot, aes(x = date, y = value, col = as.factor(key))) +
            geom_line() +
            scale_color_discrete(guide = FALSE) +
            scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(100, 1000, 10000, 100000, 1000000)) +
            geom_text_repel(data = filter(to_plot, date ==  min(to_plot$date), key == "Buy and Hold"),
                            aes(x = date, 
                                y = value * 0.8,
                                col = as.factor(key),
                                label = key,
                                family = "my_font"),
                            segment.color = 'transparent'
            ) +
            geom_text_repel(data = filter(to_plot, date ==  max(to_plot$date), key == "Trend"),
                            aes(x = date, 
                                y = value,
                                col = as.factor(key),
                                label = key,
                                family = "my_font"),
                            nudge_x = -(365*adjust_x),
                            segment.color = 'transparent'
            ) +
            ggtitle(paste0("Trend vs. Buy and Hold\n", sma_months, "-Month Simple Moving Average"))  +
            of_dollars_and_data_theme +
            labs(x = "Year" , y = "Index (Start = 100)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source: http://www.econ.yale.edu/~shiller/data.htm  (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Moves to cash when ", sma_months, "-month SMA < current price.  Adjusted for inflation and dividends.")
  
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

years_list <- seq(1900, 1980, 10)

for (start in years_list){
  for (x in 3:18){
    plot_sma(start, 2017, x)
  }
}

# ############################  End  ################################## #