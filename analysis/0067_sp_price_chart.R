cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(quadprog)
library(lubridate)
library(fTrading)
library(quantmod)
library(tidyr)
library(ggjoy)
library(dplyr)

########################## Start Program Here ######################### #

# Read in data for sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                     filter(!is.na(cape))

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
}

# Change the date to a date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, cape, price_plus_div) %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")),
    year = year(date))

#Create function for S&P data
plot_sp <- function(from_year, to_year, lag_cape, title){
  
  to_plot <- sp500_ret_pe %>%
                mutate(final_date = lead(date) - 1,
                       cape = lag(cape, n = lag_cape)) %>%
                filter(year >= from_year, year <= to_year)
  
  for (i in 1:nrow(to_plot)){
    if (i == 1){
      to_plot[i, "index"] <- 100
    } else {
      to_plot[i, "index"] <- to_plot[(i-1), "index"] * (1 + (to_plot[(i), "price_plus_div"]/to_plot[(i-1), "price_plus_div"] - 1))
    }
  }
  
  file_path <- paste0(exportdir, "67-sp-price-chart/sp-price-",from_year, "-", to_year, "-lag-", lag_cape ,".jpeg")
  
  y_max <- max(to_plot$index)
  
  
  
  plot <- ggplot(data = to_plot, aes(x=date, y=index)) +
    geom_line() +
    of_dollars_and_data_theme +
    geom_vline(linetype = "dashed", xintercept = c(as.Date("1940-01-01"), as.Date("1980-01-01"))) +
    scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
    scale_x_date(date_labels = "%Y", breaks = seq(as.Date("1900-01-01"), as.Date("2000-12-31"), by="20 years")) +
    ggtitle(paste0(title)) +
    labs(x = "Date", y = "Real Index (Start = $100)")
  
  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Real index includes reinvested dividends.") 
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_sp(1900, 2017, 0, "U.S. Stocks Since 1900")

# ############################  End  ################################## #