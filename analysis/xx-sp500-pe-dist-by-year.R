cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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
plot_sp <- function(from_year, to_year, title){
  
  to_plot <- sp500_ret_pe %>%
                mutate(ret = (price_plus_div/lag(price_plus_div) - 1)) %>%
                filter(year >= from_year, year <= to_year) %>%
                mutate(decade = floor(year/10) * 10)
  
  print(tail(to_plot))
                
  file_path <- paste0(exportdir, "xx-sp-pe-dist-by-year/sp-price-",from_year, "-", to_year,".jpeg")
  
  plot <- ggplot(data = to_plot, aes(x=ret, y=factor(year), fill = factor(year))) +
    geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
    scale_fill_discrete(guide = FALSE) +
    scale_x_continuous(limits = c(-0.25, 0.25), label = percent) +
    of_dollars_and_data_theme +
    ggtitle(paste0(title)) +
    labs(x = "Monthly Return", y = "Year")

  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Real returns includes reinvested dividends.")

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

plot_sp(1990, 2017, "CAPE May Only Be Useful Near Its Extremes")

# ############################  End  ################################## #