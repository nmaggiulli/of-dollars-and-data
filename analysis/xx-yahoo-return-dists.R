cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

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

# ############################  End  ################################## #

create_dist_compare <- function(ticker, period, title, from_year, to_year, max, unit){
  lowcase_period <- tolower(period)
  if (ticker != "BTC"){
    
  getSymbols(ticker, from = paste0(from_year,'-01-01'), to = paste0(to_year,'-12-31'), 
             src="yahoo", periodicity = lowcase_period)
    df <- data.frame(date = index(get(ticker)), 
                     get(ticker), row.names=NULL) %>%
      select(date, contains("Adjust"))
    names(df) <- c("date", "value")
    
    source_string <- paste0("Source:  Yahoo Finance (OfDollarsAndData.com)")
    
  } else {
    
    df <- readRDS(paste0(localdir, "27-quandl-bitcoin.Rds")) %>%
            filter(day(date) == 28)
    
    source_string <- paste0("Source:  Quandl (OfDollarsAndData.com)")
    
  }
  
  to_plot <-df %>%
            mutate(year = year(date),
                   month = month(date),
                   ret = (value/lag(value) - 1)) %>%
            select(year, month, ret)
  
  assign(ticker, to_plot, envir = .GlobalEnv)
  
  file_path <- paste0(exportdir, "xx-yahoo-return-dists/", ticker, "-", lowcase_period, "-distributions-",from_year, "-", to_year, ".jpeg")

  plot <- ggplot(data = to_plot, aes(x=ret, y=factor(year), fill = factor(year))) +
    geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(guide = FALSE) +
    scale_x_continuous(label = percent, limit = c(-max, max), breaks = seq(-max, max, unit)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(title)) +
    labs(x = paste0(period, " Return" ), y = "Year")
  
  note_string   <- paste0("Note:  Ticker shown is ", ticker ,".") 
  
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

#create_dist_compare("SPY", "Daily", "The Narrowing of Daily Volatility\nS&P 500", 1997, 2003, 0.05, 0.01)
create_dist_compare("SPY", "Monthly", "SPY", 2006, 2017, 0.25, 0.05)
create_dist_compare("VTV", "Monthly", "VTV", 2006, 2017, 0.25, 0.05)

# This was for Twitter for fun 
#create_dist_compare("BTC", "Monthly", "Bitcoin Monthly Return Distribution by Year", 2010, 2017, 1, 0.25)
