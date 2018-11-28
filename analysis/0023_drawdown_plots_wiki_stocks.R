cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

folder_name <- "0023_drawdown_plots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
all_wiki_stocks <- readRDS(paste0(localdir, "0023_wiki_single_stocks.Rds"))
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(Date > 1960)
  
# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(Date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
  loc_max <- 0
  for (i in 1:(nrow(vp))){
    if (vp[i, 2] < loc_max & i != 1){
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- vp[i, 2]/loc_max - 1
    } else{
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- 0
      loc_max  <- vp[i, 2]
    }
  }
  return(dd)
}

# Tickers to plot
# 
tickers <- c("AAPL", "AMZN", "XOM", "GE", "GS", "S&P 500")
  
# Have a start date
date_start <- "1997-05-15"

# Loop through tickers
for (t in 1:length(tickers)){
  if (t < length(tickers)){
    temp <- filter(all_wiki_stocks, ticker == tickers[t], Date >= date_start) %>%
              arrange(Date) %>%
              select(Date, price_plus_div)
    source_string <- "Source:  Quandl, http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"
    # Run function on specific data for drawdowns
    dd        <- drawdown_path(temp)
  } else {
    temp <- filter(sp500_ret_pe, Date >= date_start)
    # Run function on specific data for drawdowns
    dd        <- drawdown_path(temp)
  }
  print(tickers[t])
  print(sd(dd$pct))
  if (t == 1){
    to_plot <- cbind(dd, rep(tickers[t], nrow(dd)))
  } else{
    to_plot <- bind_rows(to_plot, cbind(dd, rep(tickers[t], nrow(dd))))
  }
}  

names(to_plot) <- c(names(to_plot[, 1:2]), "ticker")
  
      
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "0023_drawdown_plots/drawdowns-facet.jpeg")
  
  # Create title with ticker in subtitle
  top_title <- paste0("Individual Stock Drawdowns Can Be\nMore Extreme Than the Market")
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = Date, y = pct)) +
    geom_area(fill = "red") +
    facet_grid(~ticker) +
    ggtitle(top_title) +
    guides(fill = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(label = percent) +
    scale_x_date(date_labels = "%y") +
    labs(x = "Year (2000-2015)", y = "Percentage of Value Lost")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  note_string <- paste0("Note:  Adjusted for stock splits and dividends.") 
  
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

  
