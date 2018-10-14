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

create_dist_compare <- function(ticker1, ticker2, period, title, from_year, to_year, max, unit){
  lowcase_period <- tolower(period)
    
  getSymbols(ticker1, from = paste0(from_year,'-01-01'), to = paste0(to_year,'-12-31'), 
             src="yahoo", periodicity = lowcase_period)
  
    df1 <- data.frame(date = index(get(ticker1)), 
                     get(ticker1), row.names=NULL) %>%
      select(date, contains("Adjust")) %>%
      mutate(ticker = ticker1)
    names(df1) <- c("date", "value", "ticker")
    
    print(head(df1))
    
    if (ticker2 != ""){
      getSymbols(ticker2, from = paste0(from_year,'-01-01'), to = paste0(to_year,'-12-31'), 
                 src="yahoo", periodicity = lowcase_period)
      
    df2 <- data.frame(date = index(get(ticker2)), 
                      get(ticker2), row.names=NULL) %>%
      select(date, contains("Adjust")) %>%
      mutate(ticker = ticker2)
    names(df2) <- c("date", "value", "ticker")
    
    to_plot <- bind_rows(df1, df2)

    print(head(df2))
    } else {
      to_plot <- df1
    }
    
  file_path <- paste0(exportdir, "51-when-do-you-sell/", ticker1, "-", ticker2, "-price-chart.jpeg")

  plot <- ggplot(to_plot, aes(x=date, y=value, col=ticker)) +
            geom_line() +
            of_dollars_and_data_theme +
            ggtitle(paste0(title)) +
            labs(x = paste0(period, " Return" ), y = "Price")
 
  source_string <- paste0("Source:  Yahoo Finance (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  ")
  
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

create_dist_compare("^N225", "", "Monthly", "Japan Stock Melt Up", 1980, 2017, 0.25, 0.05)

# ############################  End  ################################## #
