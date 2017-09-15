cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Read in sentiment data
sentiment <- readRDS(paste0(localdir, "39-quandl-aaii-sentiment.Rds")) %>%
              gather(key=key, value=value, -date) %>%
              filter(!is.na(value))

n_smoothing_periods <- 24

# Calculate the annualized returns
for (i in n_smoothing_periods:nrow(sentiment)){
  if (sentiment[(i-n_smoothing_periods+1), "key"] == sentiment[i, "key"]){
    start_index <- i - n_smoothing_periods + 1
    sentiment[i, "index"] <- mean(sentiment[(start_index:i), "value"])
  } else {
    sentiment[i, "index"] <- NA
  }
}

# Functions to put strings into proper case
SentCase <- function(InputString){
  InputString <-
    paste(toupper(substring(InputString,1,1)),tolower(substring(InputString,2)),
          sep="")
}

ProperCase <- function(InputString){
  sapply(lapply(strsplit(InputString," "), SentCase), paste, collapse=" ")
}

plot_sentiment <- function(start_date, end_date, type, low_y, high_y){
  
  to_plot <- filter(sentiment, date >= start_date, date <= end_date,
                    key == type)
  
  if (type == "bullish"){
    top_title <- "Bullish Sentiment Broke Records\nNear the Peak of the Tech Bubble"
  } else {
    top_title <- "Bearish Sentiment Was Highest\nDuring the Great Recession"
  }
  
  file_path <- paste0(exportdir, "39-crowd/", type, "-",  start_date, "-", end_date,".jpeg")
  
  plot <- ggplot(to_plot, aes(x = date, y = index)) +
            geom_line() +
            ggtitle(top_title) +
            scale_y_continuous(label = percent, limits = c(low_y, high_y)) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            of_dollars_and_data_theme + 
            labs(x = "Year", y = paste0(ProperCase(type), " Sentiment (%)"))
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  Quandl, American Association of Individual Investors (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Sentiment measure has been smoothed over the previous ",  n_smoothing_periods, " weeks.") 
  
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

plot_sentiment("1996-01-01", "2002-12-31", "bullish", 0.2, 0.6)
plot_sentiment("2004-01-01", "2010-12-31", "bearish", 0.15, 0.55)


# ############################  End  ################################## #

  
