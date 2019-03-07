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
library(tidyverse)

folder_name <- "xxxx_yahoo_return_10yr"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #


today <- Sys.Date() - 1
t_day <- day(today)
t_month <-month(today)

pull_yahoo_data <- function(ticker, full_name, from_year, to_year){

  getSymbols(ticker, from = paste0(from_year,'-', t_month, '-', t_day), to = paste0(to_year,'-', t_month, '-', t_day), 
             src="yahoo", periodicity = "daily")
  
  df <- data.frame(date = index(get(ticker)), 
                   get(ticker), row.names=NULL) %>%
    select(date, contains("Adjust")) %>%
    mutate(full_name = full_name)
    
    
  names(df) <- c("date", "value", "full_name")
    
  return(df)
}

tickers <- data.frame(ticker = c("AAPL", "NFLX", "GOOG", "DPZ", "MSFT", "AMZN"),
                      full_name = c("Apple", "Netflix", "Google", "Dominos", "Microsoft", "Amazon")
          )

for(t in 1:nrow(tickers)){
  tkr  <- tickers[t, "ticker"]
  full <- tickers[t, "full_name"]
  
  temp <- pull_yahoo_data(tkr, full, 2009, 2019) 
  
  temp <- temp %>%
            mutate(value = value/temp[1, "value"])
  
  assign("temp", temp, envir = .GlobalEnv)
  if(t == 1){
    to_plot <- temp 
  } else{
    to_plot <- bind_rows(to_plot, temp)
  }
}

max_date <- max(to_plot$date)

last_day <- to_plot %>%
              filter(date == as.Date(max_date))

file_path <- paste0(out_path, "/ten_year_challenge.jpg")    
source_string <- paste0("Source:  Yahoo Finance (OfDollarsAndData.com)")
note_string <- paste0("Note: Not adjusted for inflation.")

plot <- ggplot(to_plot, aes(x=date, y=value, col = full_name)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=last_day, aes(x=date, y=value, col=full_name),
                  label = paste0(last_day$full_name),
                  max.iter = 2000, 
                  nudge_x = ifelse(last_day$full_name %in% c("Netflix", "Dominos", "Amazon"), -555, 0),
                  segment.color = "transparent") +
  of_dollars_and_data_theme +
  labs(x="Date", y="Growth of $1",
       caption = paste0(source_string, "\n", note_string)) +
  ggtitle("#TenYearChallenge")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
 
# ############################  End  ################################## #
