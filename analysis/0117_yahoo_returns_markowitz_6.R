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

folder_name <- "0117_yahoo_returns_markowitz_6"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #


today <- Sys.Date() - 1
t_day <- day(today)
t_month <-month(today)

pull_yahoo_data <- function(ticker, full_name, from_year, to_year){

  getSymbols(ticker, from = paste0(from_year,'-02-03'), to = paste0(to_year,'-', t_month, '-', t_day), 
             src="yahoo", periodicity = "daily")
  
  df <- data.frame(date = index(get(ticker)), 
                   get(ticker), row.names=NULL) %>%
    select(date, contains("Adjust")) %>%
    mutate(full_name = full_name)
    
    
  names(df) <- c("date", "value", "full_name")
    
  return(df)
}

tickers <- data.frame(ticker = c("WY", "USG", "GLW", "CAT", "MMM", "UTX", "SPY"),
                      full_name = c("Weyerhaeuser", "USG", "Corning", "Caterpillar", "3M", "United Technologies", "S&P 500")
          )

for(t in 1:nrow(tickers)){
  tkr  <- tickers[t, "ticker"]
  full <- tickers[t, "full_name"]
  
  temp <- pull_yahoo_data(tkr, full, 2018, 2019) 
  
  temp <- temp %>%
            mutate(value = value/temp[1, "value"])
  
  assign("temp", temp, envir = .GlobalEnv)
  if(t == 1){
    to_plot <- temp 
  } else if (t != 7) {
    to_plot <- bind_rows(to_plot, temp)
  } else if(t == 7){
    to_plot <- to_plot %>%
                  group_by(date) %>%
                  summarize(value = sum(value)/6) %>%
                  ungroup() %>%
                  mutate(full_name = "6 Stock\nPortfolio") %>%
                  bind_rows(temp)
  }
}

max_date <- max(to_plot$date)

last_day <- to_plot %>%
              filter(date == as.Date(max_date) - 13)

file_path <- paste0(out_path, "/markowitz_equity_portfolio.jpg")    
source_string <- paste0("Source:  Yahoo Finance (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: The Markowitz '6 Stock Portfolio' is an equally weighted portfolio  ", 
                                "of Weyerhaeuser, USG, Corning, Caterpillar, 3M,  and United Technologies."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y=value, col = full_name)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=last_day, aes(x=date, y=value, col=full_name),
                  label = paste0(last_day$full_name),
                  max.iter = 2000, 
                  nudge_y = ifelse(last_day$full_name %in% c("S&P 500"), 0.02, -.05),
                  segment.color = "transparent") +
  of_dollars_and_data_theme +
  labs(x="Date", y="Growth of $1",
       caption = paste0(source_string, "\n", note_string)) +
  ggtitle("Even Harry Markowitz Makes Mistakes")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
 
# ############################  End  ################################## #
