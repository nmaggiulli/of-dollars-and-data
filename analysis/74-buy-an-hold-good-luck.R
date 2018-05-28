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
library(lubridate)
library(fTrading)
library(quantmod)
library(stats)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

ticker <- "SPY"

getSymbols(ticker, from = paste0('2009-03-09'), to = paste0('2018-04-30'), 
           src="yahoo", periodicity = "daily")
df <- data.frame(date = index(get(ticker)), 
                 get(ticker), row.names=NULL) %>%
  select(date, contains("Adjust"))

colnames(df) <- c("date", "value")

drawdown_path <- function(vp){
  dd      <- data.frame(date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
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

to_plot <- drawdown_path(df)

file_path <- paste0(exportdir, "74-buy-and-hold-good-luck/drawdowns-since-2009.jpeg")

# Add a source and note string for the plots
source_string <- str_wrap(paste0("Source:  Yahoo Finance (OfDollarsAndData.com)"),
                          width = 85)

note_string <- str_wrap(paste0("Note: Does not adjust for inflation."),
                        width = 85)

top_title <- paste0("The Worst Drawdown in the S&P 500\nSince 2009 Has Been ", round(min(to_plot$pct), 2) * 100, "%")

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = pct)) +
  geom_area(fill = "red") +
  ggtitle(top_title) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(label = percent, limits = c(-0.2, 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Year", y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #