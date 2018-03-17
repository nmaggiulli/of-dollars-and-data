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
library(tidyr)
library(Quandl)
library(quantmod)
library(dplyr)

########################## Start Program Here ######################### #

# Bring in JPY Nikkei index data
jpy   <- Quandl('NIKKEI/INDEX')

to_plot    <-  filter(jpy, Date >= '1980-01-01', Date <= '1989-12-31') %>%
            select(Date, `Close Price`) %>%
            mutate(date = Date,
            price = `Close Price`) %>%
            select(price, date) 

file_path <- paste0(exportdir, "64-lies-and-stories/jpy-hist-price.jpeg")

source_string <- "Source:  Quandl (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Does not adjust for USD currency changes or dividends.")

plot <- ggplot(to_plot, aes(x=date, y=price)) +
          geom_line() +
          scale_y_continuous(label = comma) +
          ggtitle(paste0("In the late 1980s,\nJapan Was Taking Over the World"))  +
          of_dollars_and_data_theme +
          labs(x = "Year" , y = "Price (¥)",
               caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 

# Get NASDAQ data
getSymbols("^IXIC", src="yahoo", from = paste0('1990-01-01'), to = paste0('1999-12-31')) 

file_path <- paste0(exportdir, "64-lies-and-stories/nasdaq-hist-price.jpeg")

source_string <- "Source:  Quandl (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Does not adjust for inflation.")

to_plot <- data.frame(date = index(get("IXIC")), 
                 get("IXIC"), row.names=NULL) %>%
  select(date, contains("Adjust"))

names(to_plot) <- c("date", "price")
           
plot <- ggplot(to_plot, aes(x=date, y=price)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  ggtitle(paste0("Technology Stocks (NASDAQ) in the late 1990s"))  +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Index",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 

##Plot the US Housing data (borrowed from analysis 15)
# Set start year to be 2000
start_year <- as.Date("2000-01-01")
end_year   <- as.Date("2007-06-30")

# Write function to read in a particular tier
read_in_tier <- function(name){
  string <- deparse(substitute(name))
  temp <- readRDS(paste0(localdir, "15_metro_zhvi_", string,"tier.Rds"))
  temp <- filter(temp, year >= start_year, year <= end_year, RegionName == "United States")
  temp$type <- toTitleCase(string)
  assign(paste0("metro_zhvi_", string), temp, envir = .GlobalEnv)
}

read_in_tier(middle)

to_plot <- arrange(metro_zhvi_middle, RegionID, type, year)

# Get the years list
first_year  <- min(to_plot$year)
last_year   <- max(to_plot$year)

# Get start starting and ending home values of the middle tier
middle_start <- filter(to_plot, type == "Middle", year == first_year) %>%
  select(price)
middle_end   <- filter(to_plot, type == "Middle", year == last_year) %>%
  select(price)

# Set the file_path based on the function input 
file_path <- paste0(exportdir, "64-lies-and-stories/zhvi-us-housing.jpeg")

# Add a source and note string for the plots
source_string <- paste0("Source:  Zillow Group, ", year(start_year),"-", year(end_year), " (OfDollarsAndData.com)")

note_string <- paste0("Note:  Does not adjust for inflation.")

# Create the plot
plot <- ggplot(to_plot, aes(x = year, y = price)) +
  geom_line() +
  of_dollars_and_data_theme +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggtitle(paste0("U.S. Housing in the Mid-2000s")) +
  labs(x = "Year", y = "Median Home Value",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #