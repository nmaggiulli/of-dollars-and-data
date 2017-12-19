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
library(dplyr)

########################## Start Program Here ######################### #

# Bring in JPY Nikkei index data
jpy   <- Quandl('NIKKEI/INDEX')
to_plot    <-  filter(jpy, Date > '1980-01-01') %>%
            select(Date, `Close Price`) %>%
            mutate(date = Date,
            price = `Close Price`) %>%
            select(price, date) 

file_path <- paste0(exportdir, "51-when-do-you-sell/jpy-hist-price.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=price)) +
          geom_line() +
          scale_y_continuous(label = comma) +
          ggtitle(paste0("Japan's Melt Up and Drawdown"))  +
          of_dollars_and_data_theme +
          labs(x = "Date" , y = "Price (¥)")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Quandl (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Does not adjust for USD currency changes or dividends.")

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