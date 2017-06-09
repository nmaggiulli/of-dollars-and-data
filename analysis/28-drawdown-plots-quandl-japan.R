cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(lubridate)

########################## Start Program Here ######################### #

# Read in data for the Japanese stock market
jpy_quandl <- readRDS(paste0(localdir, "28-quandl-japan.Rds")) %>%
              mutate(close_price = `Close Price`,
                     date = Date,
                     year = year(date),
                     month = month(date),
                     day = day(date)) %>%
              select(date, close_price, year, month, day) %>%
              group_by(year, month) %>%
              arrange(year, month, day) %>%
              filter(row_number() == 1) %>%
              ungroup() %>%
              select(date, close_price)

# Set some parameters for the DCA
monthly_savings <- 100

years_vector <- seq(1960, 1995, 5)

for (i in 1:length(years_vector)){
  prices <- filter(jpy_quandl, year(date) >= years_vector[i])
  
  for (j in 1:nrow(prices)){
    if (j == 1){
      prices[j, "value_dca"]     <- monthly_savings
      prices[j, "value_lumpsum"] <- monthly_savings*(nrow(prices))
      prices[j, "cost_basis"]    <- monthly_savings
    } else {
      prices[j, "value_dca"]     <- prices[(j-1), "value_dca"] * (prices[j, "close_price"] / prices[(j-1), "close_price"]) + monthly_savings
      prices[j, "value_lumpsum"] <- prices[(j-1), "value_lumpsum"] * (prices[j, "close_price"] / prices[(j-1), "close_price"])
      prices[j, "cost_basis"]    <- prices[(j-1), "cost_basis"] + monthly_savings
    }
  }
  
  # Reshape the data
  to_plot <- prices %>%
              select(-close_price) %>%
              gather(key, value, -date)
  
  # Set file path
  file_path = paste0(exportdir, "28-quandl-japan/plot-", years_vector[i] ,".jpeg")
  
 plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
    geom_line() +
    scale_color_discrete(guide = FALSE) +
   of_dollars_and_data_theme +
   labs(x = "Date", y = "Value") +
   ggtitle("Japanese Stock Market\n", years_vector[i])
 
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source: Simulated returns (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Does not adjust for inflation.")
  
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
}




# ############################  End  ################################## #

  
