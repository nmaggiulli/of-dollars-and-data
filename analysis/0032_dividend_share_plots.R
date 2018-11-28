cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(ggrepel)
library(stringr)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#E41A1C", "#4DAF4A", "#000000", "#377EB8", "#984EA3", "#FF7F00", "#A65628")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, Date < 2017.01)

first_year <- floor(min(sp500_ret_pe$Date))
last_year <- floor(max(sp500_ret_pe$Date))



# Loop over different time horizons
n_years_vec <- seq(20, 50, 10)

# For loop for creating the many plots by n_years
for (n in n_years_vec){
  
  # Number of years to calculate returns over
  n_years  <- n
  n_months <- n_years*12
  
  # Create a date sequence for each decade
  date_seq <- seq(min(sp500_ret_pe$Date), max(sp500_ret_pe$Date) - n_years, 1)
    
  # Loop through the dates to get each return period for a specified number of years and stack it
  for (i in 1:length(date_seq)){
    ret_yr              <- filter(sp500_ret_pe, Date >= date_seq[i], Date <= date_seq[i] + n_years) %>%
      mutate(total_diff = ((price_plus_div / lag(price_plus_div, n = n_months)) - 1),
             price_diff = ((real_price / lag(real_price, n = n_months)) - 1),
             div_share =  ifelse(price_diff < 0, 1, 1 - (price_diff/total_diff))) %>%
      filter(Date == date_seq[i] + n_years) %>%
      select(Date, div_share, total_diff)
    if (i == 1){
      to_plot <- ret_yr
    } else{
      to_plot <- rbind(to_plot, ret_yr)
    }
  }
  
  # Print for error checking (i.e. when total real return is negative)
  # This never happens for any period >= 20 years
  if(min(to_plot$total_diff) < 0){
    print(paste0("Below 0 issue: ", n_years, " years"))
  }
  
  if (n_years == 20){
    top_title <- paste0("The Dividend Share of Total Return\nHas Fallen in Recent Years")
  } else {
    top_title <- paste0("Almost All Long-Term Investment Gains\nCome From Dividends")
  }
  
  # Set the file path
  file_path = paste0(exportdir, "0032_dividend_share_plots/dividend-share-over-time-", n_years, ".jpeg")
  
  # Create plot 
  plot <- ggplot(data = to_plot, aes(x = Date, y = div_share, fill = as.factor(1))) +
    geom_area() +
    scale_color_manual(values = my_palette, guide = FALSE) +
    scale_fill_manual(values = my_palette, guide = FALSE) +
    scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    
    of_dollars_and_data_theme +
    labs(x = paste0(n_years, "-Year Period Ending:") , y = "Dividend Share of Total Real Return") +
    ggtitle(top_title)
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Real returns include monthly reinvested dividends.  Dividend share is capped at 100%.") 
  
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

# ############################  End  ################################## #