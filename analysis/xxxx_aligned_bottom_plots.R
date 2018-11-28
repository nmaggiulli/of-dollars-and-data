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
library(lubridate)
library(ggjoy)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  filter(Date > "1928-01-01")

# Calculate returns for the S&P data


# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, Date, price_plus_div) %>%
  mutate(Date = as.Date(paste0(
    substring(as.character(Date), 1, 4),
    "-", 
    ifelse(substring(as.character(Date), 6, 7) == "1", "10", substring(as.character(Date), 6, 7)),
    "-01", 
    "%Y-%m-%d")))


# Create a date list for the bottoms
date_list <- c("1932-06-01", "1974-12-01", "1987-12-01", "2003-02-01", "2009-03-01")

# Create function to plot aligned drawdowns
plot_aligned_drawdowns <- function(n_years_after_bottom){
  
  # Create a counter for the subsets 
  counter            <- 1

  # Loop through each date to make a subset
  for (date in date_list){
    sp500_filtered <- filter(sp500_ret_pe, Date >= as.Date(date), Date < as.Date(date) %m+% months(n_years_after_bottom * 12))
    for (i in 1:nrow(sp500_filtered)){
      sp500_filtered[i, "year"] <- i/12
      if (i == 1){
        sp500_filtered[i, "index"] <- 100
      } else {
        sp500_filtered[i, "index"] <- sp500_filtered[(i-1), "index"] * (1 + (sp500_filtered[(i), "price_plus_div"]/sp500_filtered[(i-1), "price_plus_div"] - 1))
      }
    }
    
    # Drop unneeded columns
    sp500_filtered <- sp500_filtered %>% 
      mutate(bottom = year(date)) %>%
      select(year, bottom, index)
    
    # Append the rows as we loop through each subset
    if (counter == 1){
      to_plot <- sp500_filtered
    } else{
      to_plot <- bind_rows(to_plot, sp500_filtered)
    }
    counter <- counter + 1
  }
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "0047_aligned_bottom_plots/sp500-aligned-bottoms-", n_years_after_bottom, ".jpeg")
  
  # Create title based on the number of years
  top_title <- paste0("Larger Declines Usually Lead to\nStronger Recoveries") 

  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = year, y = index, col = as.factor(bottom))) +
    geom_line() +
    geom_hline(yintercept = 100, linetype =  "dashed", col = "black") +
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year) & bottom != 2009 & bottom != 1932), 
                    aes(x = year, 
                        y = index, 
                        col = as.factor(bottom), 
                        label = as.character(bottom),
                        family = "my_font"
                    ), force = 2) +
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year) & bottom == 2009), 
                    aes(x = year, 
                        y = index, 
                        col = as.factor(bottom), 
                        label = as.character(bottom),
                        family = "my_font"
                    ), force = 2,
                    nudge_y = 15) +
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year) & bottom == 1932), 
                    aes(x = year, 
                        y = index, 
                        col = as.factor(bottom), 
                        label = as.character(bottom),
                        family = "my_font"
                    ), force = 2,
                    nudge_y = -12) +
    ggtitle(top_title) +
    scale_color_manual(values =  my_palette, guide = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous() +
    scale_x_continuous(limits = c(0, n_years_after_bottom), breaks = seq(0, n_years_after_bottom, 1)) +
    labs(x = "Years Since Bottom", y = "Real Index (Bottom = 100)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
  note_string <- paste0("Note:  Real return includes reinvested dividends.") 
  
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

plot_aligned_drawdowns(5)

# ############################  End  ################################## #