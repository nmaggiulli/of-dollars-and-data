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
library(dplyr)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                      filter(date > "1928-01-01")

# Find the drawdowns for investigative purposes
# Use this list to find the most recent peak from each bottom in the data
dd        <- drawdown_path(sp500_ret_pe)

# Add markers onto the drawdown path for different drawdown levels
drawdown_dummy <- function(pct, name){
  peak <- 1
  for (i in 1:nrow(dd)){
    if (dd[i, "pct"] == 0){
      peak <- 1
      dd[i, name] <- 0
    } else if (dd[i, "pct"] < pct & peak == 1){
      dd[i, name] <- 1
      peak <- 0
    } else {
      dd[i, name] <- 0
    }
  }
  assign(name, sum(dd[, name]), envir = .GlobalEnv)
  assign("dd", dd, envir = .GlobalEnv)
}

drawdown_dummy(-0.3, "drawdown_30pct")
drawdown_dummy(-0.4, "drawdown_40pct")
drawdown_dummy(-0.5, "drawdown_50pct")

# Create drawdown plots
# Set the file_path based on the function input 
file_path = paste0(exportdir, "0035_aligned_drawdown_plots/drawdowns-with-markers.jpeg")

# Create title with ticker in subtitle
top_title <- paste0("The S&P 500 Has Had ", drawdown_30pct, " Drawdowns of \nOver 30% Since the Late 1920s")

# Create the plot object
plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(fill = "red") +
  geom_point(data = filter(dd, drawdown_30pct == 1), aes(x = date, y = pct), col = "black") +
  ggtitle(top_title) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(label = percent, limits = c(-1, 0)) +
  scale_x_date(date_labels = "%Y") +
  labs(x = "Year", y = "Percentage of Inflation-Adjusted Value Lost")

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

# Create function to plot aligned drawdowns
plot_aligned_drawdowns <- function(n_years_after_peak, ymax){
  
  ## Create an aligned drawdown plot for specific crashes throughout market history
  # Select the date list manually
  if(n_years_after_peak > 8){
    date_list <- c("1929-09-01", "1973-03-01", "1987-08-01", "2000-08-01")
  } else {
    date_list <- c("1929-09-01", "1973-03-01", "1987-08-01", "2000-08-01", "2007-10-01")
  }
  
  # Create a counter for the subsets 
  counter            <- 1
  
  # Loop through each date to make a subset
  for (d in date_list){
    sp500_filtered <- filter(sp500_ret_pe, date >= as.Date(d), date < as.Date(d) %m+% months(n_years_after_peak * 12))
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
                        mutate(peak = year(d)) %>%
                        select(year, peak, index)
    
    # Append the rows as we loop through each subset
    if (counter == 1){
      to_plot <- sp500_filtered
    } else{
      to_plot <- bind_rows(to_plot, sp500_filtered)
    }
    counter <- counter + 1
  }
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "0035_aligned_drawdown_plots/sp500-aligned-drawdowns-", n_years_after_peak, ".jpeg")
  
  # Create title based on the number of years
  if (n_years_after_peak == 5){
    top_title <- paste0("The Most Famous Crashes of the S&P 500\n5 Years After Each Peak") 
  } else if (n_years_after_peak == 10){
    top_title <- paste0("10 Years After the Peak, The Dotcom Bubble\nWas Comparable to the Great Depression")
  }
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = year, y = index, col = as.factor(peak))) +
    geom_line() +
    geom_hline(yintercept = 100, linetype =  "dashed", col = "black") +
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year)), 
                    aes(x = year, 
                        y = index, 
                        col = as.factor(peak), 
                        label = as.character(peak),
                        family = "my_font"
                    ), force = 2) +
    ggtitle(top_title) +
    scale_color_manual(values =  my_palette, guide = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, 25)) +
    scale_x_continuous(limits = c(0, n_years_after_peak), breaks = seq(0, n_years_after_peak, 1)) +
    labs(x = "Years Since Peak", y = "Real Index (Peak = 100)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
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

plot_aligned_drawdowns(5, 125)
plot_aligned_drawdowns(10, 275)

# ############################  End  ################################## #

  
