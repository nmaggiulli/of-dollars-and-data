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
library(dplyr)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                      filter(date > "1928-01-01")

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
}

# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, price_plus_div) %>%
                  mutate(date = as.Date(paste0(
                    substring(as.character(date), 1, 4),
                    "-", 
                    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
                    "-01", 
                    "%Y-%m-%d")))

# Create function to plot aligned drawdowns
  ## Create an aligned drawdown plot for specific crashes throughout market history
  # Select the date list manually

date_list <- c("1929-09-01", "1973-03-01", "1987-08-01", "2000-08-01", "2007-10-01")

  # Create a counter for the subsets 
  counter            <- 1
  
  # Set the number of years after the peak
  n_years_after_peak <- 9
  
  # Loop through each date to make a subset
  for (date1 in date_list){
    sp500_filtered <- filter(sp500_ret_pe, date >= as.Date(date1), date < as.Date(date1) %m+% months(n_years_after_peak * 12))
    for (i in 1:nrow(sp500_filtered)){
      if (i == 1){
        sp500_filtered[i, "year"] <- 0
        sp500_filtered[i, "index"] <- 100
        sp500_filtered[i, "cagr"]  <- 0
      } else {
        sp500_filtered[i, "year"] <- i/12
        sp500_filtered[i, "index"] <- sp500_filtered[(i-1), "index"] * (1 + (sp500_filtered[(i), "price_plus_div"]/sp500_filtered[(i-1), "price_plus_div"] - 1))
        sp500_filtered[i, "cagr"]  <- (sp500_filtered[i, "index"]/100)^(1/sp500_filtered[i, "year"]) - 1
      }
    }
    
    # Drop unneeded columns
    sp500_filtered <- sp500_filtered %>% 
                        mutate(peak = year(date1)) %>%
                        select(year, peak, index, cagr)
    
    # Append the rows as we loop through each subset
    if (counter == 1){
      sp500_filtered_all <- sp500_filtered
    } else{
      sp500_filtered_all <- bind_rows(sp500_filtered_all, sp500_filtered)
    }
    counter <- counter + 1
  }
  
# Create a years sequence over time
year_seq <- c(seq(0, n_years_after_peak, 0.5))

# Reset a counter for file naming
counter <- 10
for (year_filtered in year_seq){
  counter <- counter + 1
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "xx-cagr-peaks-troughs/sp500-aligned-drawdowns-",counter,".jpeg")
  
  to_plot <- filter(sp500_filtered_all, year <= year_filtered)
  
  top_title <- paste0("Compound Annual Growth Rate\n", year_filtered, " Years After Select Market Peaks")
 
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = year, y = cagr, col = as.factor(peak))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype =  "dashed", col = "black") +
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year)), 
                    aes(x = year, 
                        y = cagr, 
                        col = as.factor(peak), 
                        label = as.character(peak),
                        family = "my_font"
                    ), force = 2) +
    ggtitle(top_title) +
    scale_color_manual(values =  my_palette, guide = FALSE) +
    scale_y_continuous(label = percent, limits = c(-0.85, 0.1)) +
    of_dollars_and_data_theme +
    scale_x_continuous(limits = c(0, n_years_after_peak), breaks = seq(0, n_years_after_peak, 1)) +
    labs(x = "Years Since Peak", y = "CAGR Since Peak")
  
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
  if (year_filtered != n_years_after_peak){
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  } else {
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    for (j in 1:3){
    counter <- counter + 1
    file_path <- paste0(exportdir, "xx-cagr-peaks-troughs/sp500-aligned-drawdowns-",counter,".jpeg")
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    }
  }
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 40 loop -0 *.jpeg all_plots.gif


# ############################  End  ################################## #

  
