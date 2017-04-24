cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, cape != "NA", Date < 2011.02)

first_year <- floor(min(sp500_ret_pe$Date))
last_year <- floor(max(sp500_ret_pe$Date))

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

plot_returns <- function(n_years, ymin, ymax, yby){

  # Create a date sequence for each decade
  date_seq <- seq(1881.01, (2011.01-n_years), 10)
  
  # Loop through the dates to get each return period for a specified number of years and stack it
  for (i in 1:length(date_seq)){
    ret_yr            <- filter(sp500_ret_pe, Date >= date_seq[i], Date < date_seq[i] + n_years)
    initial_value     <- filter(sp500_ret_pe, Date == date_seq[i]) %>%
                          select(price_plus_div)
    ret_yr$price      <- (ret_yr$price_plus_div / rep(as.numeric(initial_value), 12*n_years)) * 100
    ret_yr$start_date <- date_seq[i]
    ret_yr$period     <- seq(1/12, n_years, 1/12)
    ret_yr            <- select(ret_yr, period, start_date, price)
    if (i == 1){
      to_plot <- ret_yr
    } else{
      to_plot <- rbind(to_plot, ret_yr)
    }
  }
    
  # Set the file_path for the next output
  file_path = paste0(exportdir, "21-the-end-is-everything/", n_years ,"yr-returns-by-decade.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = period, y = price, col = as.factor(start_date))) +
    geom_line(alpha = 0.5) +
    scale_x_continuous(limits = c(1/12, n_years), breaks = seq(5, n_years, 5)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = seq(ymin, ymax, yby)) +
    scale_color_discrete(guide = FALSE) +
    geom_text_repel(data = filter(to_plot, period == max(to_plot$period)),
                    aes(x = period, 
                        y = price,
                        col = as.factor(start_date),
                        label = start_date,
                        family = "my_font")
                    ) +
    ggtitle(paste0("What Decade You Start Matters\n(", n_years, " Year Returns)")) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Index (Year 1 = 100)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Annualized real returns include reinvested dividends.") 
  
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

n_years_vector <- c(20, 30, 40)

for (n in n_years_vector){
  if (n == 20){
    y_min <- 0
    y_max <- 1100
    y_by  <- 100
  } else if (n == 30){
    y_min <- 0
    y_max <- 1500
    y_by  <- 250
  } else if (n == 40){
    y_min <- 0
    y_max <- 4500
    y_by  <- 500
  }
  plot_returns(n, y_min, y_max, y_by)
}


# ############################  End  ################################## #