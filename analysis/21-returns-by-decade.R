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

# Load in Damodaran SP500 and Bond data
hist_bond_stock <- readRDS(paste0(localdir, "21-historical-returns-sp500-bond-damodaran.Rds"))

# Load in the FRED CPI data
cpi <- readRDS(paste0(localdir, "21-FRED-cpi.Rds"))

# Create a starting date for subsetting
starting_date <- 2011.01

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, Date < starting_date, Date > 1900)

# Subset historical bond and stock returns and adjust for CPI using FRED data
hist_bond_stock <- filter(hist_bond_stock, Date < starting_date) %>%
                      left_join(cpi, by = c("Date" = "year")) %>%
                      mutate(ret_sp500 = ret_sp500/100 - rate_cpi,
                             ret_10yr_bond = ret_10yr_bond/100 - rate_cpi) %>%
                      select(Date, ret_sp500, ret_10yr_bond)

first_year <- floor(min(sp500_ret_pe$Date))
last_year <- floor(max(sp500_ret_pe$Date))

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

plot_returns <- function(n_years, ymin, ymax, yby){

  # Create a date sequence for each decade
  date_seq <- seq(1901.01, (2011.01-n_years), 10)
  
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
  file_path = paste0(exportdir, "21-returns-by-decade/", n_years ,"yr-returns-by-decade.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = period, y = price, col = as.factor(start_date))) +
    geom_line(alpha = 0.5) +
    scale_x_continuous(limits = c(1/12, n_years), breaks = seq(5, n_years, 5)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = seq(ymin, ymax, yby)) +
    scale_color_discrete(guide = FALSE) +
    geom_text_repel(data = filter(to_plot, period == max(to_plot$period)),
                    aes(x = period, 
                        y = price,
                        col = as.factor(start_date),
                        label = round(start_date, digits=0),
                        family = "my_font")
                    ) +
    ggtitle(paste0("What Decade You Start In Matters\n(", n_years, " Year Returns)")) +
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

# Now do the same thing for the historical stock and bond data, but as a diversified portfolio
# Create a weighted return for the historical stock and bond data
plot_diversified <- function(n_years, wt_sp500){
  
  # Calculate a weighted return using the weights from the user input
  hist_bond_stock$ret_weighted <- hist_bond_stock$ret_sp500 * wt_sp500 +
                                     hist_bond_stock$ret_10yr_bond * (1 - wt_sp500)
  
  # Calculate annual index
  for (r in 1:nrow(hist_bond_stock)){
    if (r == 1){
      hist_bond_stock[r, "index"] <- 100
    } else {
      hist_bond_stock[r, "index"] <- hist_bond_stock[(r - 1), "index"] * (1 + hist_bond_stock[(r - 1), "ret_weighted"])
    }
  }
  
  # Create a date sequence for each decade
  date_seq <- seq(1931, (2011-n_years), 10)
  
  # Loop through the dates to get each return period for a specified number of years and stack it
  for (i in 1:length(date_seq)){
    ret_yr            <- filter(hist_bond_stock, Date >= date_seq[i], Date < date_seq[i] + n_years)
    initial_value     <- filter(hist_bond_stock, Date == date_seq[i]) %>%
      select(index)
    ret_yr$price      <- (ret_yr$index / rep(as.numeric(initial_value), n_years)) * 100
    ret_yr$start_date <- date_seq[i]
    ret_yr$period     <- seq(1, n_years, 1)
    ret_yr            <- select(ret_yr, period, start_date, price)
    if (i == 1){
      to_plot <- ret_yr
    } else{
      to_plot <- rbind(to_plot, ret_yr)
    }
  }
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "21-returns-by-decade/", n_years ,"yr-wstock-", as.character(wt_sp500), "-pct.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = period, y = price, col = as.factor(start_date))) +
    geom_line(alpha = 0.5) +
    scale_x_continuous(limits = c(1, n_years), breaks = seq(5, n_years, 5)) +
    scale_color_discrete(guide = FALSE) +
    geom_text_repel(data = filter(to_plot, period == max(to_plot$period)),
                    aes(x = period, 
                        y = price,
                        col = as.factor(start_date),
                        label = round(start_date, digits=0),
                        family = "my_font")
    ) +
    ggtitle(paste0(100*wt_sp500, "% Stock ", 100*(1-wt_sp500),"% Bond Portfolio\n(", n_years, " Year Returns)")) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Index (Year 1 = 100)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Adjusts for inflaton using FRED CPI data.") 
  
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
  plot_diversified(n, 1)
  plot_diversified(n, 0.5)
}


# ############################  End  ################################## #