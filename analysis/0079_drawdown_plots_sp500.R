cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

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
  
# Create function to calculate the drawdowns over time
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
  

plot_dd <- function(date_start, date_end){
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"
  
  temp <- filter(sp500_ret_pe, date >= date_start, date <= date_end)
  # Run function on specific data for drawdowns
  to_plot        <- drawdown_path(temp)
    
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "0079_drawdown_plots_sp500/drawdowns_", date_start, "_", date_end, ".jpeg")
  
  # Create title with ticker in subtitle
  top_title <- paste0("Investors in the 1940s Would Have\nRemembered These Drawdowns")
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = date, y = pct)) +
    geom_area(fill = "red") +
    ggtitle(top_title) +
    guides(fill = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(label = percent, limits = c(-1, 0)) +
    scale_x_date(date_labels = "%Y") +
    labs(x = "Year", y = "Percentage of Value Lost")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  note_string <- paste0("Note:  Adjusted for dividends and inflation.") 
  
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

plot_dd("1871-01-01", "1940-01-01")

# ############################  End  ################################## #

  
