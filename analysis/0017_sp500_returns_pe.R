cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #


library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(dplyr)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, !is.na(cape), date < 2017.01)

first_year <- floor(min(sp500_ret_pe$date))
last_year <- floor(max(sp500_ret_pe$date))

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

# Setup a vector of different returns to calculate
# This will be for 5 year, 10 year, 20 year, and 30 year returns
returns_to_calc <- seq(5, 30, by = 1)

# Calculate returns over different time frames
for (i in 1:nrow(sp500_ret_pe)){
  for (x in returns_to_calc){
    if (i + (x * 12) > nrow(sp500_ret_pe)){
      break
    } else{
      name <- paste0("return_", x, "yr")
      sp500_ret_pe[i, name] <- (sp500_ret_pe[(i + (x * 12)), "price_plus_div"]/sp500_ret_pe[(i), "price_plus_div"])^(1/(x)) - 1
    }
  }
}

sp500_ret_pe_long <- sp500_ret_pe %>%
                      select(date, cape, contains("return_")) %>%
                      gather(key, value, -date, -cape) %>%
                      filter(!is.na(value)) %>%
                      mutate(below_zero = ifelse(value < 0, 1, 0))

plot_ret_pe <- function(var){
  yvar        <- paste0("return_", var, "yr")
  filter_line <- paste0("key == '", yvar, "'")
  to_plot     <- filter_(sp500_ret_pe_long, filter_line)
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "17-sp500-returns-pe/returns-", var,"-year.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = cape, y = value, col = as.factor(below_zero))) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, col = "black") +
    scale_color_manual(values = my_palette, guide = FALSE) +
    scale_y_continuous(label = percent, limits = c(-0.15, 0.35)) +
    scale_x_continuous(limits = c(0, 45)) +
    ggtitle(paste0("U.S. Stocks Annualized Real Return\n(Over ", var, " Years) vs. P/E Ratio")) +
    of_dollars_and_data_theme +
    labs(x = "U.S. Stocks P/E Ratio" , y = "Annualized Future Real Return (%)")

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
  
  if (var == 5 | var == 30) {
    if (var == 5){
      title <- paste0("As Stocks Get More Expensive\nTheir Future Returns Generally Decrease")
    } else {
      title <- paste0("Over Longer Time Periods, CAPE is\nLess Meaningful for Future Returns")
    }
    
    # Set the file_path for the next output
    file_path = paste0(exportdir, "17-sp500-returns-pe/fit-returns-", var,"-year.jpeg")
    
    plot <- ggplot(data = to_plot, aes(x = cape, y = value)) +
      geom_point() +
      geom_smooth(method = "lm") +
      geom_hline(yintercept = 0, col = "black") +
      scale_color_manual(guide = FALSE) +
      scale_y_continuous(label = percent, limits = c(-0.15, 0.35)) +
      scale_x_continuous(limits = c(0, 45)) +
      ggtitle(title) +
      of_dollars_and_data_theme +
      labs(x = "U.S. Stocks P/E Ratio" , y = paste0("Annualized Future Real Return\nFor Next ", var, " Years"))
    
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
  
}

for (x in returns_to_calc){
  plot_ret_pe(x)
}

plot_ret_pe(5)
plot_ret_pe(30)

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 25 loop -0 returns-*.jpeg all_plots.gif


# ############################  End  ################################## #