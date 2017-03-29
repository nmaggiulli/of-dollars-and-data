cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

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

# Set program paramaters
n_years_working     <- 40
income              <- 50000
savings_rate        <- 0.15
sample_mean         <- 0.05
sample_sd           <- 0.09
n_simulations       <- 1

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#E41A1C", "#4DAF4A", "#000000", "#377EB8", "#984EA3", "#FF7F00", "#A65628")


# This seed allows us to have reproducible random sampling
set.seed(12345)         

# Create asset and return matrices 
returns_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)
asset_matrix   <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)
savings_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)

# Create a for loop for each year you work
for (i in 1:(n_years_working + 1)){
  returns <- rnorm(n_simulations, sample_mean, sample_sd)
  if (i == 1){
    savings_matrix[, i]  <- income * savings_rate
    asset_matrix[, i]    <- savings_matrix[, i]
    returns_matrix[, i]  <- rep(0, n_simulations)
  } else {
    savings_matrix[, i]  <- income * savings_rate
    returns_matrix[, i]  <- returns * asset_matrix[, (i-1)]
    asset_matrix[, i]    <- savings_matrix[, i] + returns_matrix[, i] + asset_matrix[, (i-1)]
  }
}

# Convert the matrices to a long data frame for plotting
convert_to_df <- function(matrix, type){
  out <- as.data.frame(matrix) %>%
          gather(key = "year", value = "value", 1:(n_years_working+1))
  out$simulation <- seq(1, n_simulations)
  out$year       <- rep(seq(1, (n_years_working + 1)), each = n_simulations)
  out$type       <- type
  return(out)
}

savings_df <- convert_to_df(savings_matrix, "savings")
returns_df <- convert_to_df(returns_matrix, "returns")

# Bind the two data frames
to_plot <- bind_rows(savings_df, returns_df)

# Define the y_unit for the y-axis
y_unit <- 10^floor(min(log10(abs(max(to_plot$value))), log10(abs(min(to_plot$value)))))

# Function to find a rounded max/min based on the specifications of y_unit
create_max_min <- function(x, unit, ceilfloor) {
  ceilfloor(x/unit)*unit
}

y_max <- create_max_min(max(to_plot$value), y_unit, ceiling)
y_min <- create_max_min(min(to_plot$value), y_unit, floor)

# If the distance between the max and min is too large, increase y_unit
# until the distance is less than 10 ticks away
while (ceiling(abs(y_max - y_min))/y_unit > 10){
  y_unit <- y_unit * 2
}

# Define a new y_max if the y_unit has changed
y_max <- create_max_min(y_max, y_unit, ceiling)

## Create 1st plot
  # Set the file path
  file_path = paste0(exportdir, "16-investing-vs-saving/saving-vs-investing.jpeg")
  
  # Create plot 
  plot <- ggplot(data = to_plot, aes(x = year, fill = type, weight = value)) +
              geom_bar(position = "stack") +
              geom_hline(yintercept = 0) +
              geom_text_repel(data = 
                                filter(to_plot, 
                                       year == 1, 
                                       type == "savings"),
                              aes(x = year, 
                                  y = value,
                                  col = type,
                                  label = str_wrap("Savings Matter Early in Life", width = 10),
                                  family = "my_font"),
                              nudge_y = 120000,
                              nudge_x = 2) +
              geom_text_repel(data = 
                                filter(to_plot, 
                                       year == 29, 
                                       type == "returns"),
                              aes(x = year, 
                                  y = value,
                                  col = type,
                                  label = str_wrap("Investments Dominate Later in Life", width = 10),
                                  family = "my_font"),
                              nudge_y = 130000,
                              nudge_x = -2) +
              scale_color_manual(values = my_palette, guide = FALSE) +
              scale_fill_manual(values = my_palette, guide = FALSE) +
              scale_y_continuous(labels = dollar, limits = c(y_min, y_max), breaks = seq(y_min, y_max, y_unit)) +
              scale_x_continuous(breaks = seq(0, n_years_working, 5)) +
              of_dollars_and_data_theme +
              labs(x = "Years" , y = "Change in Value") +
              ggtitle(paste0("Savings and Investment Returns Have\nVarying Impact Over Time"))
  
  # Add a source and note string for the plots
  source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes an annual mean return of ", 
                          sample_mean*100,
                          "% with a ",
                          sample_sd*100,
                          "% standard deviation.") 
  
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
 
## Create additional plot to show percentage of total assets attributable to savings vs investment
assets_df  <- convert_to_df(asset_matrix, "total_assets")

assets_df$pct   <-  1 - ((seq(1, n_years_working + 1) * (income * savings_rate)) / assets_df$value)
assets_df$type  <- "investment_pct"

## Create 2nd plot
  
  # Get the maximum percentage for the data frame
  ymax <- max(assets_df$pct)
  
  # Set the file path
  file_path = paste0(exportdir, "16-investing-vs-saving/pct-of-total-assets.jpeg")
  
  # Create plot 
  plot <- ggplot(data = assets_df, aes(x = year, y = pct, fill = type)) +
    geom_area() +
    geom_hline(yintercept = ymax, col = my_palette[1], linetype = 2) +
    scale_color_manual(values = my_palette, guide = FALSE) +
    scale_fill_manual(values = my_palette, guide = FALSE) +
    scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(0, n_years_working, 5)) +
    of_dollars_and_data_theme +
    labs(x = "Years" , y = "Percentage of Total Assets") +
    ggtitle(paste0("Percentage of Total Assets That\nCome From Investment Gains"))
  
  # Add a source and note string for the plots
  source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes an annual mean return of ", 
                          sample_mean*100,
                          "% with a ",
                          sample_sd*100,
                          "% standard deviation.") 
  
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

# ############################  End  ################################## #