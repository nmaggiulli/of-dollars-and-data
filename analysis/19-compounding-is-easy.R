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
library(magick)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Initialize savings, returns, and delay values
payment        <- 1000
n_years_saving <- 40
delay_vector   <- seq(1, 25, 1)
returns_vector <- seq(0.03, 0.08, 0.01)

# Initialize dataframe for results
results_df    <- data.frame(matrix(NA, nrow = (length(delay_vector) * length(returns_vector)), ncol = 0))

# Loop over the delay and return vectors
i <- 0
for (d in delay_vector){
  for (r in returns_vector){
    i <- i + 1
    future_value                   <- payment * (((1 + r)^n_years_saving - 1) / r)
    delay_payment                  <- (future_value * r)/((1 + r)^(n_years_saving - d) - 1)
    total_capital                  <- payment * n_years_saving
    total_capital_delayed          <- delay_payment * (n_years_saving - d)
    
    results_df[i, "increase_pct"]            <- delay_payment/payment - 1
    results_df[i, "return"]                  <- r
    results_df[i, "years_delayed"]           <- d
    results_df[i, "extra_initial_payment"]   <- (total_capital_delayed - total_capital)/ payment
  }
  
  # Filter data to specific delay period
  to_plot <- filter(results_df, years_delayed == d)
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "19-compounding-is-easy/delay-", d,"-years.jpeg")
  
  # Create year string for title
  if (d == 1){
    year_string <- "Year"
  } else {
    year_string <- "Years"
  }
  
  plot <- ggplot(data = to_plot, aes(x = return, y = increase_pct)) +
    geom_line() +
    scale_y_continuous(label = percent, limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_x_continuous(label = percent, limits = c(min(returns_vector), max(returns_vector))) +
    ggtitle(paste0("Additional Annual Savings Required\nWhen Delaying (For ", d, " ", year_string, ")")) +
    of_dollars_and_data_theme +
    labs(x = "Expected Future Return" , y = "Additional Annual Savings (%)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source: Simulated returns (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes ", n_years_saving, " years of annual savings and a constant rate of return.") 
  
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
  
  # Set the file_path for the next output
  file_path2 = paste0(exportdir, "19-compounding-is-easy/delay-", d,"-years-free-y.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = return, y = increase_pct)) +
    geom_line() +
    scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent, limits = c(min(returns_vector), max(returns_vector))) +
    ggtitle(paste0("Additional Annual Savings Required\nWhen Delaying (For ", d, " ", year_string, ")")) +
    of_dollars_and_data_theme +
    labs(x = "Expected Future Return" , y = "Additional Annual Savings (%)")
  
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
  ggsave(file_path2, my_gtable, width = 15, height = 12, units = "cm")
}

# Read in the the individual images
frames <- lapply(delay_vector, function(x){
  image_read(paste0(exportdir, "19-compounding-is-easy/delay-", x,"-years.jpeg"))
})

# Make animation from the frames read in during the prior step
image_write(image_animate(image_join(frames), fps = 4), 
            paste0(exportdir, "19-compounding-is-easy/all-delays.gif"))


# ############################  End  ################################## #