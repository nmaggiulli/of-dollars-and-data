cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

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
}

  return_for_filter <- c(0.04, 0.06, 0.08)

  # Filter data to specific delay period and add a text flag variable
  to_plot <- filter(results_df, return %in% return_for_filter) %>%
              mutate(text_flag = ifelse(increase_pct > 3.57 & increase_pct < 3.98, 1 ,0))
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "0019_compounding_is_easy/return_pcts.jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = years_delayed, y = increase_pct, col = as.factor(return))) +
    geom_line() +
    scale_color_manual(guide = FALSE, values = my_palette) +
    geom_text_repel(data = filter(to_plot, text_flag == 1),
                    aes(x = years_delayed, 
                        y = increase_pct,
                        col = as.factor(return),
                        label = paste0(as.factor(round(100*return)), "%"),
                        family = "my_font"),
                    nudge_y = 0.01,
                    max.iter = 3000) +
    scale_y_continuous(label = percent, limits = c(0, 4), breaks = seq(0, 4, 1)) +
    scale_x_continuous(limits = c(1, max(delay_vector))) +
    ggtitle(paste0("As Your Expected Return Increases,\nDelaying Investment Gets More Costly")) +
    of_dollars_and_data_theme +
    labs(x = "Years Delayed" , y = "Additional Annual Savings (%)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source: Simulated returns (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes ", n_years_saving, " years of annual savings with the annual return listed above.") 
  
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