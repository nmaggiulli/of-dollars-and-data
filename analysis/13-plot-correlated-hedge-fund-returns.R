cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(ggrepel)
library(stringr)

########################## Start Program Here ######################### #

# Load data fom local library
hf_results <- readRDS(paste0(localdir, "13-hf-correlation-results.Rds"))

# Loop through rows to add additional info to the data frame
for (i in 1:nrow(hf_results)){
  if (hf_results[i, "scenario"] == 1){
    hf_results[i, "scenario_name"] <- "1% management or 30% of alpha"
  } else if (hf_results[i, "scenario"] == 2){
    hf_results[i, "scenario_name"] <- "2% management and 20% performance"
  } else if (hf_results[i, "scenario"] == 3){
    hf_results[i, "scenario_name"] <- "1% management and 17% performance"
  }
}

unique_outperformance <- unique(hf_results$hf_outperformance)
for (j in unique_outperformance){
  to_plot <- filter(hf_results, hf_outperformance == j)
  
  file_path = paste0(exportdir, "13-simulate-correlated-hedge-fund-returns/hf-outperform-", j ,".jpeg")
  
  plot <- ggplot(to_plot, aes(x = hf_corr_to_market, y = hf_outperform_pct, col = scenario_name)) +
            geom_line() +
            geom_text_repel(data = filter(to_plot, hf_corr_to_market == 0.5), 
                    aes(hf_corr_to_market, 
                        col = scenario_name,
                        label = str_wrap(scenario_name, width = 18),
                        family = "my_font")) +
          scale_color_brewer(palette = "Set1", guide = FALSE) +
          ggtitle(paste0("Hedge Fund Performance Relative to Market\n(Assumes ", 100*j, "% Annual Outperformance)")) +
          of_dollars_and_data_theme +
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), label = percent) +
          scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
          labs(x = "Hedge Fund Correlation to Market" , y = "Percentage of Simulations Where Fund\nOutperforms The Market (Net of Fees)")
  
  # Add a source and note string for the plots
  source_string <- "Source:  Simulated returns (OfDollarsAndData.com)"
  note_string   <- "Note:  Assumes the market fund has an annual expense ratio of 5 basis points." 
  
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

# ############################  End  ################################## #
