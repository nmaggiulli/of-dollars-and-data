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
library(ggjoy)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Set program paramaters
n_years_working     <- 40
income              <- 50000
annual_return       <- 0.05

create_value_path <- function(savings_rate, annual_return, name){
  
  results_df <- data.frame(matrix(NA, nrow = n_years_working, ncol = 0))
  
  for (i in 1:n_years_working){
    results_df[i, "rate_label"]  <- paste0(savings_rate*100, "%")
    results_df[i, "rate"]    <- savings_rate
    results_df[i , "year"]    <- i
    results_df[i, "return"]  <- annual_return
    if (i==1){
      results_df[i, "value"] <- income * savings_rate
    } else {
      results_df[i, "value"] <- (results_df[(i -1), "value"] * (1+annual_return)) + income*savings_rate 
    }
  }
  
  assign(name, results_df, envir = .GlobalEnv)
  print(results_df[n_years_working, "value"])
}

create_value_path(0.05, annual_return, "sr_5_5")
create_value_path(0.1, annual_return, "sr_10_5")
create_value_path(0.15, annual_return, "sr_15_5")


results_df <- bind_rows(sr_5_5, sr_10_5, sr_15_5)
results_df$rate_label <- factor(results_df$rate_label, levels = unique(results_df$rate_label[order(results_df$rate)]))

years_to_plot <- c(seq(2, n_years_working, 2), n_years_working, n_years_working)

for (i in 1:length(years_to_plot)){
  if (i < 10){
    i_string <- paste0("0", i)
  } else{
    i_string <- i
  }
  
  to_plot <- filter(results_df, year == years_to_plot[i])
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "45-the-constant-reminder/saving-plot-", i_string, ".jpeg")
  
  plot <- ggplot(data = to_plot, aes(x = rate_label, y = value, fill = rate_label)) +
    geom_bar(stat="identity") +
    scale_fill_discrete(guide = FALSE) +
    ggtitle(paste0("Total Wealth by Savings Rate\nAfter ", years_to_plot[i], " Years")) +
    scale_y_continuous(label = dollar, limits = c(0, 1000000)) +
    of_dollars_and_data_theme +
    labs(x = "Savings Rate" , y = "Total Wealth")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source: Simulated data (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes an annual income of $", formatC(as.numeric(income), format="f", digits=0, big.mark=","), " with a ", annual_return*100,"% annual return.") 
  
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

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# convert -delay 30 loop -0 saving-plot-*.jpeg all_plots.gif


# ############################  End  ################################## #