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
library(ggrepel)
library(reldist)
library(Hmisc)
library(lazyeval)

########################## Start Program Here ######################### #

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

year_list <- unique(scf_stack$year)
edcl_list <- unique(scf_stack$edcl)
agecl_list <- unique(scf_stack$agecl)

to_plot <- as.data.frame(matrix(NA, nrow = length(year_list), ncol = 0))

i <- 1
  for (y in year_list){
      filtered <- filter(scf_stack, year == y)
      to_plot[i, "Net Worth"]  <- gini(filtered$networth, filtered$wgt)
      to_plot[i, "Total Income"]    <- gini(filtered$income, filtered$wgt)
      to_plot[i, "Wages"]   <- gini(filtered$wageinc, filtered$wgt)
      to_plot[i, "Interest and Dividends"] <- gini(filtered$intdivinc, filtered$wgt)
      to_plot[i, "year"] <- y
      i <- i + 1
  }
  to_plot <- gather(to_plot, key, value, -year)

# Set the file_path based on the loopfunction input 
file_path = paste0(exportdir, "10-scf-income-inequality/overall.jpeg")

  plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
    geom_line() +
    scale_color_brewer(palette = "Set1", guide = FALSE) +
    ggtitle(paste0("Net Worth and Dividends Exhibit More Inequality\nThan Wage Income")) +
    of_dollars_and_data_theme +
    scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
    labs(x = "Year" , y = "Gini")
  
  # Add a source and note string for the plots
  source_string <- "Source:  Federal Reserve Board, Survey of Consumer Finances (OfDollarsAndData.com)"
  note_string   <- "Note:  Calculates the Gini coefficient using income weighted by household." 
  
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

# Plot each var individually
plot_var_percentile <- function(x){  
  to_plot <-  scf_stack %>%
              group_by(year) %>%
              summarise_(`10th Percentile` = interp(
                        ~wtd.quantile(var, weights = wgt, probs=0.1), 
                        var = as.name(x)),
                        `30th Percentile` = interp(
                          ~wtd.quantile(var, weights = wgt, probs=0.3), 
                          var = as.name(x)),
                        `50th Percentile` = interp(
                          ~wtd.quantile(var, weights = wgt, probs=0.5), 
                          var = as.name(x)),
                        `70th Percentile` = interp(
                          ~wtd.quantile(var, weights = wgt, probs=0.7), 
                          var = as.name(x)),
                        `90th Percentile` = interp(
                          ~wtd.quantile(var, weights = wgt, probs=0.9), 
                          var = as.name(x))
                        ) %>%
              gather("percentiles", value, -year)
  
  # Set the file_path based on the loopfunction input 
  file_path = paste0(exportdir, "10-scf-income-inequality/", x, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = year, y = value, col = percentiles)) +
    geom_line() +
    scale_color_brewer(palette = "Set1", guide = FALSE) +
    ggtitle(paste0(x)) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = x)
  
  # Add a source and note string for the plots
  source_string <- "Source:  Federal Reserve Board, Survey of Consumer Finances (OfDollarsAndData.com)"
  note_string   <- "Note:  Calculates the weighted average." 
  
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

plot_var_percentile("wageinc")
plot_var_percentile("networth")
plot_var_percentile("intdivinc")
plot_var_percentile("bussefarminc")
plot_var_percentile("ssretinc")





# ############################  End  ################################## #