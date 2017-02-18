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
library(Hmisc)
library(reldist)
library(stringr)

########################## Start Program Here ######################### #

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

scf_stack[is.na(scf_stack$equitinc), "equitinc"] <- 0

year_list <- unique(scf_stack$year)
edcl_list <- unique(scf_stack$edcl)
agecl_list <- unique(scf_stack$agecl)

to_plot <- as.data.frame(matrix(NA, nrow = length(year_list), ncol = 0))

i <- 1
  for (y in year_list){
      filtered <- filter(scf_stack, year == y)
      to_plot[i, "Net Worth"]                    <- gini(filtered$networth, filtered$wgt)
      to_plot[i, "Wages"]                        <- gini(filtered$wageinc, filtered$wgt)
      to_plot[i, "Interest and Dividend Income"] <- gini(filtered$intdivinc, filtered$wgt)
      to_plot[i, "Assets"]                       <- gini(filtered$asset, filtered$wgt)
      to_plot[i, "Equity Income"]                <- gini(filtered$equitinc, filtered$wgt)
      to_plot[i, "year"]                         <- y
      i <- i + 1
  }
  to_plot <- gather(to_plot, key, value, -year)

# Set the file_path based on the loopfunction input 
file_path = paste0(exportdir, "10-scf-inequality/inequality-by-type.jpeg")

  plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
    geom_line() +
    geom_text_repel(data = filter(to_plot, year == 2004, key != "Interest and Dividend Income"), 
                    aes(year, 
                        col = key,
                        label = str_wrap(key, width = 30),
                        family = "my_font"),
                    max.iter =  3000,
                    nudge_y = -0.035) +
    geom_text_repel(data = filter(to_plot, year == 2004, key == "Interest and Dividend Income"), 
                    aes(year, 
                        col = key,
                        label = str_wrap(key, width = 30),
                        family = "my_font"),
                    max.iter =  3000,
                    nudge_y = 0.035) +
    scale_color_brewer(palette = "Set1", guide = FALSE) +
    ggtitle(paste0("Financial Assets Exhibit More Inequality\nThan Wage Income in the U.S.")) +
    of_dollars_and_data_theme +
    scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
    labs(x = "Year" , y = "Inequality Measure (0 = Equality, 1 = Inequality)")
  
  # Add a source and note string for the plots
  source_string <- "Source:  Federal Reserve Board, Survey of Consumer Finances (OfDollarsAndData.com)"
  note_string   <- "Note:  The inequality measure is a weighted Gini coefficient." 
  
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