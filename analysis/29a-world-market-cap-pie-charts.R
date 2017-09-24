cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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

world_cap <- readRDS(paste0(localdir, "29-dimson-world-market-cap.Rds")) %>%
              arrange(year, desc(region)) %>%
              group_by(year) %>% 
              mutate(pos = cumsum(pct)- pct/2)

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#984EA3", "grey", "#E41A1C", "#377EB8", "#A65628")
  
years <- unique(world_cap$year)

for (yr in years){
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "29-e-pluribus-unum/dimson-world-market-cap-", yr, ".jpeg")
  
  # Create title with ticker in subtitle
  top_title <- paste0("Share of World Stock Market\n", yr)
  
  to_plot <- filter(world_cap, year == yr)
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = factor(1), y = pct, fill = region)) +
    geom_bar(width = 1, stat = "identity") +
    geom_label(data=to_plot,
               aes(x= factor(1), y=pos, label = paste0(region, " ", pct*100, "%"), family = "my_font")) +
    coord_polar("y", start=0) +
    ggtitle(top_title) +
    guides(fill = FALSE) +
    scale_fill_manual(guide = FALSE, values=my_palette) +
    of_dollars_and_data_theme +
    theme(axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()
    ) +
    scale_y_continuous(label = percent)
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Triumph of the Optimists - Dimson, et al. (OfDollarsAndData.com)"
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}



# ############################  End  ################################## #