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
library(maps)
library(magick)

########################## Start Program Here ######################### #

# Create a function to read in data and select the start year for maps
plot_data <- function(file_name, yr){
  to_plot <- readRDS(paste0(localdir, "15_", file_name, ".Rds"))
  to_plot <- filter_(to_plot, ~year >= yr) %>%
              arrange_(~RegionID, ~year)
  
  # Get the years list
  years_list   <- unique(round(to_plot$year, digits = 0))
  first_year   <- min(years_list)
  
  # Get states data for the map
  all_states           <- map_data("state")
  all_states$subregion <- NULL
  
  # Find the range of y-values for mapping
  y_max <- max(to_plot$price, na.rm = TRUE)
  y_min <- min(to_plot$price, na.rm = TRUE)
  
  to_plot <- to_plot %>%
                left_join(all_states)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "15-zillow-home-price-maps/", file_name, "-", yr, ".jpg")
  
  # Create the plot
  plot <- ggplot() + geom_polygon(data = to_plot,
                           aes(x = long, 
                               y = lat,
                               group = group, 
                               fill = to_plot$value_num)
                           ) + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide= FALSE, limits = c(y_min, y_max)) +
  of_dollars_and_data_theme +
  ggtitle(top_title) +
  theme(axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
  )
  # Add a source and note string for the plots
  source_string <- "Source:  Bureau of Labor Statistics (OfDollarsAndData.com)"
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  note_string <- paste0("Note:  Changes below 0 are coded to 0.  Maximum ", measure, " ", inc_dec, " since ", first_year, " is: ", round(y_max*100,1), "%.")
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_data("state_mediansold_all", 2000)

    
  # Read in the the individual images
  frames <- lapply(yr_list, function(yr){
    image_read(paste0(exportdir, "12-bls-maps/", geoname, "-map-", m, "-", yr, ".jpg"))
  })
  
  # Make animation from the frames read in during the prior step
  image_write(image_animate(image_join(frames), fps = 1), 
              paste0(exportdir, "12-bls-maps/all-", geoname, "-", m,"-maps.gif"))
  
     





# ############################  End  ################################## #