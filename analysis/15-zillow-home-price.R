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
plot_data <- function(file_name, yr, top_title){
  to_plot <- readRDS(paste0(localdir, "15_", file_name, ".Rds"))
  to_plot <- filter_(to_plot, ~year >= yr) %>%
              arrange_(~RegionID, ~year) 
  
  to_plot <- mutate(to_plot, region = tolower(RegionName))
  
  print(head(to_plot))
  
  # Get the years list
  first_year  <- min(to_plot$year)
  last_year   <- max(to_plot$year)
  
  # Get states data for the map
  all_states           <- map_data("state")
  all_states$subregion <- NULL
  
  # Create a diff variable for % changes over time
  to_plot <- to_plot %>%
              filter(year== first_year | year == last_year) %>%
              arrange(RegionID, year) %>%
              mutate(diff = price/lag(price) - 1) %>%
              filter(year == last_year) %>%
                left_join(all_states)
  
  # Find the range of y-values for mapping
  y_max <- max(to_plot$diff, na.rm = TRUE)
  y_min <- min(to_plot$diff, na.rm = TRUE)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "15-zillow-home-price/", file_name, "-", yr, ".jpg")
  
  print(head(to_plot))
  
  # Create the plot
  plot <- ggplot() + geom_polygon(data = to_plot,
                           aes(x = long, 
                               y = lat,
                               group = group, 
                               fill = to_plot$diff)
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
  source_string <- "Source:  Zillow Research (OfDollarsAndData.com)"
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  note_string <- paste0("Note:  ")
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_data("state_mediansold_all", 
          2000,
          "Median Sold Price All Homes\n2000 - 2017")

plot_data("state_zhvi_toptier", 
          2000,
          "Median Sold Price Top Tier Homes\n2000 - 2017")

plot_data("state_zhvi_bottomtier", 
          2000,
          "Median Sold Price Bottom Tier Homes\n2000 - 2017")

    
  # Read in the the individual images
  # frames <- lapply(yr_list, function(yr){
  #   image_read(paste0(exportdir, "12-bls-maps/", geoname, "-map-", m, "-", yr, ".jpg"))
  # })
  # 
  # # Make animation from the frames read in during the prior step
  # image_write(image_animate(image_join(frames), fps = 1), 
  #             paste0(exportdir, "12-bls-maps/all-", geoname, "-", m,"-maps.gif"))
  # 
  #    
  # 




# ############################  End  ################################## #