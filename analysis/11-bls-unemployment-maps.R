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

# Load in data
ue_stack <- readRDS(paste0(localdir, "11-bls-ue.Rds"))

# Filter the data to be only for annual unemployment rates and for states
ue_stack <- filter(ue_stack, period == "M13", area_type_code == "F", measure_code == "03")

# Also remove PR
ue_stack <- ue_stack[grepl(", PR", ue_stack$area_text) != 1,]

# Get the years list
years_list <- unique(ue_stack$year)
n_years    <- length(years_list)

# Get state data for the map
#all_states <- map_data("state")

# Get counties data for the map
all_counties <- map_data("county")

ue_stack$comma     <- gregexpr(pattern =',',ue_stack$area_text)
ue_stack$len       <- nchar(as.character(ue_stack$area_text))
ue_stack$region    <- trimws(
                        tolower(
                          unlist(sapply(
                            X = substr(ue_stack$area_text, 
                              as.numeric(ue_stack$comma) + 2, 
                              as.numeric(ue_stack$len)),
                            FUN = function(x){
                              if (x != "District of Columbia"){
                                state.name[grep(x, state.abb)]
                              } else{
                                x
                              }
                            }
                          ))
                        )
                      )

ue_stack$subregion <- trimws(
                        tolower(
                          gsub("Parish", "", 
                          gsub("County", "", 
                             substr(
                               ue_stack$area_text, 
                               1, 
                               as.numeric(ue_stack$comma) - 1)
                          )
                          )
                        )
                      )

# Set the subregion manually for DC
ue_stack[ue_stack$region == "district of columbia", "subregion"] <- "washington"

plot_year <- function(yr){
  to_plot <- ue_stack %>%
                filter(year == yr) %>%
                left_join(all_counties)
  
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "11-bls-unemployment/state-map-", yr, ".jpg")
  
  plot <- ggplot() + geom_polygon(data = to_plot,
                           aes(x = long, 
                               y = lat,
                               group = group, 
                               fill = as.numeric(to_plot$value)),
                           ) + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide= FALSE) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Unemployment Rate in ", yr)) +
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
  note_string   <- "Note:  To do." 
  
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
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

for (i in years_list){
  plot_year(i)
}

frames <- lapply(years_list, function(yr){
  image_read(paste0(exportdir, "11-bls-unemployment/state-map-", yr, ".jpg"))
})

image_write(image_animate(image_join(frames), fps = 2), 
            paste0(exportdir, "11-bls-unemployment/all_maps.gif"))





# ############################  End  ################################## #