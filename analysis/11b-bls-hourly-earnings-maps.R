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

# Load in UE data
oe_stack_orig <- readRDS(paste0(localdir, "05-bls-oe.Rds"))

# Filter the data to be only for annual unemployment rates and for states
bls_oe_filtered <- filter(oe_stack_orig, areatype_name == "State",
                          datatype_name %in% c("Hourly 10th percentile wage", 
                                               "Hourly 25th percentile wage",
                                               "Hourly median wage",
                                               "Hourly 75th percentile wage",
                                               "Hourly 90th percentile wage"),
                          str_trim(value) !=  "-",
                          occupation_name == "All Occupations") %>%
                    mutate(region = trimws(tolower(area_name))) %>%
                    select(year, value, footnote_codes, area_name, areatype_name,
                      industry_name, occupation_name, datatype_name, region)

# Add additonal factor for datatype_name
levels(bls_oe_filtered$datatype_name) <- c(levels(bls_oe_filtered$datatype_name), "Hourly 50th percentile wage")
bls_oe_filtered$datatype_name[bls_oe_filtered$datatype_name == "Hourly median wage"] <- "Hourly 50th percentile wage"

# Get state data for the map
all_states <- map_data("state")

# Create a quintile list
quintile_list <- c("Hourly 10th percentile wage", 
                   "Hourly 25th percentile wage",
                   "Hourly 50th percentile wage",
                   "Hourly 75th percentile wage",
                   "Hourly 90th percentile wage")

# Create a ymin and ymax for the coloring of the maps
y_max <- max(as.numeric(bls_oe_filtered$value), na.rm = TRUE)
y_min <- min(as.numeric(bls_oe_filtered$value), na.rm = TRUE)

# Use a function to subset the data and join on the coordinates before mapping
plot_quintile <- function(q){
  to_plot <- bls_oe_filtered %>%
                filter(datatype_name == q) %>%
                left_join(all_states)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "11-bls-maps/oe-state-map-", q, ".jpg")
  
  plot <- ggplot() + geom_polygon(data = to_plot,
                           aes(x = long, 
                               y = lat,
                               group = group, 
                               fill = as.numeric(to_plot$value)),
                           colour = "white"
                           ) + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide= FALSE, limits = c(y_min, y_max)) +
  of_dollars_and_data_theme +
  ggtitle(paste0(q, "\n2015")) +
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
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

# Create a map for each quintile
for (i in quintile_list){
  plot_quintile(i)
}

# Read in the completed frames
frames <- lapply(quintile_list, function(q){
  image_read(paste0(exportdir, "11-bls-maps/oe-state-map-", q, ".jpg"))
})

# Create a GIF from the images
image_write(image_animate(image_join(frames), fps = 1), 
            paste0(exportdir, "11-bls-maps/oe-state-maps.gif"))





# ############################  End  ################################## #