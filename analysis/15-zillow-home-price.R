cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(tools)
library(lubridate)

########################## Start Program Here ######################### #

start_year <- as.Date("2000-01-01")

# Write function to read in a particular tier
read_in_tier <- function(name){
  string <- deparse(substitute(name))
  temp <- readRDS(paste0(localdir, "15_metro_zhvi_", string,"tier.Rds"))
  temp <- filter(temp, year >= start_year)
  temp$type <- toTitleCase(string)
  assign(paste0("metro_zhvi_", string), temp, envir = .GlobalEnv)
}

read_in_tier(top)
read_in_tier(middle)
read_in_tier(bottom)

# Combine all tiers
all_tiers <- bind_rows(metro_zhvi_top, metro_zhvi_bottom, metro_zhvi_middle)

# Find a list of all cities with all 3 tiers
regions_3_tiers <- all_tiers %>%
                  filter(year == start_year, !is.na(price)) %>%
                  unite(region_type, RegionID, type, sep = "_") %>%
                  group_by(RegionName) %>%
                  summarise(n_tiers = length(unique(region_type))) %>%
                  filter(n_tiers == 3) %>%
                  select(RegionName)

# Subset tiers to only those with non-NA prices across all 3 regions
all_tiers <- all_tiers %>%
                inner_join(regions_3_tiers) %>%
                arrange(RegionID, type, year)

# Get the start_year_price for each metro area
start_year_price <- all_tiers %>%
                      filter(year == start_year) %>%
                      mutate(start_year_price = price) %>%
                      select(RegionID, start_year_price, type)

# Join on the start_year to create the index
all_tiers <- all_tiers %>%
              inner_join(start_year_price) %>%
              mutate(index = (price/start_year_price - 1)*100 + 100) 

# For exploratory purposes
biggest_change <- all_tiers %>%
                    group_by(RegionName, type) %>%
                    summarise(max_index = max(index),
                              min_index = min(index)) %>%
                    mutate(max_minus_min = max_index - min_index) %>%
                    arrange(max_minus_min)

# Create charts for particular summary areas

## Bottom Declines
bottom_cities <- c("Chicago, IL", "Detroit, MI", "Atlanta, GA", "St. Louis, MO")

## High volatility
highvol_cities <- c("Riverside, CA", "Stockton, CA", "Key West, FL", "Hilo, HI")

## Low Volatility
lowvol_cities <- c("Boulder, CO", "Erie, PA", "Ithaca, NY", "Johnson City, TN")

# Get the US middle tier home price as its own series
us_middle_tier <- filter(all_tiers, 
                         RegionName == "United States",
                         type == 'Middle')
us_middle_tier$type <- "U.S. Middle"

# Start for-loop for the 3 facet wraps to make
for (i in 1:3){
  if (i == 1){
    cities  <- bottom_cities
    outname <- "bottom-cities-facet"
    title   <- "Some Areas Shown Larger Inequality\nAcross Housing Tiers"
  } else if (i == 2){
    cities  <- highvol_cities
    outname <- "highvol-cities-facet"
    title   <- "Some Areas Had An Amplified Boom and Bust"
  } else if (i == 3){
    cities  <- lowvol_cities
    outname <- "lowvol-cities-facet"
    title   <- "Some Areas Shown No Evidence\nOf The Housing Crisis"
  }
  
  # Stack the middle tier US data frame 4 times
  us_changed <- bind_rows(us_middle_tier, us_middle_tier, us_middle_tier, us_middle_tier)
  
  # Change the region name to reflect the names of the cities
  us_changed$RegionName <- rep(cities, each = nrow(us_middle_tier))
  
  # Filter the all_tiers data to the cities we care about
  to_plot <- filter(all_tiers, RegionName %in% (cities))
  
  # Bind the US data with the cities we care about
  to_plot <- bind_rows(us_changed, to_plot)
  
  # Get the years list
  first_year  <- min(to_plot$year)
  last_year   <- max(to_plot$year)
  
  # Plot the result
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "15-zillow-home-price/", outname, ".jpeg")
  
  # Create the plot
  plot <- ggplot(to_plot, aes(x = year, y = index, col = type)) +
    geom_line() +
    facet_wrap(~RegionName) +
    scale_color_manual(guide = FALSE, values=my_palette) +
    of_dollars_and_data_theme +
    scale_y_continuous() +
    scale_x_date(date_breaks = "2 years", date_labels = "%y") +
    ggtitle(title) +
    labs(x = "Year", y = "Index (2000 = 100)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  Zillow, ", year(start_year),"-2017 (OfDollarsAndData.com)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  note_string <- paste0("Note:  The black line represents the middle U.S. housing tier.")
  
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

## This is for the individual charts with US middle tier included

# Find a unique list of metros
unique_metros <- unique(all_tiers$RegionName)

for (n in unique_metros){

  # Subset the data to plot by the region name
  to_plot <- filter(all_tiers, RegionName == n) %>%
              arrange(RegionID, type, year)
  
  if (n != "United States"){
    # Bind the US middle tier to the data
    to_plot <- bind_rows(to_plot, us_middle_tier)
  }
  
  # Get the years list
  first_year  <- min(to_plot$year)
  last_year   <- max(to_plot$year)
  
  # Get start starting and ending home values of the middle tier
  middle_start <- filter(to_plot, type == "Middle", year == first_year) %>%
                    select(price)
  middle_end   <- filter(to_plot, type == "Middle", year == last_year) %>%
                    select(price)
  
  # Find y-max
  y_max <- max(to_plot$index, na.rm = TRUE)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "15-zillow-home-price/zhvi-", n ,".jpeg")
  
  # Create the plot
  plot <- ggplot(to_plot, aes(x = year, y = index, col = type)) +
            geom_line() +
            geom_text_repel(data = filter(to_plot, year == last_year),
                            aes(year,
                                index, 
                                label = type,
                                family = "my_font")) +
            scale_color_manual(guide = FALSE, values=my_palette) +
            of_dollars_and_data_theme +
            scale_y_continuous() +
            scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
            ggtitle(paste0("Zillow Home Value Index By Tier\n", n)) +
            labs(x = "Year", y = "Index (2000 = 100)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  Zillow, ", year(start_year),"-2017 (OfDollarsAndData.com)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  note_string <- paste0("Note:  A middle tier home was valued at $", 
                        formatC(as.numeric(middle_start), format="f", digits=0, big.mark=","), 
                        " in ", 
                        year(start_year), 
                        " and $", 
                        formatC(as.numeric(middle_end), format="f", digits=0, big.mark=","), 
                        " in 2017.")
  
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}



    




# ############################  End  ################################## #