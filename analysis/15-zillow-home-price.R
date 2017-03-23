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
library(tools)

########################## Start Program Here ######################### #

start_year <- as.Date("2000-01-01")


# Read in data frames
metro_mediansold_all <- readRDS(paste0(localdir, "15_metro_mediansold_all.Rds"))

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

all_tiers <- bind_rows(metro_zhvi_top, metro_zhvi_bottom, metro_zhvi_middle)

unique_metros <- unique(all_tiers$RegionName)

for (n in unique_metros){

  # Subset the data to plot by the region name
  to_plot <- filter(all_tiers, RegionName == n) %>%
              arrange(RegionID, type, year)
  
  # Get the region_id for file naming
  region_id <- unique(to_plot$RegionID)
  
  # Get the years list
  first_year  <- min(to_plot$year)
  last_year   <- max(to_plot$year)
  
  # Create the index for the price series
  for (i in 1:nrow(to_plot)){
    if (to_plot[i, "year"] == first_year){
      last_value <- to_plot[i, "price"]
      to_plot[i, "index"] <- 100
    } else {
      pct_change <- (to_plot[i, "price"] - last_value) / last_value
      last_value <- to_plot[i, "price"]
      to_plot[i, "index"] <- (1 + pct_change) * to_plot[i - 1, "index"]
    }
  }
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "15-zillow-home-price/zhvi-", region_id ,".jpeg")
  
  # Create the plot
  plot <- ggplot(to_plot, aes(x = year, y = index, col = type)) +
            geom_line() +
            geom_text_repel(data = filter(to_plot, year == last_year),
                            aes(year,
                                index, 
                                label = type,
                                family = "my_font")) +
            scale_color_discrete(guide = FALSE) +
            of_dollars_and_data_theme +
            scale_y_continuous(limits = c(50, 300)) +
            ggtitle(paste0("Home Sale Values By Tier\n", n)) +
            labs(x = "Year", y = "Index (2000 = 100)")
  
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



    




# ############################  End  ################################## #