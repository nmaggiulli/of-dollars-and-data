cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

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
ue_stack <- readRDS(paste0(localdir, "12-bls-ue.Rds")) %>%
              select(year, period, area_text, measure_text, area_type_code, value)

# Create 2016 data for the states only (take mean of the months)
ue_2016 <- filter(ue_stack, area_type_code == "A", year == 2016) %>%
              group_by(year, area_text, measure_text, area_type_code) %>%
                summarise(value = mean(as.numeric(value)),
                          period = "M13") %>%
                  select(year, period, area_text, measure_text, area_type_code, value)

# Filter the data to be only for annual unemployment rates and for states
ue_stack <- filter(ue_stack, 
                   year >= 2007, 
                   period == "M13", 
                   area_type_code == "A" | area_type_code == "F") %>%
                    rbind(ue_2016)

# Also remove PR
ue_stack <- ue_stack[grepl(", PR", ue_stack$area_text) != 1,]

# Get the years list
years_list   <- unique(ue_stack$year)
measure_list <- unique(ue_stack$measure_text)
geo_list     <- c("all_counties", "all_states")

first_year   <- min(years_list)

# Clean up the names on UE to match with the map data
# Break out county and state
ue_stack$comma  <- gregexpr(pattern =',',ue_stack$area_text)
ue_stack$len    <- nchar(as.character(ue_stack$area_text))
ue_stack_county <- filter(ue_stack, area_type_code == "F")   
ue_stack_county <-  mutate(ue_stack_county, region =
                      trimws(
                        tolower(
                          unlist(sapply(
                            X = substr(ue_stack_county$area_text, 
                                  as.numeric(ue_stack_county$comma) + 2, 
                                  as.numeric(ue_stack_county$len)),
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
                    )

ue_stack_county$subregion <- trimws(
                              tolower(
                                gsub("city|parish|county|/|\\.| |'", "", ignore.case = TRUE,
                                   substr(
                                     ue_stack_county$area_text, 
                                     1, 
                                     as.numeric(ue_stack_county$comma) - 1
                                  )
                                )
                              )
                            )

ue_stack_state <- filter(ue_stack, area_type_code == "A") %>%
                      mutate(region = trimws(tolower(area_text)),
                             subregion = "none")

ue_stack <- rbind(ue_stack_state, ue_stack_county) %>%
              select(year, value, measure_text, area_text, area_type_code, region, subregion)

# Set the subregion manually for DC
ue_stack[ue_stack$region == "district of columbia", "subregion"] <- "washington"

# Get counties data for the map
all_counties <- map_data("county")

# Get states data for the map
all_states <- map_data("state")

all_states$subregion <- NULL

all_counties$subregion <- gsub(" |\\.|city", "", ignore.case = TRUE,
                               all_counties$subregion
                          )

# Test the matching between the map data and the UE data
ue_unique <- unique(ue_stack[, c("region", "subregion")])
county_unique <- unique(all_counties[, c("region", "subregion")])

in_county_not_ue <- anti_join(county_unique, ue_unique)
in_ue_not_county <- anti_join(ue_unique, county_unique)

# Use a proper case function for the titles
# Found on StackOverflow
SentCase <- function(InputString){
  InputString <-
    paste(toupper(substring(InputString,1,1)),tolower(substring(InputString,2)),
          sep="")
}

ProperCase <- function(InputString){
  sapply(lapply(strsplit(InputString," "), SentCase), paste, collapse=" ")
}

# Create pct changes for unemployment, employment, and labor force since the first_year
# Also drop first_year for everything but the unemployment rate
ue_stack <- filter(ue_stack, year == first_year, measure_text != "unemployment rate") %>%
                  mutate(value_first = value) %>%
                    select(value_first, area_text, measure_text) %>%
                      full_join(ue_stack) %>%
                      mutate(value_num = 
                             ifelse(measure_text == "unemployment rate",
                                    as.numeric(value),
                                    as.numeric(value)/lag(as.numeric(value_first)) - 1
                              )
                    ) %>%
            filter((measure_text != "unemployment rate" & year > first_year) |
                      measure_text == "unemployment rate")

# Create a function to plot for each year and measure
plot_year_measure <- function(yr, measure, geo){
  
  # Create vars based on the geo variable
  if (geo == "all_counties"){
    geoname <- "county"
    at_code <- "F"
  } else {
    geoname <- "state"
    at_code <- "A"
  }
  
  # Subset to the measure so we can get the max and min y-values
  to_plot <- ue_stack %>%
              filter(measure_text == measure, area_type_code == at_code)
  
  # Find the range of y-values for mapping
  y_max <- max(to_plot$value_num, na.rm = TRUE)
  y_min <- min(to_plot$value_num, na.rm = TRUE)
  
  if (measure != "unemployment rate" & measure != "unemployment"){
    to_plot <- mutate(to_plot, value_num = ifelse(value_num > 0, 0, value_num * -1))
    y_max <- max(to_plot$value_num, na.rm = TRUE)
    y_min <- min(to_plot$value_num, na.rm = TRUE)
  } else if(measure == "unemployment") {
    to_plot <- mutate(to_plot, value_num = ifelse(value_num < 0, 0, value_num))
    y_max <- max(to_plot$value_num, na.rm = TRUE)
    y_min <- min(to_plot$value_num, na.rm = TRUE)
  }
  
  print(measure)
  print(y_max)
  print(y_min)
  
  to_plot <- to_plot %>%
                filter(year == yr) %>%
                left_join(get(geo, envir = .GlobalEnv))
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "12-bls-maps/", geoname, "-map-", measure, "-", yr, ".jpg")
  
  # Create a string to explain an increase or decrease in a measure
  # This will be used for titles and footnotes
  if (measure == "unemployment" | measure == "unemployment rate"){
    inc_dec <- "increase"
  } else{
    inc_dec <- "decrease"
  }
  
  #Create title
  if (measure != "unemployment rate"){
    top_title <- paste0("Percentage ", ProperCase(inc_dec)," in ", ProperCase(measure)," From\n", first_year, " to ", yr)
  } else {
    top_title <- paste0(ProperCase(measure)," by ", ProperCase(geoname), "\n", yr)
  }
  
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
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  
  if (measure != "unemployment rate"){
    note_string <- paste0("Note:  Changes below 0 are coded to 0.  Maximum ", measure, " ", inc_dec, " since ", first_year, " is: ", round(y_max*100,1), "%.")
    note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                            gp =gpar(fontfamily = "my_font", fontsize = 8))
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  }
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

# Loop through each year, geo, measure to create the plots
for (i in years_list){
  for (g in geo_list){
    for (m in measure_list){
      if ((i > first_year & g == "all_states") | 
          (i >= first_year & m == "unemployment rate" & i < 2016) |
          (i > first_year & g == "all_counties" & m == "labor force" & i < 2016)
      ){
        plot_year_measure(i, m, g)
      }
    }
  }
}

# Loop through the geographies and measures to create the GIFs
for (m in measure_list){
  for (g in geo_list){
    # Create vars based on the geo variable
    if (g == "all_counties" & (m == "unemployment rate" | m == "labor force")){
      geoname <- "county"
      at_code <- "F"
      if (m == "unemployment rate"){
        yr_list <- years_list[1:(length(years_list) - 1)]
      } else {
        yr_list <- years_list[2:(length(years_list) - 1)]
      }
    } else {
      geoname <- "state"
      at_code <- "A"
      if (m != "unemployment rate"){
        yr_list <- years_list[2:length(years_list)]
      } else {
        yr_list <- years_list
      }
    }
    
  # Read in the the individual images
  frames <- lapply(yr_list, function(yr){
    image_read(paste0(exportdir, "12-bls-maps/", geoname, "-map-", m, "-", yr, ".jpg"))
  })
  
  # Make animation from the frames read in during the prior step
  image_write(image_animate(image_join(frames), fps = 1), 
              paste0(exportdir, "12-bls-maps/all-", geoname, "-", m,"-maps.gif"))
  
  }      
}





# ############################  End  ################################## #