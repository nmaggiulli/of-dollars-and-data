cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
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
library(ggrepel)

########################## Start Program Here ######################### #

# Load data fom local library
bls_productivity <- readRDS(paste0(localdir, "0004_bls_productivity.Rds"))

# Filter out annual averages and to all employed individuals
bls_productivity <- filter(bls_productivity, 
                           !is.na(year_quarter), 
                           class_name == "All employed persons",
                           duration_name == "Index, base year = 100",
                           measure_name == "Employment" | measure_name == "Labor productivity (output per hour)",
                           sector_name == "Manufacturing" | sector_name == "Business")

# Set the file_path based on the function input 
file_path = paste0(exportdir, "0004_bls_productivity/bls-manf-bus-employment-productivity.jpeg")

# Define the first and last year_quarter
first_year_quarter <- max(min(bls_productivity[bls_productivity$sector_name == "Manufacturing", "year_quarter"]),
                          min(bls_productivity[bls_productivity$sector_name == "Business", "year_quarter"]))
last_year_quarter  <- min(max(bls_productivity[bls_productivity$sector_name == "Manufacturing", "year_quarter"]),
                          max(bls_productivity[bls_productivity$sector_name == "Business", "year_quarter"]))

# Filter the data again to show the same time series for each facet
to_plot <- filter(bls_productivity, year_quarter >= first_year_quarter & year_quarter <= last_year_quarter)

for (i in 1:nrow(to_plot)){
  if (to_plot[i, "year_quarter"] == first_year_quarter){
    last_value <- to_plot[i, "value"]
    to_plot[i, "value"] <- 100
  } else {
    pct_change <- (to_plot[i, "value"] - last_value) / last_value
    last_value <- to_plot[i, "value"]
    to_plot[i, "value"] <- (1 + pct_change) * to_plot[i - 1, "value"]
  }
}

midpoint <- (max(to_plot$value) + min(to_plot$value)) / 2

# Create a dynamic title based upon the agecl and edcl
top_title <- "While Manufacturing Productivity Soars,\nManufacturing Employment Is In Decline\n1987 - 2016"

# Create a ggplot of value by year_quarter
# Use geom_text_repel for better labelings
plot <- ggplot(to_plot, aes(x = year_quarter, y = value, col = measure_name)) +
  geom_line() +
  geom_text_repel(data = filter(to_plot, year_quarter == last_year_quarter),
                  aes(year_quarter, 
                      value, 
                      label = round(value, 0), 
                      family = "my_font"), 
                  size = 3, 
                  nudge_y = -10) +
  geom_text_repel(data = filter(to_plot, year_quarter == last_year_quarter, sector_name == "Manufacturing"),
                  aes(year_quarter, 
                      ifelse(value > midpoint, midpoint , midpoint - 70), 
                      col = measure_name,
                      label = str_wrap(measure_name, width = 20), 
                      family = "my_font"), 
                  size = 2.5,
                  segment.alpha = 0) +
  geom_text_repel(data = filter(to_plot, year_quarter == last_year_quarter, sector_name == "Business"),
                  aes(year_quarter, 
                      ifelse(value > midpoint, value + 10, value - 25), 
                      col = measure_name,
                      label = str_wrap(measure_name, width = 20), 
                      family = "my_font"), 
                  size = 2.5,
                  segment.alpha = 0) +
  facet_grid(. ~ sector_name) +
  scale_colour_brewer(palette="Set1", guide = FALSE) +
  theme(legend.position="bottom") +
  ggtitle(top_title)  +
  scale_x_date(breaks = seq(first_year_quarter, last_year_quarter, 365.25*7), date_labels = "%Y") +
  of_dollars_and_data_theme +
  
  labs(x = "Year" , y = "Index (1987 = 100)")

# Add a source and note string for the plots
source_string <- "Source:  Bureau of Labor Statistics (OfDollarsAndData.com)"
note_string   <- "Note:  The Business sector excludes government, private households, and select nonprofits." 

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