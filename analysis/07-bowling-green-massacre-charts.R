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
library(ggrepel)

########################## Start Program Here ######################### #

# "Import" the data
years <- seq(2011, 2017, 1)

# Recount the victims
victims <- rep(0, length(years))

victims_plus_injured <- rep(0, length(years))

victim_df <- as.data.frame(cbind(years, victims, victims_plus_injured))

# Also break out by religious status
religion_df <- as.data.frame(matrix(NA, nrow=4, ncol=0))

religion_df$religious_status <- c("Christian", "Muslim", "Jewish", "Non-religious")

religion_df$victims_by_religion <- rep(0, nrow(religion_df))

# Show percentage of total airtime given by large Media outlets
airtime_df <- as.data.frame(matrix(NA, nrow=4, ncol=0))
airtime_df$media <- c("Fox News", "CNN", "MSNBC", "Others")

airtime_df$pct_airtime <- as.numeric(rep(0, nrow(airtime_df)))

  
# Plot the victim, time trends
file_path = paste0(exportdir, "07-bowling-green-massacre-charts/victims.jpeg")

  plot <- ggplot(victim_df, aes(x = years, y = victims, col = "red"))  +
    geom_line() +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(limits = c(-1, 500)) +
    scale_x_continuous(breaks = seq(min(years), max(years), 1)) +
    ggtitle("Number of Bowling Green Massacre Deaths\n2011-2017")  +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Number of People Killed")


# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source: Alternative facts (OfDollarsAndData.com)"
note_string   <- "Note:  Perpetrators included in death count." 

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# Plot the victim + injured, time trends
file_path = paste0(exportdir, "07-bowling-green-massacre-charts/victims_and_injured.jpeg")

plot <- ggplot(victim_df, aes(x = years, y = victims_plus_injured, col = "red"))  +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(-1, 500)) +
  scale_x_continuous(breaks = seq(min(years), max(years), 1)) +
  ggtitle("Including Those Injured Leads to\na 3x Increase In Total Victims")  +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Number Killed + Injured")


# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source: Kellyanne Conway's imagination (OfDollarsAndData.com)"
note_string   <- "Note:  Injured includes those who experienced amnesia, strep throat, or papercuts." 

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable  <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# Plot the breakout by religious status
file_path = paste0(exportdir, "07-bowling-green-massacre-charts/religious_status.jpeg")

plot <- ggplot(religion_df, aes(x =  religious_status, y = victims_by_religion, fill = religious_status, col = religious_status))  +
  geom_bar(stat = "identity", position = "dodge") +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(0, 500)) +
  ggtitle("Religion Was a Factor in the Massacre")  +
  of_dollars_and_data_theme +
  labs(x = "Religious Status" , y = "Number of Victims")


# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source: Pipe dream (OfDollarsAndData.com)"
note_string   <- "Note:  Religious status was inferred based off of country of origin." 

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable  <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# Plot the breakout by media coverage
file_path = paste0(exportdir, "07-bowling-green-massacre-charts/media.jpeg")

plot <- ggplot(airtime_df, aes(x =  media, y = pct_airtime, fill = media, col = media))  +
  geom_bar(stat = "identity", position = "dodge") +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  ggtitle("Percentage of Airtime Dedicated to\nBowling Green Massacre")  +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  of_dollars_and_data_theme +
  labs(x = "Media Outlet" , y = "Percentage of Airtime Covered")


# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source: Nielsen Media (OfDollarsAndData.com)"
note_string   <- "Note:  'Others' excludes Breitbart news." 

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable  <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
