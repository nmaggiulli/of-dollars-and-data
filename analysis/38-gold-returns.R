cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds")) %>%
                    select(year, Gold)

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year, "%d/%m/%y")

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

# Create an index to calculate drawdowns
for (i in 1:nrow(full_bv_returns)){
  if (i == 1){
    full_bv_returns[i, "index"] <- 100
  } else {
    full_bv_returns[i, "index"] <- full_bv_returns[(i-1), "index"] * (1 + full_bv_returns[(i-1), "Gold"])
  }
}

############################### First Returns Plot ###############################  
# Set the file_path for the output
file_path = paste0(exportdir, "38-gold-returns/gold-returns.jpeg")

to_plot <- full_bv_returns

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = year, y = Gold)) +
  geom_bar(stat = "identity", fill = "gold") +
  ggtitle(paste0("Gold is a Very Volatile Asset Class")) +
  scale_y_continuous(label = percent, limits = c(-0.5, 1.25)) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Annual Real Return (%)")

# Add a source and note string for the plots
source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 

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

############################### Plot drawdowns ###############################  

# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
  loc_max <- 0
  for (i in 1:(nrow(vp))){
    if (vp[i, 2] < loc_max & i != 1){
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- vp[i, 2]/loc_max - 1
    } else{
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- 0
      loc_max  <- vp[i, 2]
    }
  }
  return(dd)
}

# Run the drawdown function for the index
to_plot <- drawdown_path(full_bv_returns[, c("year", "index")])

# Calculate longest drawdown
# Initialize counters
longest_dd <- 0
counter <- 0

# Find the 
for (j in 1:nrow(to_plot)){
  if (to_plot[j, "pct"] == 0){
    longest_dd <- max(longest_dd, counter)
    counter <- 1
  } else{
    counter <- counter + 1
  }
}

# Find min and max dates
start_date <- min(to_plot$date)
end_date   <- max(to_plot$date)

# File path to save
file_path <- paste0(exportdir, "38-gold-returns/gold-drawdowns.jpeg")

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = pct)) +
  geom_area(fill = "red") +
  ggtitle(paste0("Gold Had a Real Drawdown Lasting ", longest_dd, " Years")) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(label = percent, limits = c(-1, 0)) +
  scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), date_breaks = "5 years") +
  labs(x = "Year", y = "Percentage of Value Lost")

  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 
  
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

  
