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
library(plotly)
library(RColorBrewer)
library(ggrepel)
library(Hmisc)

########################## Start Program Here ######################### #

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# Filter data to exclude those over 65
scf_stack <- filter(scf_stack, agecl != "75+" & agecl != "65-74")

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year  <- max(scf_stack$year)

# Create lists of education class and age class to loop over
edcl_list  <- sort(unique(scf_stack$edcl))

# Create a function to plot the networth charts by type
# Type will define the type of chart
plot_networth <- function(type){  
  
  # Create a counter
  n <- 1
  
  # Loop through the education list in order to create plots
  for (j in edcl_list){
    # Filter the data to the correct education level
    # Then group by year and calculate networth percentiles
    # For type == 1 also subset to the last year in the data
    if (type == 1){
      to_plot <- filter(scf_stack, edcl == j, year == last_year) %>%
                      group_by(year, agecl) %>%
                  summarise(`10th Percentile` = wtd.quantile(networth, weights = wgt, probs=0.1),
                            `25th Percentile` = wtd.quantile(networth, weights = wgt, probs=0.25),
                            `50th Percentile` = wtd.quantile(networth, weights = wgt, probs=0.5)) %>%
                gather(`Net Worth Percentile`, value, -year, -agecl)
    } else if (type == 2 || type == 3){
      if (type == 2){
        to_plot <- filter(scf_stack, edcl == j) %>%
          group_by(year, agecl) %>%
            summarise(`10th` = wtd.quantile(networth, weights = wgt, probs=0.1),
                    `25th` = wtd.quantile(networth, weights = wgt, probs=0.25),
                    `50th` = wtd.quantile(networth, weights = wgt, probs=0.5)) %>%
            gather(`Net Worth Percentile`, value, -year, -agecl)
        } else if (type == 3){
          to_plot <- filter(scf_stack, edcl == j) %>%
            group_by(year, agecl) %>%
            summarise(`90th` = quantile(networth, weights = wgt, probs=0.90)) %>%  
            gather(`Net Worth Percentile`, value, -year, -agecl)
        }
    }
  
    # Convert the Net Worth percentiles to factors
    if (type == 1){
    # Alter certain variables to be factors
      to_plot$`Net Worth Percentile` <- factor(to_plot$`Net Worth Percentile`,
                                             levels = c("10th Percentile", "25th Percentile", "50th Percentile"))
    } else if (type == 2){
      to_plot$`Net Worth Percentile` <- factor(to_plot$`Net Worth Percentile`,
                                               levels = c("10th", "25th", "50th"))
    } else if (type == 3){
      to_plot$`Net Worth Percentile` <- factor(to_plot$`Net Worth Percentile`,
                                               levels = c("90th"))
    }
    
    # Define the y_unit for the y-axis
    y_unit <- 10^ceiling(min(log10(abs(max(to_plot$value))), log10(abs(min(to_plot$value)))))
    
    # Function to find a rounded max/min based on the specifications of y_unit
    create_max_min <- function(x, unit, ceilfloor) {
      ceilfloor(x/unit)*unit
    }
    
    y_max <- create_max_min(max(to_plot$value), y_unit, ceiling)
    y_min <- create_max_min(min(to_plot$value), y_unit, floor)
    
    # If the distance between the max and min is too large, increase y_unit
    # until the distance is less than 10 ticks away
    while (ceiling(abs(y_max - y_min))/y_unit > 10){
      y_unit <- y_unit * 2
    }
    
    # Define a new y_max if the y_unit has changed
    y_max <- create_max_min(y_max, y_unit, ceiling)
      
    # Assign the data frame to another name to exmaine after plotting 
    assign(paste0("to_plot_", n), to_plot) 
                
    if (type == 1){
      # Set the file_path based on the function input 
      file_path = paste0(exportdir, "0003_scf_networth_charts/edc_", as.character(n), ".jpeg")
      
      # Create a dynamic title based upon the agecl and edcl
      top_title <- paste0("Education Level:  ", j, "\n", last_year)
      
      # Create plot with the correct theme and for the last year only
      plot <- ggplot(to_plot, aes(x = agecl, y = value, col = `Net Worth Percentile`, group = `Net Worth Percentile`)) +
        geom_line() +
        geom_text_repel(data = filter(to_plot, agecl == "55-64"), 
                        aes(agecl, value, label = `Net Worth Percentile`, family = "my_font"), 
                        size = 2.8,
                        nudge_y = 10) +
        scale_colour_brewer(palette="Set1", guide = FALSE) +
        ggtitle(top_title)  +
        scale_y_continuous(labels = dollar, limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = y_unit)) +
        of_dollars_and_data_theme +
        labs(x = "Age Group" , y = "Net Worth ($)")
    } else if (type == 2 || type == 3){
      if (type == 2){
        file_path = paste0(exportdir, "0003_scf_networth_charts/edc_", as.character(n), "by_age_year", ".jpeg")
      } else if (type == 3){
        file_path = paste0(exportdir, "0003_scf_networth_charts/edc_", as.character(n), "by_age_year_90", ".jpeg")
      }
      # Create a dynamic title based upon the agecl and edcl
      top_title <- paste0("Education Level:  ", j, "\nBy Age Group Over Time")
      
      # Create plot with the correct theme and make it a facet to show multiple age groups
      plot <- ggplot(to_plot, aes(x = year, y = value, col = `Net Worth Percentile`)) +
        geom_line() +
        facet_grid(. ~ agecl) +
        scale_colour_brewer(palette="Set1", guide = guide_legend()) +
        theme(legend.position="bottom") +
        ggtitle(top_title)  +
        scale_x_continuous(breaks = seq(first_year, last_year, by = 9)) +
        scale_y_continuous(labels = dollar, limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = y_unit)) +
        of_dollars_and_data_theme +
        labs(x = "Year" , y = "Net Worth ($)")
    }
    
    # Add a source and note string for the plots
    source_string <- "Source:  Federal Reserve Board, Survey of Consumer Finances (OfDollarsAndData.com)"
    note_string   <- "Note:  Net worth percentiles are shown at the household level.  Includes home equity." 
    
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
      
    # Increment the counter
    n <- n + 1
  }
}

# Create charts by education level with age group as X-axis
plot_networth(1)

# Create charts for each age edc group, year as the x-axis (uses facet)
plot_networth(2)

# Create charts for each age edc group, year as the x-axis (uses facet for the 90th percentile)
plot_networth(3)


# ############################  End  ################################## #