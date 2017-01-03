cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

########################## Start Program Here ######################### #

# Set the LibreBaskerville font
windowsFonts(my_font=windowsFont("Libre Baskerville"))

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year <- max(scf_stack$year)

# Create lists of education class and age class to loop over
agecl_list <- unique(scf_stack$agecl)
edcl_list <- unique(scf_stack$edcl)

# As a reminder here are what the codes for agecl and edcl represent:
# agecl = age class, 1:<35, 2:35-44, 3:45-54, 4:55-64, 5:65-74, 6:>=75
# edcl = education class, 1 = no high school diploma/GED, 2 = high school diploma or GED,
#   3 = some college, 4 = college degree

# Loop through the lists in order to create plots
for (i in agecl_list){
  for (j in edcl_list){
    
    # Define strings for each age class based on the definitions above
    if (i == 1){
      agecl_string <- paste0("Less Than 35")
    } else if (i > 1 & i < 6){
      agecl_string <- paste0((35 + (i-2)*10), "-", (44 + (i-2)*10))
    } else if (i == 6){
      agecl_string <- paste0("75 or Older")
    }
    
    # Do the same for the education class
    if (j == 1){
      edcl_string <- "No High School Diploma/GED"
    } else if (j == 2){
      edcl_string <- "High School Diploma/GED"
    } else if (j == 3){
      edcl_string <- "Some College"
    } else if (j == 4){
      edcl_string <- "College Degree"
    }
    
    # Filter the data to the correct age and education 
    # Then group by year and calculate networth percentiles
    to_plot <- filter(scf_stack, agecl == i, edcl == j) %>%
                    group_by(year) %>%
                summarise(`10th` = quantile(networth, probs=0.1),
                          `25th` = quantile(networth, probs=0.25),
                          `50th` = quantile(networth, probs=0.5),
                          `75th` = quantile(networth, probs=0.75),
                          #`90th` = quantile(networth, probs=0.90),
                          n_obs = n()) %>%
              gather(`Net Worth Percentile`, value, -year)
    
    plot_n <- filter(to_plot, `Net Worth Percentile` == "n_obs")
    to_plot <- filter(to_plot, `Net Worth Percentile` != "n_obs")
    
    # Assign the data frame to another name to exmaine after plotting 
    assign(paste0("to_plot_", i, "_", j), to_plot) 
                
    # Set the file_path based on the function input 
    file_path = paste0(exportdir, "03-scf-networth-charts/age_", i, "_edc_", j, ".jpeg")
    
    # Create a dynamic title based upon the agecl and edcl
    top_title <- paste0("Age Cohort:  ", agecl_string, "\n", "Education Cohort:\n", edcl_string)
    
    plot <- ggplot(to_plot, aes(x = year, y = value, col = `Net Worth Percentile`)) +
      geom_line() +
      ggtitle(top_title)  +
      scale_x_continuous(breaks = seq(first_year, last_year, 3)) +
      scale_y_continuous(labels = dollar) +
      # Make a theme that matches the OfDollarsAndData.com blog
      theme(plot.title = element_text(family="my_font", size = 15, face="bold", margin = margin(0, 0, 10, 0)),
            axis.title.y = element_text(face = "bold", size = 11, family = "my_font", margin = margin(0, 10, 0, 0)),
            axis.text.y = element_text(color = "black"),
            axis.ticks.y = element_line(color = "black"),
            axis.title.x = element_text(face = "bold", size = 11, family = "my_font", margin = margin(10, 0, 0, 0)),
            axis.text.x = element_text(color = "black"),
            axis.ticks.x = element_line(color = "black"),
            legend.position="right") +
      labs(x = "Year" , y = "Net Worth ($)")
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}



# ############################  End  ################################## #