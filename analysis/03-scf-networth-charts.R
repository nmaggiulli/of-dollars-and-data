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
library(devtools)
library(plotly)

########################## Start Program Here ######################### #

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year  <- max(scf_stack$year)

# Plotly credentials
py <- plotly("ofdollarsanddata", "..........")

plotly_data <- filter(scf_stack, edcl == 4, year == 2013) %>%
  group_by(year, agecl) %>%
  summarise(`10th` = quantile(networth, probs=0.1),
            `25th` = quantile(networth, probs=0.25),
            `50th` = quantile(networth, probs=0.5),
            `75th` = quantile(networth, probs=0.75),
            n_obs = n()) %>%
  gather(`Net Worth Percentile`, value, -year, -agecl)

ggtest <- ggplot(data = plotly_data, aes(x = agecl, y = value, col = `Net Worth Percentile`)) +
    geom_line() +
  of_dollars_and_data_theme +
    ggtitle("Education Level:  College Degree")  +
    scale_x_continuous(breaks = seq(1, 6, 1)) +
    scale_y_continuous(labels = dollar) +
    labs(x = "Age Group" , y = "Net Worth ($)")

ggplotly(ggtest)

# Create lists of education class and age class to loop over
edcl_list  <- unique(scf_stack$edcl)

# As a reminder here are what the codes for agecl and edcl represent:
# agecl = age class, 1:<35, 2:35-44, 3:45-54, 4:55-64, 5:65-74, 6:>=75
# edcl = education class, 1 = no high school diploma/GED, 2 = high school diploma or GED,
#   3 = some college, 4 = college degree

# Loop through the lists in order to create plots
for (j in edcl_list){
  
  # Define strings for each age class based on the definitions above
#   if (i == 1){
#     agecl_string <- paste0("Less Than 35")
#   } else if (i > 1 & i < 6){
#     agecl_string <- paste0((35 + (i-2)*10), "-", (44 + (i-2)*10))
#   } else if (i == 6){
#     agecl_string <- paste0("75 or Older")
#   }
  
  # Define strings for education class
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
  to_plot <- filter(scf_stack, edcl == j, year == 2013) %>%
                  group_by(year, agecl) %>%
              summarise(`10th` = quantile(networth, probs=0.1),
                        `25th` = quantile(networth, probs=0.25),
                        `50th` = quantile(networth, probs=0.5),
                        `75th` = quantile(networth, probs=0.75),
                        n_obs = n()) %>%
            gather(`Net Worth Percentile`, value, -year, -agecl)
  
  plot_n <- filter(to_plot, `Net Worth Percentile` == "n_obs")
  to_plot <- filter(to_plot, `Net Worth Percentile` != "n_obs")

  to_plot$`Net Worth Percentile` <- factor(to_plot$`Net Worth Percentile`,
                                           levels=c("75th", "50th", "25th", "10th"))
  
  #Get agecl min and max for x-axis on plots
  first_agecl <- min(to_plot$agecl)
  last_agecl  <- max(to_plot$agecl)
  
  # Assign the data frame to another name to exmaine after plotting 
  assign(paste0("to_plot_", j), to_plot) 
              
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "03-scf-networth-charts/_edc_", j, ".jpeg")
  
  # Create a dynamic title based upon the agecl and edcl
  top_title <- paste0("Education Level:  ", edcl_string)
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value, col = `Net Worth Percentile`)) +
    geom_line() +
    ggtitle(top_title)  +
    scale_x_continuous(breaks = seq(first_agecl, last_agecl, 1)) +
    scale_y_continuous(labels = dollar) +
    of_dollars_and_data_theme +
    labs(x = "Age Group" , y = "Net Worth ($)")
  
  source_string <- "Source:  Federal Reserve Board, Survey of Consumer Finances"
  note_string   <- "Note:  Net worth percentiles are shown at the household level." 
  
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  ggplotly(plot)
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}



# ############################  End  ################################## #