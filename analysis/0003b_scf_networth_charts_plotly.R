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
library(Hmisc)

########################## Start Program Here ######################### #

# Plotly credentials 
py <- plotly("ofdollarsanddata",as.character(plotly_api_key))

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# Filter data to exclude those over 65
scf_stack <- filter(scf_stack, agecl != "75+" & agecl != "65-74")

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year  <- max(scf_stack$year)

# Create lists of education class and age class to loop over
edcl_list  <- sort(unique(scf_stack$edcl))

# Create a function to make the data needed for plotly
# I did this since it does not look like plot_ly can run inside a loop
create_plotly_data <- function(edcl_string, out){
  to_plot <- filter(scf_stack, edcl == edcl_string, year == 2013) %>%
    group_by(year, agecl) %>%
    summarise(`10th` = wtd.quantile(networth, weights = wgt, probs=0.1),
              `25th` = wtd.quantile(networth, weights = wgt, probs=0.25),
              `50th` = wtd.quantile(networth, weights = wgt, probs=0.5),
              `75th` = wtd.quantile(networth, weights = wgt, probs=0.75)) %>%
    gather(`Net Worth Percentile`, value, -year, -agecl)
}

# Run the function to create 2 data sets for plotting
to_plot_1 <- create_plotly_data("High School Diploma/GED")
to_plot_2 <- create_plotly_data("College Degree")

# Create an x-axis for the plot_ly plot
xaxis <- list(title = "Age Group",
              showline = TRUE,
              showgrid = TRUE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

# Create an y-axis for the plot_ly plot
yaxis <- list(title = "Net Worth as of 2013 ($)",
              showgrid = TRUE,
              zeroline = TRUE,
              showline = TRUE,
              showticklabels = TRUE)
 
# Use the information above and create an interactive plot_ly plot
to_post_1 <- plot_ly(data = to_plot_1, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
        add_lines() %>%
        layout(title = "Education Level: High School Diploma/GED", xaxis = xaxis, yaxis = yaxis)

to_post_2 <- plot_ly(data = to_plot_2, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
  add_lines() %>%
  layout(title = "Education Level: College Degree", xaxis = xaxis, yaxis = yaxis)
                
# Post it publically on the ofdollarsanddata plotly profile
plotly_POST(x = to_post_1, filename =  "0003_networth_2013_edc_highschool", sharing =  "public")
plotly_POST(x = to_post_2, filename =  "0003_networth_2013_edc_college", sharing =  "public")
    


# ############################  End  ################################## #