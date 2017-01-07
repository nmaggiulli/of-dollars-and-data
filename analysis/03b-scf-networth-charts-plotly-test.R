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
library(plotly)
library(RColorBrewer)

########################## Start Program Here ######################### #

# Plotly credentials 
py <- plotly("ofdollarsanddata",as.character(plotly_api_key))

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

# Filter data to exclude those over 65
scf_stack <- filter(scf_stack, agecl != "75+" & agecl != "65-74")

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year  <- max(scf_stack$year)

# Create lists of education class and age class to loop over
edcl_list  <- sort(unique(scf_stack$edcl))

create_plotly_data <- function(edcl_string, out){
  to_plot <- filter(scf_stack, edcl == edcl_string, year == 2013) %>%
    group_by(year, agecl) %>%
    summarise(`10th` = quantile(networth, probs=0.1),
              `25th` = quantile(networth, probs=0.25),
              `50th` = quantile(networth, probs=0.5),
              `75th` = quantile(networth, probs=0.75)) %>%
    gather(`Net Worth Percentile`, value, -year, -agecl)
}

to_plot_1 <- create_plotly_data("High School Diploma/GED")
to_plot_2 <- create_plotly_data("College Degree")


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

yaxis <- list(title = "Net Worth as of 2013 ($)",
              showgrid = TRUE,
              zeroline = TRUE,
              showline = TRUE,
              showticklabels = TRUE)
 
# Create a plotly interactive plot as well
to_post_1 <- plot_ly(data = to_plot_1, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
        add_lines() %>%
        layout(title = "Education Level: High School Diploma/GED", xaxis = xaxis, yaxis = yaxis)

to_post_2 <- plot_ly(data = to_plot_2, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
  add_lines() %>%
  layout(title = "Education Level: College Degree", xaxis = xaxis, yaxis = yaxis)
                
# Post it publically on the ofdollarsanddata plotly profile
plotly_POST(x = to_post_1, filename =  "03-networth-2013-edc-highschool", sharing =  "public")
plotly_POST(x = to_post_2, filename =  "03-networth-2013-edc-college", sharing =  "public")
    


# ############################  End  ################################## #