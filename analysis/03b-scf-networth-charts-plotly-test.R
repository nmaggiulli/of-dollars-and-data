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

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

# Plotly credentials 
py <- plotly("ofdollarsanddata",as.character(plotly_api_key))

# Define first_year and last_year dynamically for the charts
first_year <- min(scf_stack$year)
last_year  <- max(scf_stack$year)

# Define strings for each age class based on the definitions above
scf_stack$agecl <- ifelse(scf_stack[,"agecl"] == 1, "<35", 
                   ifelse(scf_stack[,"agecl"] == 2, "35-44",
                   ifelse(scf_stack[,"agecl"] == 3, "45-54",   
                   ifelse(scf_stack[,"agecl"] == 4, "55-64",
                   ifelse(scf_stack[,"agecl"] == 5, "65-74",
                   ifelse(scf_stack[,"agecl"] == 6, "Over 75", "99"))))))

# Define strings for education class
scf_stack$edcl <- ifelse(scf_stack[,"edcl"]  == 1, "No High School Diploma/GED", 
                   ifelse(scf_stack[,"edcl"] == 2, "High School Diploma/GED",
                   ifelse(scf_stack[,"edcl"] == 3, "Some College",   
                   ifelse(scf_stack[,"edcl"] == 4, "College Degree", "99"))))

# Make edcl into a factor
scf_stack$edcl <- factor(scf_stack$edcl,levels = c("No High School Diploma/GED", 
                                                    "High School Diploma/GED", 
                                                    "Some College", 
                                                    "College Degree"))

# Create lists of education class and age class to loop over
edcl_list  <- sort(unique(scf_stack$edcl))

to_plot <- filter(scf_stack, edcl == "College Degree", year == 2013) %>%
  group_by(year, agecl) %>%
  summarise(`10th` = quantile(networth, probs=0.1),
            `25th` = quantile(networth, probs=0.25),
            `50th` = quantile(networth, probs=0.5)) %>%
  gather(`Net Worth Percentile`, value, -year, -agecl)

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
  
  yaxis <- list(title = "Net Worth ($)",
                showgrid = TRUE,
                zeroline = TRUE,
                showline = TRUE,
                showticklabels = TRUE)
 
  # Create a plotly interactive plot as well
  plot_ly(data = to_plot, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
          add_lines() %>%
          layout(title = "College Degree", xaxis = xaxis, yaxis = yaxis)
                
#   # Post it publically on the ofdollarsanddata plotly profile
#   plotly_POST(x = to_post, filename =  paste0("03-networth_edcl_", n), sharing =  "public")
    


# ############################  End  ################################## #