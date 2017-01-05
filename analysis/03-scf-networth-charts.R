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
scf_stack$edcl <- ifelse(scf_stack[,"edcl"] == 1, "No High School Diploma/GED", 
                   ifelse(scf_stack[,"edcl"] == 2, "High School Diploma/GED",
                   ifelse(scf_stack[,"edcl"] == 3, "Some College",   
                   ifelse(scf_stack[,"edcl"] == 4, "College Degree", "99"))))

# Create lists of education class and age class to loop over
edcl_list  <- unique(scf_stack$edcl)

# Loop through the education list in order to create plots
# Create a counter
n <- 1
for (j in edcl_list){
  # Filter the data to the correct age and education 
  # Then group by year and calculate networth percentiles
  to_plot <- filter(scf_stack, edcl == j, year == 2013) %>%
                  group_by(year, agecl) %>%
              summarise(`10th` = quantile(networth, probs=0.1),
                        `25th` = quantile(networth, probs=0.25),
                        `50th` = quantile(networth, probs=0.5),
                        `75th` = quantile(networth, probs=0.75)) %>%
            gather(`Net Worth Percentile`, value, -year, -agecl)
  
  plot_n <- filter(to_plot, `Net Worth Percentile` == "n_obs")
  to_plot <- filter(to_plot, `Net Worth Percentile` != "n_obs")

  to_plot$`Net Worth Percentile` <- factor(to_plot$`Net Worth Percentile`,
                                           levels = c("75th", "50th", "25th", "10th"))
  to_plot$agecl <- factor(to_plot$agecl,levels = c("<35", "35-44", "45-54", "55-64",
                                                 "65-74", "Over 75"))
  
  # Assign the data frame to another name to exmaine after plotting 
  assign(paste0("to_plot_", n), to_plot) 
              
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "03-scf-networth-charts/edc_", as.character(n), ".jpeg")
  
  # Create a dynamic title based upon the agecl and edcl
  top_title <- paste0("Education Level:  ", j)
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value, col = `Net Worth Percentile`, group = `Net Worth Percentile`)) +
    geom_line() +
    ggtitle(top_title)  +
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
  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Create a plotly interactive plot as well
  to_post <- plot_ly(data = to_plot, x = ~agecl, y = ~value, color = ~`Net Worth Percentile`) %>%
                add_lines()
                
  # Post it publically on the ofdollarsanddata plotly profile
  plotly_POST(x = to_post, filename =  paste0("03-networth_edcl_", n), sharing =  "public")
    
  
  # Increment the counter
  n <- n + 1
}



# ############################  End  ################################## #