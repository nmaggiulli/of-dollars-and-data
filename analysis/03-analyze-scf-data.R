cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)

########################## Start Program Here ######################### #

# Set the LibreBaskerville font
windowsFonts(LibreBaskerville=windowsFont("Libre Baskerville"))

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

# Create lists of education class and age class to loop over
agecl_list <- unique(scf_stack$agecl)
edcl_list <- unique(scf_stack$edcl)

# Loop through the lists in order to create plots
for (i in agecl_list){
  for (j in edcl_list){
    # Filter the data to the correct age and education 
    # Then group by year and calculate networth percentiles
    to_plot <- filter(scf_stack, agecl == i, edcl == j) %>%
                    group_by(year) %>%
                summarise(pct_10 = quantile(networth, probs=0.1),
                          pct_25 = quantile(networth, probs=0.25),
                          pct_50 = quantile(networth, probs=0.5),
                          pct_75 = quantile(networth, probs=0.75),
                          pct_90 = quantile(networth, probs=0.90),
                          n_obs = n()) %>%
              gather(networth_percentile, value, -year)
    
    plot_n <- filter(to_plot, networth_percentile == "n_obs")
    to_plot <- filter(to_plot, networth_percentile != "n_obs")
    
    # Assign the data frame to another name to exmaine after plotting 
    assign(paste0("to_plot_", i, "_", j), to_plot) 
                
    # Set the file_path based on the function input 
    file_path = paste0(exportdir, "03-scf-networth-charts/age_", i, "_edc_", j, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = year, y = value, col = networth_percentile)) +
      geom_line() +
      # Make a theme that matches the OfDollarsAndData.com blog
      theme(plot.title = element_text(family="LibreBaskerville", size = 15, face="bold", margin = margin(0, 0, 10, 0)),
            axis.title.y = element_text(face = "bold", size = 11, family = "LibreBaskerville", margin = margin(0, 10, 0, 0)),
            axis.text.y = element_text(color = "black"),
            axis.ticks.y = element_line(color = "black"),
            axis.title.x = element_text(face = "bold", size = 11, family = "LibreBaskerville", margin = margin(10, 0, 0, 0)),
            axis.text.x = element_text(color = "black"),
            axis.ticks.x = element_line(color = "black")) +
      labs(x = "Year" , y = "Networth ($)")
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}



# ############################  End  ################################## #