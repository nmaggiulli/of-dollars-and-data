cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(zoo)
library(tidyverse)

folder_name <- "0144_wealth_tax_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

tax_brackets <- c(32*10^6, 50*10^6, 250*10^6, 500*10^6, 1*10^9, 2.5*10^9, 5*10^9, 10*10^9, 300*10^9)
tax_rates <- seq(0.01, 0.08, 0.01)

net_worths <- c(32*10^6, 50*10^6, 250*10^6, 500*10^6, 1*10^9, 2.5*10^9, 5*10^9, 10*10^9)

final_results <- data.frame()

counter <- 1
for(net_worth in net_worths){
  final_tax <- 0 
  for(i in 1:(length(tax_brackets) - 1)){
    current_bracket <- tax_brackets[i]
    next_bracket <- tax_brackets[(i+1)]
    current_rate <- tax_rates[i]
    
    if(net_worth <= current_bracket){
      current_tax <- 0
    } else{
      if(net_worth >= next_bracket){
        current_tax <- (next_bracket - current_bracket) * current_rate
      } else{
        current_tax <- (net_worth - current_bracket) * current_rate
      }
    }
    
    final_tax <- final_tax + current_tax
  }
  final_results[counter, "net_worth"] <- net_worth
  final_results[counter, "tax_amount"] <- final_tax
  final_results[counter, "tax_rate"] <- final_tax/net_worth
  counter <- counter + 1
}

plot_nw <- function(max_nw, name){
  
  file_path <- paste0(out_path, "/tax_total_by_net_worth_", name, ".jpeg")
  
  source_string <- paste0("Source:  OfDollarsAndData.com")
  
  to_plot <- final_results %>%
                filter(net_worth <= max_nw) %>%
                mutate(net_worth = net_worth/(10^6))
  
  plot <- ggplot(data = to_plot, aes(x=net_worth, y = tax_amount)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Effective Annual Wealth Tax by Net Worth")) +
    labs(x = paste0("Net Worth (in millions)"), y = "Annual Tax ($)",
         caption = paste0(source_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_nw(10^10, "all")
plot_nw(10^9, "1bil")

# ############################  End  ################################## #