cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(ggrepel)
library(tidyverse)

folder_name <- "87-scale-analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- readRDS(paste0(localdir, "87-employee-rev-ycharts.Rds"))

vars <- c("assets", "netinc", "revenue")

plot_log_log <- function(var, proper_name){
  
  to_plot <- df %>%
                select_("employees", "company", "year", var) %>%
                filter(!is.na(employees))
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/scale_employees_", var, ".jpeg")
  
  # Strings for source and note
  source_string <- str_wrap("Source:  YCharts (OfDollarsAndData.com)",
                            width = 80)
  
  plot <- ggplot(to_plot, aes_string(x="employees", y=var)) +
      geom_point() +
      geom_smooth(method = "lm") +
      scale_y_log10() +
      scale_x_log10() +
      of_dollars_and_data_theme + 
      ggtitle("Time To Send A Message\nFrom London to New York") +
      labs(x = "Number of Employees" , y = proper_name,
         caption = paste0("\n", source_string))
  
  

}

plot_log_log("assets", "Total Assets")



# ############################  End  ################################## #