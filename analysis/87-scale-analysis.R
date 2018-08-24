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
  file_path <- paste0(out_path, "/scale_employees_by_company_", var, ".jpeg")
  
  # Calculate slope of regression line
  fmla <- as.formula(paste0("log(", var, ") ~ log(employees)"))
  model <- lm(fmla, to_plot)
  slope <- model$coefficients[2]
  
  # Strings for source and note
  source_string <- str_wrap("Source:  YCharts (OfDollarsAndData.com)",
                            width = 80)
  note_string <- str_wrap(paste0("Note:  Every doubling in the number of employees increases '", proper_name, "' by ", 100*(round(slope, 2)), "%."),
                          width = 85)
  
  
  plot <- ggplot(to_plot, aes_string(x="employees", y=var)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(~company) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
      of_dollars_and_data_theme + 
      ggtitle(paste0("Number of Employees vs. ", proper_name)) +
      labs(x = "Number of Employees" , y = proper_name,
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  # Plot again, but not by company
  file_path <- paste0(out_path, "/scale_employees_all_", var, ".jpeg")

  note_string <- str_wrap(paste0("Note:  Only shows data for Facebook, Apple, Amazon, Google, Netflix, and Microsoft.  
                                 Every doubling in the number of employees increases '", proper_name, "' by ", 100*(round(slope, 2)), "%."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes_string(x="employees", y=var)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    of_dollars_and_data_theme + 
    ggtitle(paste0("Number of Employees vs. ", proper_name)) +
    labs(x = "Number of Employees" , y = proper_name,
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}
  
plot_log_log("assets", "Assets")
plot_log_log("rev", "Revenue")
plot_log_log("netinc", "Net Income")



# ############################  End  ################################## #