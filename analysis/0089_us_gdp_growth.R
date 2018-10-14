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

folder_name <- "0089_usd_gdp_growth"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, folder_name, "/usgdp_1790_2017_measuringworth.csv")) 

colnames(raw) <- c("year", "real_gdp", "pop", "real_gdp_per_capita")

to_plot <- raw %>%
        mutate(real_gdp = as.numeric(str_remove_all(real_gdp, ",")) * 10^6,
               pop = as.numeric(str_remove_all(pop, ",")) * 10^3,
               real_gdp_per_capita = as.numeric(str_remove_all(real_gdp_per_capita, ",")))

specific_points <- filter(to_plot, year %in% c(1800, 1865, 1926, 2017))

file_path <- paste0(out_path, "/us_real_gdp_log_cite.jpeg")

source_string <- str_wrap(paste0("Source:  Louis Johnston and Samuel H. Williamson, 'What Was the U.S. GDP Then?' MeasuringWorth, 2018,
                                 http://www.measuringworth.org/usgdp/ (OfDollarsAndData.com)"),
                          width = 85)

note_string <- str_wrap(paste0("Note:  All amounts are in 2012 dollars."))

plot <- ggplot(to_plot, aes(x=year, y=real_gdp)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1800, 2000, 25)) +
  geom_point(data=specific_points, aes(x=year, y=real_gdp), col = "red") +
  geom_text_repel(data=specific_points, aes(x=year, y=real_gdp), 
                  label = paste0("$", formatC(round(specific_points$real_gdp/10^9, 2), 
                                              format='d', 
                                              big.mark = ","), 
                                 " billion ","(", specific_points$year, ")"), 
                  size = 3.5, max.iter = 3000) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  of_dollars_and_data_theme + 
  ggtitle(paste0("U.S. Real GDP Has Grown Over\n2,200x Since the Year 1800")) +
  labs(x = "Year" , y = "Real GDP (Log Scale)",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# Plot per capita
file_path <- paste0(out_path, "/us_real_gdp_per_capita_log_cite.jpeg")

plot <- ggplot(to_plot, aes(x=year, y=real_gdp_per_capita)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1800, 2000, 25)) +
  geom_point(data=specific_points, aes(x=year, y=real_gdp_per_capita), col = "red") +
  geom_text_repel(data=specific_points, aes(x=year, y=real_gdp_per_capita), 
                  label = paste0("$", formatC(round(specific_points$real_gdp_per_capita, 1), 
                                              format='d', 
                                              big.mark = ","), 
                                 " ","(", specific_points$year, ")"), 
                  size = 3.5, nudge_y = 0.05) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  of_dollars_and_data_theme + 
  ggtitle(paste0("U.S. Real GDP Per Capita Has Grown\nConsiderably As Well")) +
  labs(x = "Year" , y = "Real GDP Per Capita (Log Scale)",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #