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

folder_name <- "88-viatical-chart"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Create data based on Bhattacharya paper "Price Regulation in Secondary Insurance Markets"
to_plot <- data.frame(price = seq(0, 1, 0.1),
                 life_expectancy = c(20, 15, 12, 8, 6, 4.5, 3, 2, 1, 0.5, 0))

# Plot 
file_path <- paste0(out_path, "/price_life_expectancy.jpeg")

source_string <- str_wrap(paste0("Source:  'Price Regulation in Secondary Insurance Markets', Bhattacharya, J. et al. (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note:  Blue line is the actuarially fair price and the black dashed line is the minimum regulated price."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=life_expectancy, y=price)) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(label = percent, breaks = seq(0, 1, 0.1)) +
  of_dollars_and_data_theme + 
  ggtitle(paste0("As Life Expectancy Increases\nViatical Prices Will Decrease")) +
  labs(x = "Life Expectancy " , y = "Price (% of Face Value)",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #