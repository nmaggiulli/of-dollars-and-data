cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(ggrepel)
library(stringr)

folder_name <- "_mttw/0004_inflation_cuts_half"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#000000")

to_plot <- data.frame(rate = seq(0.02, 0.05, 0.01),
                      years = c(35, 23, 17, 14)) %>%
            mutate(label = paste0(years, " 年"),
                   label_bottom = paste0(100*rate, "%"))


# Set the file path
file_path <- paste0(out_path, "/inflation_cuts_half.jpeg")

# Create plot
plot <- ggplot(data = to_plot, aes(x = rate, y = years)) +
          geom_bar(stat = "identity", fill = bw_colors) +
          geom_text(data = to_plot, aes(x= rate, y = years, label = label),
                    col = "black",
                    size = 3.5,
                    family = "my_font",
                    vjust = -0.5) +
          geom_text(data = to_plot, aes(x= rate, y = 0, label = label_bottom),
                    col = "black",
                    size = 3.5,
                    family = "my_font",
                    vjust = 1.5) +
          scale_x_continuous(label = percent_format(accuracy = 1)) +
          of_dollars_and_data_theme +
          theme(axis.line.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.x = element_blank(),
                axis.text.x = element_blank()) +
          labs(x = "通膨率" , y = "") +
          ggtitle(paste0("通膨使你的購買力減半所需的時間"))
  
# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  

# ############################  End  ################################## #