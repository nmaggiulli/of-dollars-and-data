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
library(tidyverse)

folder_name <- "_ctwl/0003_wealth_level_steps"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

w_levels <- c(10^4, 10^5, 10^6, 10^7, 10^8)
df <- data.frame(wealth = c(0, w_levels),
                 level = seq(1, length(w_levels) + 1))

to_plot <- df

file_path <- paste0(out_path, "/wealth_steps_log_scale.jpeg")

plot <- ggplot(data = to_plot, aes(x=wealth, y = level)) +
  geom_step() +
  scale_y_continuous(label = comma, breaks = seq(1, length(w_levels) +1)) +
  scale_x_continuous(label = dollar_format(), limits= c(NA, 10^8 *1.1), breaks = c(0, w_levels), trans = "log10") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Wealth Level Based on Net Worth")) +
  labs(x = paste0("Net Worth"), y = paste0("Wealth Level"))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #