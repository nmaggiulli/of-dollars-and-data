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

folder_name <- "_twl/0003_wealth_level_steps"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

anchor <- 0.9999999
nw_levels <- c(10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9)
w_levels <- c(anchor, seq(1, 6))
                
df <- data.frame(level = w_levels, nw = nw_levels)

to_plot <- df

file_path <- paste0(out_path, "/wealth_steps_log_scale.jpeg")

plot <- ggplot(data = to_plot, aes(x = level, y = nw)) +
  geom_step() +
  scale_x_continuous(label = comma, breaks = w_levels[2:length(w_levels)], limits = c(anchor, 6)) +
  scale_y_continuous(label = dollar_format(), limits= c(10^3, 10^9 *1.1), breaks = c(nw_levels[2:(length(nw_levels) - 1)]), trans = "log10") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Wealth Level Based on Net Worth")) +
  labs(x = paste0("Wealth Level"), y = paste0("Net Worth"))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #