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
library(gganimate)
library(tidylog)
library(tidyverse)

folder_name <- "_jkb/0019_bonus_avoid_the_zeros"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

set.seed(12345)

nums <- seq(1, 100)
ret_dynamic_neg <- -0.96
ret_dynamic_pos <- 0.04
ret_static <- 0.01

my_palette <- c("#969696", "#000000")

plot_sim_days <- function(n_days, median){

  n_simulations <- 1000
  sampled <- sample(nums, n_days*n_simulations, replace = TRUE)
  
  mat <- matrix(nrow = n_simulations*n_days, ncol = 4)
  
  counter <- 1
  for(j in 1:n_simulations){
    print(j)
    for(i in 1:n_days){
      mat[counter, 1] <- i
      ret_dynamic <- ifelse(sampled[counter] == 50, ret_dynamic_neg, ret_dynamic_pos)
      
      if(i == 1){
        mat[counter, 2] <- 1
        mat[counter, 3] <- 1
      } else{
        mat[counter, 2] <- mat[(counter-1), 2] * (1 + ret_dynamic)
        mat[counter, 3] <- mat[(counter-1), 3] * (1 + ret_static)
      }
    mat[counter, 4] <- j
    counter <- counter + 1
    }
  }
  
  df <- mat %>%
              as.data.frame()
  
  colnames(df) <- c("period", "Asset A", "Asset B", "sim")
  
  if(median == 1){
    to_plot <- df %>%
                  gather(-period, -sim, key=key, value=value) %>%
                  group_by(period, key) %>%
                  summarise(value = quantile(value, probs = 0.5)) %>%
                  ungroup()
    title_string <- "Median"
  } else{
    to_plot <- df %>%
      gather(-period, -sim, key=key, value=value) %>%
      group_by(period, key) %>%
      summarise(value = mean(value)) %>%
      ungroup()
    title_string <- "Average"
  }
  
  file_path <- paste0(out_path, "/asset_simulation_", n_days, "_days_", title_string, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=period, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = my_palette) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0(title_string, " Outcome by Day")) +
    labs(x="Day", y=paste0(title_string, " Portfolio Value"))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Plot medians
plot_sim_days(50, 1)
plot_sim_days(100, 1)
plot_sim_days(1000, 1)

# Plot averages
plot_sim_days(1000, 0)

# ############################  End  ################################## #