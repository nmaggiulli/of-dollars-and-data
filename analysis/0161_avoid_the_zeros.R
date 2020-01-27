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

folder_name <- "0161_avoid_the_zeros"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

set.seed(12345)

nums <- seq(1, 100)
ret_dynamic_neg <- -0.96
ret_dynamic_pos <- 0.04
ret_static <- 0.01


plot_sim_periods <- function(n_periods){

  n_simulations <- 1000
  sampled <- sample(nums, n_periods*n_simulations, replace = TRUE)
  
  mat <- matrix(nrow = n_simulations*n_periods, ncol = 4)
  
  counter <- 1
  for(j in 1:n_simulations){
    print(j)
    for(i in 1:n_periods){
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
  
  colnames(df) <- c("period", "Dynamic", "Static", "sim")
  
  to_plot <- df %>%
                gather(-period, -sim, key=key, value=value) %>%
                group_by(period, key) %>%
                summarize(value = quantile(value, probs = 0.5)) %>%
                ungroup()
  
  file_path <- paste0(out_path, "/static_v_dynamic_", n_periods, ".jpeg")
  source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
  note_string <-  str_wrap(paste0("Note:  Runs ", formatC(n_simulations, big.mark = ","), " simulations of each strategy for ", formatC(n_periods, big.mark = ","), " periods."), 
                           width = 80)
  
  plot <- ggplot(to_plot, aes(x=period, y=value, col = key)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c("red", "blue")) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Dynamic vs. Static Strategy\nMedian Outcome by Period")) +
    labs(x="Period", y="Median Portfolio Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_sim_periods(50)
plot_sim_periods(100)
plot_sim_periods(1000)

# ############################  End  ################################## #