cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "_jkb/0001_investing_vs_saving_plots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Set program paramaters
n_years_working     <- 40
income              <- 100000
savings_rate        <- 0.1
sample_mean         <- 0.05
sample_sd           <- 0
n_simulations       <- 1

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#969696", "#525252")

# This seed allows us to have reproducible random sampling
set.seed(12345)         

# Create asset and return matrices 
returns_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)
asset_matrix   <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)
savings_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years_working+1)

# Create a for loop for each year you work
for (i in 1:(n_years_working + 1)){
  ret_1yr <- rnorm(n_simulations, sample_mean, sample_sd)
  if (i == 1){
    savings_matrix[, i]  <- income * savings_rate
    asset_matrix[, i]    <- savings_matrix[, i]
    returns_matrix[, i]  <- rep(0, n_simulations)
  } else {
    savings_matrix[, i]  <- income * savings_rate
    returns_matrix[, i]  <- ret_1yr * asset_matrix[, (i-1)]
    asset_matrix[, i]    <- savings_matrix[, i] + returns_matrix[, i] + asset_matrix[, (i-1)]
  }
}

# Convert the matrices to a long data frame for plotting
convert_to_df <- function(matrix, type){
  out <- as.data.frame(matrix) %>%
          gather(key = "year", value = "value", 1:(n_years_working+1))
  out$simulation <- seq(1, n_simulations)
  out$year       <- rep(seq(1, (n_years_working + 1)), each = n_simulations)
  out$type       <- type
  return(out)
}

savings_df <- convert_to_df(savings_matrix, "savings")
returns_df <- convert_to_df(returns_matrix, "returns")

# Bind the two data frames
to_plot <- bind_rows(savings_df, returns_df)

## Create 1st plot
  # Set the file path
  file_path <- paste0(out_path, "/saving-vs-investing.jpeg")
  
  # Create plot 
  plot <- ggplot(data = to_plot, aes(x = year, fill = type, weight = value)) +
              geom_bar(position = "stack") +
              geom_hline(yintercept = 0) +
              geom_text_repel(data = 
                                filter(to_plot, 
                                       year == 1, 
                                       type == "savings"),
                              aes(x = year, 
                                  y = value,
                                  col = type,
                                  label = str_wrap("Savings Matter Early in Life", width = 10),
                                  family = "my_font"),
                              nudge_y = 20000,
                              nudge_x = 2) +
              geom_text_repel(data = 
                                filter(to_plot, 
                                       year == 29, 
                                       type == "returns"),
                              aes(x = year, 
                                  y = value,
                                  col = type,
                                  label = str_wrap("Investments Dominate Later in Life", width = 10),
                                  family = "my_font"),
                              nudge_y = 130000,
                              nudge_x = -2) +
              scale_color_manual(values = my_palette, guide = FALSE) +
              scale_fill_manual(values = my_palette, guide = FALSE) +
              scale_y_continuous(labels = dollar, limits = c(0, 80000), breaks = seq(0, 80000, 10000)) +
              scale_x_continuous(breaks = seq(0, n_years_working, 5)) +
              of_dollars_and_data_theme +
              labs(x = "Years" , y = "Annual Change in Value") +
              ggtitle(paste0("Savings and Investment Returns Have\nVarying Impact Over Time"))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
 
## Create additional dataset to show percentage of total assets attributable to savings vs investment
assets_df  <- convert_to_df(asset_matrix, "total_assets")

assets_df$pct   <-  1 - ((seq(1, n_years_working + 1) * (income * savings_rate)) / assets_df$value)
assets_df$type  <- "investment_pct"


# ############################  End  ################################## #