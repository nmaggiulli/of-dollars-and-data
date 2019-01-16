cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(tidyverse)

folder_name <- "0108_asset_rank_tiles"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Get a list of custom colors from Color Brewer
full_colors <- c("#ffffbf", 
                 "#fee08b", 
                 "#fdae61",
                 "cyan",
                 "#d53e4f",
                 "#66c2a5",
                 "#3288bd",
                 "#abdda4",
                 "#f46d43",
                 "#e6f598"
)

# Read in data
rets <- read_excel(paste0(importdir, "0108_jpm_asset_returns/jpm_asset_returns_2004_2018.xlsx")) %>%
  arrange(year, ret)

n_assets <- length(unique(rets$asset))

first_year <- min(rets$year)
last_year <- max(rets$year)

year_tiles <- data.frame(year = seq(first_year, last_year)) %>%
  mutate(rank = n_assets + 1,
         text = as.character(year))

to_plot <- cbind(rets, rank=seq(1, n_assets)) %>%
  mutate(text = paste0(asset, "\n", round(ret, 3), "%")) %>%
  bind_rows(year_tiles)

# Create function for coloring
plot_ranks <- function(asset_num){
  
  if(asset_num < 10){
    asset_num_string <- paste0("0", asset_num)
  } else{
    asset_num_string <- asset_num
  }
  
  # Set plot colors when plotting an individual asset
  if(asset_num >= 1 & asset_num <= n_assets){
    plot_colors <- rep("gray", n_assets)
    plot_colors[asset_num] <- full_colors[asset_num]
    
    file_path <- paste0(out_path, "/asset_ranks_", asset_num_string, ".jpeg")
  } else{
    plot_colors <- full_colors
    file_path <- paste0(out_path, "/all_colored.jpeg")
  }
  
  source_string <- paste0("Source: JPMorgan Guide to the Markets 2018")
  
  plot <- ggplot(to_plot, aes(x=year, y=rank, fill=asset)) +
    geom_tile() +
    geom_text(data=to_plot, aes(x=year, y=rank), label = to_plot$text, family = "my_font", size = ifelse(is.na(to_plot$asset), 2.3, 1.4)) +
    scale_fill_manual(guide = FALSE, values = plot_colors) +
    of_dollars_and_data_theme +
    ggtitle("Asset Class Returns") +
    theme(axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    labs(x="", y="", caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

for(i in 1:n_assets){
  plot_ranks(i)
}

plot_ranks(n_assets + 1)

create_gif(out_path, "asset_ranks_*.jpeg", 120, 0, "all_asset_ranks.gif")

# ############################  End  ################################## #