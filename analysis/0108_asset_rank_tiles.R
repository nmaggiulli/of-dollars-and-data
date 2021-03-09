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
full_colors_10 <- c("#ffffbf", 
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

full_colors_22 <- c(
  "#6894AD",
  "#fffdd0",  
  "#7D868C",  
  "#03bf5b",  
  "#50AB70",  
  "#91C84C",  
  "#0078B6",  
  "#87C8DC",  
  "#AEA38C",  
  "pink", 
  "#46812B", 
  "#75A5B1", 
  "#00A0A5", 
  "#9FB6BC", 
  "#6D702F", 
  "#6BC7B9", 
  "#AFAA2F", 
  "#6C5F60", 
  "#566870", 
  "#988085",
  "#D8D2D4", 
  "#D2E288"
)

create_tiles <- function(file_in, color_list, file_out, size_small, size_large, source_string){
  
  rets <- read_excel(paste0(importdir, "0108_asset_ranks/", file_in)) %>%
    filter(year>= 2009) %>%
    arrange(year, ret)
  
  n_assets <- length(unique(rets$asset))
  
  first_year <- min(rets$year)
  last_year  <- max(rets$year)
  n_years    <- last_year - first_year + 1
  
  avg <- rets %>%
    group_by(asset) %>%
    summarise(ret = round((prod((ret/100 + 1))^(1/n_years) - 1) * 100, 1)) %>%
    ungroup() %>%
    mutate(year = "Ann.") %>%
    arrange(ret) %>%
    cbind(rank = seq(1, n_assets))
  
  first_year <- as.character(min(rets$year))
  last_year <- as.character(max(rets$year))
  
  year_tiles <- data.frame(year = c(seq(first_year, last_year), "Ann.")) %>%
    mutate(rank = n_assets + 1,
           text = as.character(year))
  
  to_plot <- cbind(rets, rank=seq(1, n_assets)) %>%
    mutate(year = as.character(year)) %>%
    bind_rows(avg) %>%
    mutate(text = paste0(asset, "\n", round(ret, 3), "%")) %>%
    bind_rows(year_tiles)
  
  for(i in 1:(n_assets+1)){
    if(i < 10){
      asset_num_string <- paste0("0", i)
    } else{
      asset_num_string <- i
    }
    
    # Set plot colors when plotting an individual asset
    if(i >= 1 & i <= n_assets){
      plot_colors <- rep("gray", n_assets)
      plot_colors[i] <- color_list[i]
      
      file_path <- paste0(out_path, "/", file_out, "_ranks_", asset_num_string, ".jpeg")
    } else{
      plot_colors <- color_list
      file_path <- paste0(out_path, "/all_colored_", file_out, ".jpeg")
    }
    
    plot <- ggplot(to_plot, aes(x=year, y=rank, fill=asset)) +
      geom_tile() +
      geom_text(data=to_plot, aes(x=year, y=rank), 
                label = to_plot$text, 
                size = ifelse(is.na(to_plot$asset), size_large, size_small)) +
      scale_fill_manual(guide = FALSE, values = plot_colors) +
      of_dollars_and_data_theme +
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) +
      ggtitle("Annual Returns & Ranks") +
      labs(x="", y="",
           caption = paste0(source_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
  create_gif(out_path, paste0(file_out, "_ranks_*.jpeg"), 120, 0, paste0("_gif_", file_out, "_ranks.gif"))
}

create_tiles("jpm_asset_returns_2004_2018.xlsx", full_colors_10, "assets", 1.6, 2.5, "Source: J.P.Morgan Guide to the Markets 1Q 2019 (OfDollarsAndData.com)")
create_tiles("country_returns_1998_2018.xlsx", full_colors_22, "countries", 1.1, 2.0, "Source: DFA (OfDollarsAndData.com)")

# ############################  End  ################################## #