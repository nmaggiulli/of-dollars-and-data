cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(tidyr)
library(ggjoy)
library(lubridate)
library(dplyr)

########################## Start Program Here ######################### #

cpi <- readRDS(paste0(localdir, "21-FRED-cpi.Rds"))

df <- readRDS(paste0(localdir, "xx_sp500_sector_returns.Rds")) %>%
  filter(year(date) > 1989, year(date) < 2018) %>%
  mutate(year = year(date),
         ret = ret + 1,
         sector = case_when(sector == "S&P 500" ~ "*S&P 500*",
                            TRUE ~ sector)) %>%
  group_by(year, sector) %>%
  summarize(value = prod(ret) - 1) %>%
  left_join(cpi) %>%
  mutate(value = value - rate_cpi) %>%
  ungroup() %>%
  select(year, value, sector)

first_year <- min(df$year)
last_year  <- max(df$year)

  # Create data and plot line charts
  to_plot <- df %>%
              arrange(sector, year)
  
  for (i in 1:nrow(to_plot)){
    if (i == 1){
      to_plot[i, "index"] <- 100
    } else if (to_plot[i, "sector"] != to_plot[(i-1), "sector"]){
      to_plot[i, "index"] <- 100
    } else {
      to_plot[i, "index"] <- to_plot[(i-1), "index"] * (1 + to_plot[(i-1), "value"])
    }
  }
 
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "xx-sp500-sector-returns/sector_line_charts.jpeg")

  # Set note and source string
  source_string <- str_wrap("Source: Morningstar, FRED (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Returns are adjusted for inflation using FRED CPI data."), width = 85)

  plot <- ggplot(data = to_plot, aes(x=year, y=index, col = factor(sector))) +
            geom_line() +
            of_dollars_and_data_theme +
            ggtitle(paste0("Total Returns By Sector\n", first_year, "-", last_year)) +
            labs(x = "Year", y = "Index (Start = 100)",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #