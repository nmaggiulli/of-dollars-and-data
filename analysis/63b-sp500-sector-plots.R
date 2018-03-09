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

df <- readRDS(paste0(localdir, "63_sp500_sector_returns.Rds")) %>%
  filter(year(date) > 1989, year(date) < 2018) %>%
  mutate(year = year(date),
         ret = ret + 1,
         sector = case_when(sector == "S&P 500" ~ "*S&P 500*",
                            TRUE ~ sector)) %>%
  group_by(year, sector) %>%
  summarize(value = prod(ret) - 1) %>%
  left_join(cpi) %>%
  mutate(value = value - rate_cpi,
         overall = case_when (sector == "*S&P 500*" ~ 1,
                    TRUE ~ 0)) %>%
  ungroup() %>%
  select(year, value, sector, overall)

# Create a tactical sector strategy that changes each year
switch_strategy <- df %>%
                    group_by(year) %>%
                    arrange(year, -value) %>%
                    filter(row_number()==1) %>%
                    ungroup() %>%
                    mutate(year = year + 1) %>%
                    filter(year < 2018) %>%
                    select(year, sector) %>%
                    inner_join(df) %>%
                    mutate(overall = 1,
                           sector = "*Switch Strategy*")

first_year <- min(df$year)
last_year  <- max(df$year)

  # Create data and plot line charts
  to_plot <- df %>%
              bind_rows(switch_strategy) %>%
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
  file_path <- paste0(exportdir, "63-sp500-sector-returns/sector_line_charts.jpeg")

  # Set note and source string
  source_string <- str_wrap("Source: Morningstar, FRED (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Returns are adjusted for inflation using FRED CPI data."), width = 85)

  plot <- ggplot(data = to_plot, aes(x=year, y=index, col = factor(sector), 
                                     linetype = as.factor(overall),
                                     size = as.factor(overall))) +
            geom_line() +
            scale_color_discrete(name = "Sector") + 
            scale_linetype_manual(guide = FALSE, values = c("dashed", "solid")) + 
            scale_size_discrete(guide = FALSE, range = c(0.5, 1.5)) +
            of_dollars_and_data_theme +
            ggtitle(paste0("Total Growth By Sector\n", first_year, "-", last_year)) +
            labs(x = "Year", y = "Index (Start = 100)",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

#Print some stats for the post

  # Summarize all sectors 1991-2017
  df %>%
    filter(year > 1990) %>%
    group_by(sector) %>%
    summarize(avg_ret = mean(value),
              stdev = sd(value))
  
  switch_strategy %>%
    filter(year > 1990) %>%
    group_by(sector) %>%
    summarize(avg_ret = mean(value),
              stdev = sd(value))


# ############################  End  ################################## #