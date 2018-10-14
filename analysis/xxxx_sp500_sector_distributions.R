cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
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

cpi <- readRDS(paste0(localdir, "0021_FRED_cpi.Rds"))

df <- readRDS(paste0(localdir, "0063_sp500_sector_returns.Rds")) %>%
        filter(year(date) > 1989, year(date) < 2018) %>%
        mutate(year = year(date),
               ret = ret + 1,
               sector = case_when(sector == "S&P 500" ~ "*S&P 500*",
                                  TRUE ~ sector)) %>%
        group_by(year, sector) %>%
        summarize(value = prod(ret) -1) %>%
        left_join(cpi) %>%
        mutate(value = value - rate_cpi) %>%
        ungroup() %>%
        select(year, value, sector)

first_year <- min(df$year)
last_year  <- max(df$year)

plot_dist_years <- function(n_years){
  
  to_plot <- df %>%
              arrange(sector, year)
  
  # Calculate the annualized returns
  for (i in n_years:nrow(to_plot)){
    if (to_plot[(i-n_years+1), "sector"] == to_plot[i, "sector"]){
      start_index <- i - n_years + 1
      to_plot[i, "ret"] <- prod(1 + to_plot[(start_index:i), "value"])^(1/n_years) - 1
    } else {
      to_plot[i, "ret"] <- NA
    }
  }
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "xxxx_sp500_sector_returns/return-dist-", n_years, "-years.jpeg")
  
  # Set note and source string
  source_string <- str_wrap("Source: Morningstar, FRED (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note: Returns are adjusted for inflation using FRED CPI data."
  ),
  width = 85)

  plot <- ggplot(data = to_plot, aes(x=ret, y=factor(sector), fill = factor(sector))) +
            geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
            scale_fill_discrete(guide = FALSE) +
            scale_x_continuous(label = percent) +
            of_dollars_and_data_theme +
            ggtitle(paste0(n_years, "-Year Annualized Return by Sector\n", first_year, "-", last_year)) +
            labs(x = "Annual Real Return", y = "Sector",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

n_years_list <- seq(1, 10, 1)

for (j in 1:length(n_years_list)){
    plot_dist_years(n_years_list[j])
}


# ############################  End  ################################## #