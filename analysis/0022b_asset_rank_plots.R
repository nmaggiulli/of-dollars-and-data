cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)

out_path <- paste0(exportdir, "0022_bv_network_and_rank_plots")

########################## Start Program Here ######################### #

# Read in BV data
bv <- readRDS(paste0(localdir, "0006_bv_returns.Rds")) %>%
        mutate(`10 Year Treasury Bonds` = `Treasury 10yr`,
               `3 Month T-Bills` = `Tbill 3m`,
               year = as.Date(year, format = "%m/%d/%y")) %>%
          select(-`Treasury 10yr`, -`Tbill 3m`)

# Create min and max years for plots
min_year <- min(year(bv$year))
max_year <- max(year(bv$year))

## Reproduce colors from ggplot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Convert wide to long data
bv_long <- bv %>%
            gather(-year, key = "key", value = "return") %>%
            arrange(year, -return)

# Create a varlist to loop over
varlist <- unique(bv_long$key)
  
for (k in 1:length(varlist)){
# Add ranking and color
  df <- cbind(bv_long, rank = seq(1, 9, 1)) %>%
            mutate(c = ifelse(key == varlist[k], "1", "0"))
  
  # Set the file_path for the next output
  file_path = paste0(out_path ,"/rank-", varlist[k] ,"-by-year.jpeg")
  
  plot <-ggplot(mapping = aes(year, y = rank, group = key, color = c)) +
          geom_line(size = 1.7, alpha = 0.25, data = df) +
          geom_line(size = 2.5, data = df %>% filter(key  == varlist[k])) +
          geom_point(size = 4, alpha = 0.25, data = df) +
          geom_point(size = 1.75, color = "white", data = df) +
          scale_color_manual(values = c("grey", gg_color_hue(length(varlist))[k]), guide = FALSE) +
          ggtitle(paste0("Real Return Rank\n", varlist[k])) +
          xlab("Year") +
          ylab("Rank") +
          of_dollars_and_data_theme +
          scale_y_continuous(trans =  "reverse", breaks = seq(1, length(varlist), 1)) +
          scale_x_date(breaks=seq(as.Date("1980-01-02"),as.Date("2015-01-02"),"5 years"),
                           labels=date_format("%Y"))
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

create_gif(out_path, "rank-*.jpeg", 120, out_name = "gif_all_asset_ranks.gif")

# ############################  End  ################################## #