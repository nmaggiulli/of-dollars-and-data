cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(ggjoy)
library(dplyr)

########################## Start Program Here ######################### #

# Load in BV returns and remove the commodities from the returns
full_bv_returns <- readRDS(paste0(localdir, "0006_bv_returns.Rds")) %>%
                    select(-Commodities, -Gold, -`U.S. Home Price`)

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year, "%d/%m/%y")

# Find the min and max year and then remove the year variable
min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

full_bv_returns <- select(full_bv_returns, -year)

# Find the number of assets
n_assets <- length(colnames(full_bv_returns))

# Create many different strategies for comparison
# Create an equal weight strategy

full_bv_returns$`Equal Wt` <- apply(full_bv_returns*1/n_assets, 1, sum)

full_bv_returns$`U.S. 60-40` <- full_bv_returns$`S&P 500` * 0.6 + full_bv_returns$`Treasury 10yr` * 0.4
full_bv_returns$`U.S. 80-20` <- full_bv_returns$`S&P 500` * 0.8 + full_bv_returns$`Treasury 10yr` * 0.2
full_bv_returns$`All Stocks` <- full_bv_returns$`S&P 500` * (1/2) + 
                                full_bv_returns$`Int. Stocks` * (1/2) 


to_plot <- select(full_bv_returns, `S&P 500`, `All Stocks`, 
                  `Equal Wt`, `U.S. 60-40`,`U.S. 80-20`) %>%
            gather(key = key, value = value) %>%
            group_by(key) %>%
            summarise(value = mean(value))

order_vec <- to_plot[order(to_plot$value), "key"] %>% collect %>% .[["key"]]
to_plot$key <- factor(to_plot$key, levels = order_vec)

# Set the file_path for the next output
file_path = paste0(exportdir, "0048_compare_strats/compare-return-strats.jpeg")

plot <- ggplot(data = to_plot, aes(x = key, y = value, fill = key)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(guide = FALSE) +
  ggtitle(paste0("There Are Many Ways to Win in Investing")) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  labs(x = "Strategy" , y = "Average Annualized Real Return")

# Add a source and note string for the plots
source_string <- paste0("Source: BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
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

# ############################  End  ################################## #

  
