cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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
library(dplyr)

########################## Start Program Here ######################### #

# Load in Damodaran SP500 and Bond data
hist_bond_stock <- readRDS(paste0(localdir, "21-historical-returns-sp500-bond-damodaran.Rds"))

# Load in the FRED CPI data
cpi <- readRDS(paste0(localdir, "21-FRED-cpi.Rds"))

# Subset historical bond and stock returns and adjust for CPI using FRED data
hist_bond_stock <-hist_bond_stock %>%
                  left_join(cpi, by = c("Date" = "year")) %>%
                  mutate(ret_sp500 = ret_sp500 - rate_cpi,
                         ret_10yr_bond = ret_10yr_bond - rate_cpi,
                         decade = Date %/% 10 * 10) %>%
                  select(Date, decade, ret_10yr_bond, ret_sp500)

# Get the min and max year
min_year <- min(hist_bond_stock$Date)
max_year <- max(hist_bond_stock$Date)

# Get the average return for plotting
avg_ret <- mean(hist_bond_stock$ret_10yr_bond)

############################### First Returns Plot ###############################  
# Set the file_path for the output
file_path = paste0(exportdir, "37-bond-returns/bond-returns.jpeg")

to_plot <- hist_bond_stock

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = Date, y = ret_10yr_bond)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle(paste0("U.S. 10 Year Bonds Averaged ", round(avg_ret,2)*100, "% Real Returns\n", min_year, " - ", max_year)) +
  scale_y_continuous(labels = percent) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Annual Real Return (%)")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Adjusts for inflation using FRED CPI data.") 

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

############################### Second Returns Plot, by Decade ###############################  
# Set the file_path for the output
file_path = paste0(exportdir, "37-bond-returns/bond-stock-by-decade.jpeg")

to_plot <- hist_bond_stock %>%
            select(decade, ret_10yr_bond) %>%
            group_by(decade) %>%
              summarize(count = n(),
                        `U.S. 10-Year Bond` = prod(1 + ret_10yr_bond)^(1/count) - 1) %>%
              select(-count) %>%
              gather(key=key, value=value, -decade)
              

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = decade, y = value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  ggtitle(paste0("Bonds Have Had Multiple Decades\nwith Negative Annualized Real Returns")) +
  scale_fill_discrete(guide = FALSE) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(min(to_plot$decade), max(to_plot$decade), 10)) +
  of_dollars_and_data_theme +
  labs(x = "Decade" , y = "Annualized Real Return (%)")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Adjusts for inflation using FRED CPI data.") 

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

  
