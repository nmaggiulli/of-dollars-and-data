cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Load in Damodaran SP500 and Bond data
hist_bond_stock <- readRDS(paste0(localdir, "21-historical-returns-sp500-bond-damodaran.Rds"))

# Get start year
start_year <- min(hist_bond_stock$Date)

# Load in the FRED CPI data
cpi <- readRDS(paste0(localdir, "21-FRED-cpi.Rds"))

# Number of years to calculate returns over
n_years <- 30

# Adjust historical bond and stock returns for CPI using FRED data
hist_bond_stock <- hist_bond_stock %>%
                      left_join(cpi, by = c("Date" = "year")) %>%
                      mutate(ret_sp500 = ret_sp500 - rate_cpi,
                             ret_10yr_bond = ret_10yr_bond - rate_cpi) %>%
                      select(Date, ret_sp500, ret_10yr_bond)

  
# Create a date sequence for each decade
date_seq <- seq(1928, max(hist_bond_stock$Date) - n_years, 1)

# Calculate annual index
for (r in 1:nrow(hist_bond_stock)){
  if (r == 1){
    hist_bond_stock[r, "index_s"] <- 100
    hist_bond_stock[r, "index_b"] <- 100
  } else {
    hist_bond_stock[r, "index_s"] <- hist_bond_stock[(r - 1), "index_s"] * (1 + hist_bond_stock[(r - 1), "ret_sp500"])
    hist_bond_stock[r, "index_b"] <- hist_bond_stock[(r - 1), "index_b"] * (1 + hist_bond_stock[(r - 1), "ret_10yr_bond"])
  }
}
  
# Loop through the dates to get each return period for a specified number of years and stack it
for (i in 1:length(date_seq)){
  ret_yr              <- filter(hist_bond_stock, Date >= date_seq[i], Date <= date_seq[i] + n_years) %>%
                          mutate(`S&P 500` = (index_s / lag(index_s, n = n_years))^(1/n_years) - 1,
                                `U.S. 10 Year Bond` = (index_b / lag(index_b, n = n_years))^(1/n_years) - 1) %>%
                          filter(Date == date_seq[i] + n_years) %>%
                          select(Date, `S&P 500`, `U.S. 10 Year Bond`)
  if (i == 1){
    to_plot <- ret_yr
  } else{
    to_plot <- rbind(to_plot, ret_yr)
  }
}

# Convert the wide data to long data
to_plot <- gather(to_plot, key= "asset", value="ret_30yr", -Date)
  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "31-against-the-gods/sp500-vs-us-bond-30-yr.jpeg")
  
  # Create our plot of rolling 30 year returns
  plot <- ggplot(data = to_plot, aes(x = Date, y = ret_30yr, col = as.factor(asset))) +
            geom_line() +
            geom_text_repel(data = filter(to_plot, Date ==  min(to_plot$Date), asset == "S&P 500"),
                            aes(x = Date, 
                                y = ret_30yr,
                                col = as.factor(asset),
                                label = asset,
                                family = "my_font"),
                            nudge_y  = -0.01,
                            nudge_x  = 10,
                            segment.color = 'transparent'
            ) +
            geom_text_repel(data = filter(to_plot, Date == min(to_plot$Date), asset == "U.S. 10 Year Bond"),
                            aes(x = Date, 
                                y = ret_30yr,
                                col = as.factor(asset),
                                label = asset,
                                family = "my_font"),
                            nudge_y = 0.005, 
                            nudge_x  = 10,
                            segment.color = 'transparent'
            ) +
            scale_y_continuous(label = percent) +
            scale_x_continuous(breaks = seq(1960, 2010, 10)) +
            scale_color_discrete(guide = FALSE) +
            ggtitle(paste0("The S&P 500 Has Outperformed U.S. Bonds\nFor Every ",  n_years, "-Year Period Since ", start_year)) +
            of_dollars_and_data_theme +
            labs(x = paste0(n_years,"-Year Period Ending") , y = paste0(n_years, "-Year Annualized Real Return"))


# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Annualized real returns are adjusted for inflation using FRED CPI data.") 

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