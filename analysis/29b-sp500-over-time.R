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
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Subset S&P 500 returns
sp500_ret_pe <- filter(sp500_ret_pe, cape != "NA", Date < 2017.01)

first_year <- floor(min(sp500_ret_pe$Date))
last_year <- floor(max(sp500_ret_pe$Date))

for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
}

# Convert the cape to a numeric
sp500_ret_pe$cape <- as.numeric(sp500_ret_pe$cape)

create_period_return <- function(start_year, end_year){

  ret_yr            <- filter(sp500_ret_pe, Date >= start_year, Date < end_year)
  initial_value     <- filter(sp500_ret_pe, Date == start_year) %>%
    select(price_plus_div)
  ret_yr$price      <- (ret_yr$price_plus_div / rep(as.numeric(initial_value), 12*(end_year-start_year))) * 1
  ret_yr$start_date <- start_year
  ret_yr$period     <- seq(1/12, (end_year-start_year), 1/12)
  return(ret_yr)
}

ret_1900 <- create_period_return(1900.01, 2000.01)
ret_2000 <- create_period_return(2000.01, 2017.01)

n_years <- 100

to_plot <- bind_rows(ret_1900, ret_2000)

# Set the file_path for the next output
file_path = paste0(exportdir, "29-e-pluribus-unum/returns_by_century.jpeg")

plot <- ggplot(data = to_plot, aes(x = period, y = price, col = as.factor(start_date))) +
  geom_line(alpha = 0.5) +
  scale_x_continuous(limits = c(1/12, n_years), breaks = seq(5,n_years, 5)) +
  scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000)) +
  scale_color_discrete(guide = FALSE) +
  geom_text_repel(data = filter(to_plot, period > 16.99, start_date == 2000.01),
                  aes(x = period, 
                      y = price,
                      col = as.factor(start_date),
                      label = "2000-2016",
                      family = "my_font"),
                  nudge_y = -.3,
                  nudge_x = 7
  ) +
  geom_text_repel(data = filter(to_plot, period == max(to_plot$period)),
                  aes(x = period, 
                      y = price,
                      col = as.factor(start_date),
                      label = "1900-1999",
                      family = "my_font")
  ) +
  ggtitle(paste0("Will the 21st Century Produce\nSimilar Returns To the 20th Century?")) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Index (Year 1 = $1)")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Annualized real returns include reinvested dividends.") 

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