cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

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
library(stats)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Create a function to determine the percentage of the total return that is related to price (not dividends)
# Also use it to find the percentage of months that represent the total return over the period being used
filter_year <- function(date_var){
  
  # Get the first and last year in the data for plot printing
  sp500_ret_pe <- filter(sp500_ret_pe, Date >= date_var)
  
  first_year <- floor(min(sp500_ret_pe$Date))
  last_year  <- floor(max(sp500_ret_pe$Date))
  
  for (i in 1:nrow(sp500_ret_pe)){
    if (i == 1){
      sp500_ret_pe[i, "n_shares"]       <- 1
      sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
      sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
    } else{
      sp500_ret_pe[i, "n_shares"]        <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
      sp500_ret_pe[i, "new_div"]         <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
      sp500_ret_pe[i, "price_plus_div"]  <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
      sp500_ret_pe[i, "tot_ret"]         <- sp500_ret_pe[i, "price_plus_div"]/sp500_ret_pe[1, "price_plus_div"] - 1
      sp500_ret_pe[i, "price_ret"]       <- sp500_ret_pe[i, "real_price"]/sp500_ret_pe[1, "real_price"] - 1
      if (sp500_ret_pe[i, "tot_ret"] < 0){
        sp500_ret_pe[i, "price_ret_pct"] <- 1
      } else if (sp500_ret_pe[i, "price_ret"] < 0){
        sp500_ret_pe[i, "price_ret_pct"] <- 0
      } else{
      sp500_ret_pe[i, "price_ret_pct"]   <- sp500_ret_pe[i, "price_ret"]/(sp500_ret_pe[i, "price_ret"]  + sp500_ret_pe[i, "n_shares"] - 1)
      }
      sp500_ret_pe[i, "div_ret_pct"]     <- 1 - sp500_ret_pe[i, "price_ret_pct"]
    }
  }
  
  to_plot <- select(sp500_ret_pe, Date, div_ret_pct)

  to_plot$year_month <- as.Date(paste0(substring(to_plot$Date,
                                                      1,
                                                      4),
                                            "-",
                                            substring(to_plot$Date,
                                                      6,
                                                      7),
                                            "-01"))
  
  print(paste0("Dividends represent ", 100*round(to_plot[nrow(to_plot), "div_ret_pct"], digits = 3), "% of cumulative returns."))
  
# Set the file_path for the next output
file_path = paste0(exportdir, "10-sp500-returns-dividends/top-monthly-returns-", first_year,".jpeg")

# Create the plot with labels using geom_text_repel
plot <- ggplot(data = to_plot, aes(x = year_month, y = div_ret_pct)) +
  geom_area(fill = "darkgreen", col = "darkgreen") +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(label = percent, limits = c(0, 1)) +
  ggtitle(paste0("Dividends As A Share of Total U.S. Stock Returns\n", first_year, " - ", last_year)) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Share of Total Return")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Assumes the dividend return is 100% when the total price return is negative.")

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

# Run the function for the full period
filter_year(1871.01)


# ############################  End  ################################## #