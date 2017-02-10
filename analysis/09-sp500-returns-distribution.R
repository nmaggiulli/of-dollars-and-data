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
library(lubridate)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller

sp500_ret_pe   <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

sp500_ret_pe$year_month <- as.Date(paste0(substring(sp500_ret_pe$Date,
                                                    1,
                                                    4),
                                                    "-",
                                          substring(sp500_ret_pe$Date,
                                                    6,
                                                    7),
                                          "-01"))

# Get the first and last year in the data for plot printing
first_year <- floor(min(sp500_ret_pe$Date))
last_year  <- floor(max(sp500_ret_pe$Date))

# Create a function to determine the percentage of the total return that is related to price (not dividends)
filter_year <- function(x){
  
  sp500_ret_pe <- filter(sp500_ret_pe, Date >= x)

  sp500_total_ret <- (sp500_ret_pe[nrow(sp500_ret_pe), "price_plus_div"]/sp500_ret_pe[1, "price_plus_div"]) - 1
  sp500_price_ret <- (sp500_ret_pe[nrow(sp500_ret_pe), "real_price"]/sp500_ret_pe[1, "real_price"]) - 1
  sp500_div_ret <- sp500_ret_pe[nrow(sp500_ret_pe), "n_shares"]   
  print(paste0("Price return starting at ", x, " accounted for: ", round(100 * (sp500_price_ret / (sp500_price_ret + sp500_div_ret))), "% of the total return."))
  
  sp500_ret_pe <- arrange(sp500_ret_pe, desc(ret_1_month))
  
  for (i in 1:nrow(sp500_ret_pe)){
    if (i == 1){
      sp500_ret_pe[i, "ret_cumulative"] <- (1 + sp500_ret_pe[i, "ret_1_month"])
      sp500_ret_pe[i, "pct_of_months"] <- i/nrow(sp500_ret_pe)
      sp500_ret_pe[i, "before_total"] <- 1
    } else {
      sp500_ret_pe[i, "before_total"] <- 1
      sp500_ret_pe[i, "ret_cumulative"] <- (1 + sp500_ret_pe[i, "ret_1_month"]) * sp500_ret_pe[(i-1), "ret_cumulative"]
      sp500_ret_pe[i, "pct_of_months"] <- i/nrow(sp500_ret_pe)
      if(sp500_ret_pe[i, "ret_cumulative"] > sp500_total_ret){
        break
      }
    }
  }
  filtered <- filter(sp500_ret_pe, before_total == 1, is.na(lead(before_total)))
  print(paste0("It took ", round(100 * filtered$pct_of_months), "% of months to equal the total return"))
  if (x == 1871.01){
    assign("to_plot", sp500_ret_pe, envir = .GlobalEnv)
  }
  return(round(100 * filtered$pct_of_months))
}

# Loop over the years for exploratory purposes
for (j in seq(1871.01, 2011.01, 10)){
  if (j == 1871.01){
    pct_of_total_ret <- filter_year(j)
  } else{
    test <- filter_year(j)
  }
}

# Alter before_total flag to reflect 2 for all times where it is NA
to_plot[, "before_total"] <- apply(to_plot[, "before_total"], 1, function(x){ifelse(!is.na(x), x, 0)})

# Alter before_total flag to equal -1 when the 1-month return is below zero
to_plot[which(to_plot$ret_1_month < 0), "before_total"] <- -1

ymax <- ceiling(max(to_plot[, "ret_1_month"]) * 10)/ 10
ymin <- floor(min(to_plot[, "ret_1_month"]) * 10) / 10

# Set the file_path for the next output
file_path = paste0(exportdir, "09-sp500-returns-pe/top-monthly-returns.jpeg")

plot <- ggplot(data = to_plot, aes(x = reorder(Date, -ret_1_month), y = ret_1_month, col = as.factor(before_total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label = percent, limits = c(ymin, ymax)) +
  scale_color_discrete(guide = FALSE) +
  ggtitle(paste0(pct_of_total_ret,"% of Monthly U.S. Stock Returns\nRepresent The Entire Gain Since ", first_year)) +
  of_dollars_and_data_theme +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "Highest to Lowest 1-Month Real Return" , y = "U.S. Stock Real 1-Month Return (%)")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Real returns include reinvested dividends.")

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