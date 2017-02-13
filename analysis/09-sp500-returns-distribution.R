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
      sp500_ret_pe[i, "n_shares"] <- 1
      sp500_ret_pe[i, "new_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
      sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
    } else{
      sp500_ret_pe[i, "n_shares"] <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
      sp500_ret_pe[i, "new_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
      sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
      sp500_ret_pe[i, "ret_1_month"] <- sp500_ret_pe[i, "price_plus_div"]/sp500_ret_pe[(i - 1), "price_plus_div"] - 1
    }
  }

  sp500_total_ret <- (sp500_ret_pe[nrow(sp500_ret_pe), "price_plus_div"]/sp500_ret_pe[1, "price_plus_div"]) - 1
  sp500_price_ret <- (sp500_ret_pe[nrow(sp500_ret_pe), "real_price"]/sp500_ret_pe[1, "real_price"]) - 1
  sp500_div_ret <- sp500_ret_pe[nrow(sp500_ret_pe), "n_shares"]   
  print(paste0("Price return starting at ", date_var, " accounted for: ", round(100 * (sp500_price_ret / (sp500_price_ret + sp500_div_ret))), "% of the total return."))
  print(paste0("The total return is:  ", round(100 * sp500_total_ret), "%"))
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
      if((sp500_ret_pe[i, "ret_cumulative"] - 1) > sp500_total_ret){
        break
      }
    }
  }
  
  filtered <- filter(sp500_ret_pe, before_total == 1, is.na(lead(before_total)))
  print(paste0("It took ", round(100 * filtered$pct_of_months), "% of months to equal the total return"))
  to_plot <- filter(sp500_ret_pe, !is.na(ret_1_month))
  n_months <- nrow(to_plot)
  pct_of_total_ret <- round(100 * filtered$pct_of_months)

# Alter before_total flag to reflect 2 for all times where it is NA
to_plot[, "before_total"] <- apply(to_plot[, "before_total"], 1, function(x){ifelse(!is.na(x), x, 0)})

print(paste0("Number of months for cumulative gains:  ", nrow(filter(to_plot, before_total == 1))))

# Alter before_total flag to equal -1 when the 1-month return is below zero
to_plot[which(to_plot$ret_1_month < 0), "before_total"] <- -1

# Get the max, middle, and min y_filter for the labels
max_y_filter <- max(to_plot$ret_1_month)
min_y_filter <- min(to_plot$ret_1_month)

# Find the ymax and ymin for plotting the heights of the y axis properly
ymax <- ceiling(max(to_plot[, "ret_1_month"]) * 10)/ 10
ymin <- floor(min(to_plot[, "ret_1_month"]) * 10) / 10

# Set the file_path for the next output
file_path = paste0(exportdir, "09-sp500-returns-pe/top-monthly-returns-",first_year,".jpeg")

# Create the plot with labels using geom_text_repel
plot <- ggplot(data = to_plot, aes(x = reorder(Date, -ret_1_month), y = ret_1_month, col = as.factor(before_total), fill =  as.factor(before_total))) +
  geom_bar(stat = "identity") +
  geom_text_repel(data = filter(to_plot, ret_1_month == max_y_filter),
                  aes(x  = reorder(Date, -ret_1_month),
                      y = ymax / 3,
                      col = as.factor(before_total),
                  label = str_wrap(paste0("These returns represent all of the gains since ", first_year), width = 20),
                  family = "my_font"),
                  nudge_x = n_months / 4,
                  nudge_y = ymax / 2) +
  geom_text_repel(data = to_plot[round(nrow(to_plot)/ 2), ],
                  aes(x  = reorder(Date, -ret_1_month),
                      y = 0,
                      col = as.factor(before_total),
                      label = "These gains are canceled out",
                      family = "my_font"),
                      nudge_y = ymin / 2) +
  geom_text_repel(data = filter(to_plot, ret_1_month == min_y_filter),
                  aes(x  = reorder(Date, -ret_1_month),
                      y = ymin / 2,
                      col = as.factor(before_total),
                      label = "by these losses",
                      family = "my_font"),
                      nudge_x = -(n_months / 2),
                      nudge_y = ymin / 4) +
  scale_y_continuous(label = percent, limits = c(ymin, ymax)) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  ggtitle(paste0(pct_of_total_ret,"% of Monthly U.S. Stock Returns\nRepresent The Entire Gain Since ", first_year)) +
  of_dollars_and_data_theme +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "Highest to Lowest 1-Month Real Return" , y = "U.S. Stock Real 1-Month Return")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Real returns include reinvested dividends.  ",  formatC(n_months, format="d", big.mark=','), " monthly returns are shown.")

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

#Try a histogram plot as well

# Set the file_path for the next output
file_path = paste0(exportdir, "09-sp500-returns-pe/hist-returns-",first_year,".jpeg")

# Create the plot with labels using geom_text_repel
plot <- ggplot(data = to_plot, aes(x = ret_1_month)) +
      geom_histogram(aes(y=..density..), 
                     colour = "red", 
                     fill = "red",
                     binwidth = 0.01) +
  stat_function(fun=dnorm, args=list(mean=mean(to_plot$ret_1_month), sd=sd(to_plot$ret_1_month))) +
  ggtitle(paste0("Histogram of Real Returns Starting in ", first_year)) +
  of_dollars_and_data_theme +
  labs(x = "U.S. Stock Real 1-Month Return" , y = "Count")

# Add a source and note string for the plots
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm, ", first_year, " - ", last_year," (OfDollarsAndData.com)")
note_string   <- paste0("Note:  Real returns include reinvested dividends.  ",  formatC(n_months, format="d", big.mark=','), " monthly returns are shown.")

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

# Try out a normality 
print(shapiro.test(to_plot$ret_1_month))

}

# Run the function for the full period
filter_year(1871.01)

# Run it for 2000.01 - 2016.12
filter_year(2000.01)


# ############################  End  ################################## #