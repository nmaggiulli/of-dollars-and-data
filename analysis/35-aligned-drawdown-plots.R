cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

# Plotting parameter (how many years after the peak do you want to plot??)
n_years_after_peak <- 5

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Calculate returns for the S&P data
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

# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, Date, price_plus_div) %>%
                  mutate(Date = as.Date(paste0(
                    substring(as.character(Date), 1, 4),
                    "-", 
                    ifelse(substring(as.character(Date), 6, 7) == "1", "10", substring(as.character(Date), 6, 7)),
                    "-01", 
                    "%Y-%m-%d")))
  
# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(Date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
  loc_max <- 0
  for (i in 1:(nrow(vp))){
    if (vp[i, 2] < loc_max & i != 1){
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- vp[i, 2]/loc_max - 1
    } else{
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- 0
      loc_max  <- vp[i, 2]
    }
  }
  return(dd)
}

# Find the drawdowns for investigative purposes
# Use this list to find the most recent peak from each bottom in the data
dd        <- drawdown_path(sp500_ret_pe)

# Select the date list manually
date_list <- c("1929-09-01", "1973-03-01", "1987-08-01", "2000-08-01", "2007-10-01")

# Create a counter for the subsets 
counter            <- 1

# Loop through each date to make a subset
for (date in date_list){
  sp500_filtered <- filter(sp500_ret_pe, Date >= as.Date(date), Date < as.Date(date) %m+% months(n_years_after_peak * 12))
  for (i in 1:nrow(sp500_filtered)){
    sp500_filtered[i, "month"] <- i
    if (i == 1){
      sp500_filtered[i, "index"] <- 100
    } else {
      sp500_filtered[i, "index"] <- sp500_filtered[(i-1), "index"] * (1 + (sp500_filtered[(i), "price_plus_div"]/sp500_filtered[(i-1), "price_plus_div"] - 1))
    }
  }
  
  # Drop unneeded columns
  sp500_filtered <- sp500_filtered %>% 
                      mutate(peak = year(date)) %>%
                      select(month, peak, index)
  
  # Append the rows as we loop through each subset
  if (counter == 1){
    to_plot <- sp500_filtered
  } else{
    to_plot <- bind_rows(to_plot, sp500_filtered)
  }
  counter <- counter + 1
}

# Set the file_path based on the function input 
file_path = paste0(exportdir, "35-aligned-drawdown-plots/sp500-aligned-drawdowns.jpeg")

# Create title with ticker in subtitle
top_title <- paste0("Every Market Crash Exhibits Different\nBehavior Over Time")

# Create the plot object
plot <- ggplot(to_plot, aes(x = month, y = index, col = as.factor(peak))) +
  geom_line() +
  geom_hline(yintercept = 100, linetype =  "dashed", col = "black") +
  geom_text_repel(data = filter(to_plot, month == max(to_plot$month)), 
                  aes(x = month, 
                      y = index, 
                      col = as.factor(peak), 
                      label = as.character(peak),
                      family = "my_font"
                  ),
                  max.iter = 3000) +
  ggtitle(top_title) +
  guides(col = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, 25)) +
  scale_x_continuous(limits = c(0, n_years_after_peak*12), breaks = seq(0, n_years_after_peak*12, 12)) +
  labs(x = "Months Since Peak", y = "Real Index (Peak = 100)")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
note_string <- paste0("Note:  Real return includes reinvested dividends.") 

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 

# ############################  End  ################################## #

  
