cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(magrittr)
library(dplyr)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds")) %>%
                    filter(Date >= "1880-01-01")

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
  dd      <- data.frame(Date = as.Date(1:nrow(vp), origin=Sys.Date()), drawdown = numeric(nrow(vp)))
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

# Create vector of drawdown percentages to loop over

drops <- c(-0.05, -0.1, -0.2, -0.3, -0.4, -0.5)
#drops <- c(-0.05)

## These next functions will be used for summary stats
# Find the amount of time from bottom to peak
bottom_to_peak <- function(drop){
  limit                <- 0
  n_months_below_limit <- 0
  n_months_recovery    <- c()
  for (i in 2:nrow(dd)){
    if (dd[i, "drawdown"] == 0){
      n_months_recovery    <- c(n_months_recovery, n_months_below_limit)
      limit                <- 0
      n_months_below_limit <- 0
    } else if (dd[i, "drawdown"] < drop | limit == 1){
      limit <- 1
      n_months_below_limit <- n_months_below_limit + 1
    }
  }
  
  n_months_recovery_ex_0 <- n_months_recovery[!n_months_recovery %in% c(0)]
  print(paste0("Average months for a ", drop ," recovery: ", mean(n_months_recovery_ex_0)))
  print(paste0("Median months for a ", drop ," recovery: ", median(n_months_recovery_ex_0)))
}

# Find the amount of time from peak to bottom
peak_to_bottom <- function(drop){
  n_months_above_limit <- 0
  n_months_to_drop    <- c()
  limit <- 0
  for (i in 2:nrow(dd)){
    if (dd[i, "drawdown"] < drop & limit == 0){
      limit <- 1
      n_months_to_drop    <- c(n_months_to_drop, n_months_above_limit)
    } else if (dd[i, "drawdown"] > drop & limit == 0){
      n_months_above_limit <- n_months_above_limit + 1
    } else if (dd[i, "drawdown"] == 0 & limit == 1){
      limit <- 0
      n_months_above_limit <- 0
    }
  }
  
  n_months_to_drop_ex_0 <- n_months_to_drop[!n_months_to_drop %in% c(0)]
  print(paste0("Average months for a ", drop ," drop: ", mean(n_months_to_drop_ex_0)))
  print(paste0("Median months for a ", drop ," drop: ", median(n_months_to_drop_ex_0)))
}

# Find the amount of time from peak to known bottom
# A known bottom is biased in that we know that we will hit a certain drawdown
peak_to_known_bottom <- function(drop){
  n_months_above_limit <- 0
  n_months_to_drop    <- c()
  limit <- 0
  for (i in 2:nrow(dd)){
    if (dd[i, "drawdown"] < drop & limit == 0){
      limit <- 1
      n_months_to_drop    <- c(n_months_to_drop, n_months_above_limit)
    } else if (dd[i, "drawdown"] == 0){
      limit <- 0
      n_months_above_limit <- 0
    }
    n_months_above_limit <- n_months_above_limit + 1
  }
  
  n_months_to_drop_ex_0 <- n_months_to_drop[!n_months_to_drop %in% c(0)]
  print(paste0("Average months for a known ", drop ," drop: ", mean(n_months_to_drop_ex_0)))
  print(paste0("Median months for a known ", drop ," drop: ", median(n_months_to_drop_ex_0)))
}

# Loop through each drop amount
for (j in 1:length(drops)){
  # Create dataset with drawdown info and S&P 500 prices
  sp500_ret_pe_dd <- sp500_ret_pe %>%
    left_join(dd)
  
  bottom_to_peak(drops[j])
  peak_to_bottom(drops[j])
  peak_to_known_bottom(drops[j])
  
  if (j == 1){
    name <- "5 Percent"
  } else if (j == 2){
    name <- "10 Percent"
  } else if (j == 3){
    name <- "20 Percent"
  } else if (j == 4){
    name <- "30 Percent"
  } else if (j == 5){
    name <- "40 Percent"
  } else if (j == 6){
    name <- "50 Percent"
  }
  
  # First tag the recoveries
  sp500_ret_pe_dd <- arrange(sp500_ret_pe_dd, Date)
  limit <- 0
  for (i in 1:nrow(sp500_ret_pe_dd)){
   if(sp500_ret_pe_dd[i, "drawdown"] < drops[j] & limit == 0){
     sp500_ret_pe_dd[i, "drop"] <- 1
     sp500_ret_pe_dd[i, "peak"] <- 0
     sp500_ret_pe_dd[i, "recovery_date"] <- sp500_ret_pe_dd[(i+1), "Date"]
     limit <- 1
   } else if (sp500_ret_pe_dd[i, "drawdown"] != 0 & limit == 1){
     sp500_ret_pe_dd[i, "recovery"] <- 1
     sp500_ret_pe_dd[i, "recovery_date"] <- sp500_ret_pe_dd[(i-1), "recovery_date"]
     sp500_ret_pe_dd[i, "peak"] <- 999
   } else if (sp500_ret_pe_dd[i, "drawdown"] == 0){
     limit                      <- 0
     sp500_ret_pe_dd[i, "peak"] <- 1
   } else{
     sp500_ret_pe_dd[i, "peak"]     <- 999
   }
  }
  
  # Sort the data backwards and then go through it again to flag the drops
  sp500_ret_pe_dd <- arrange(sp500_ret_pe_dd, desc(Date))
  limit <- 0
  for (i in 1:nrow(sp500_ret_pe_dd)){
    if(sp500_ret_pe_dd[i, "peak"] == 0 & limit == 0){
      sp500_ret_pe_dd[i, "drop"]   <- 1
      sp500_ret_pe_dd[i, "drop_date"] <- sp500_ret_pe_dd[i, "Date"]
      limit <- 1
    } else if (sp500_ret_pe_dd[i, "drawdown"] != 0 & limit == 1){
      sp500_ret_pe_dd[i, "drop"]   <- 1
      sp500_ret_pe_dd[i, "drop_date"] <- sp500_ret_pe_dd[(i-1), "drop_date"]
    } else if (sp500_ret_pe_dd[i, "drawdown"] == 0){
      limit <- 0
    }
  }
  
  to_plot <- arrange(sp500_ret_pe_dd, Date)
  
  drops_df      <- filter(to_plot, drop == 1, drop_date != Date, lag(peak) == 1)
  recoveries_df <- filter(to_plot, recovery == 1, recovery_date != Date, lead(peak) == 1)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "36-stocks-ladder/sp500-bottom-peaks-", name, ".jpeg")
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = Date, y = price_plus_div)) +
    geom_rect(data=drops_df, aes(xmin = Date, ymin = 0, 
                  xmax = as.Date(drop_date), ymax = price_plus_div),
              fill = "red") +
    geom_rect(data=recoveries_df, aes(xmin = Date, ymin = 0, 
                              xmax = as.Date(recovery_date), ymax = price_plus_div),
              fill = "green") +
    geom_line() +
    ggtitle(paste0("The S&P 500 Drops Fast and Recovers Slowly\n", name, " Drawdowns")) +
    scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
    of_dollars_and_data_theme +
    labs(x = "Year", y = "Real Price + Dividends (Log Scale)")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
  note_string <- paste0("Note:  Red bars correspond to drawdowns and green bars are the subsequent recoveries.") 
  
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
}






# ############################  End  ################################## #

  
