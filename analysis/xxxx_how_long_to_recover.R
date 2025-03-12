cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

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

folder_name <- "xxxx_how_long_to_recover"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    filter(date >= "1920-01-01",
                           date <= "2024-12-31")
  
# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(date = as.Date(1:nrow(vp), origin=Sys.Date()), drawdown = numeric(nrow(vp)))
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

drops <- c(-0.1, -0.2, -0.3, -0.4, -0.5)

results_df <- data.frame(matrix(NA, nrow=length(drops), ncol=0))

## These next functions will be used for summary stats
# Find the amount of time from bottom to peak
bottom_to_peak <- function(drop, j, name){
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
  results_df[j, "name"]    <- name
  results_df[j, "median_bottom_to_peak"] <- median(n_months_recovery_ex_0)/12
  print(paste0("The average number of months from a ", drop, " drawdown to a peak is: ", mean(n_months_recovery_ex_0)))
  assign("results_df", results_df, envir = .GlobalEnv)
}

# Find the amount of time from peak to bottom
peak_to_bottom <- function(drop, j, name){
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
  results_df[j, "name"]    <- name
  results_df[j, "median_peak_to_bottom"] <- median(n_months_to_drop_ex_0)/12
  print(paste0("The average number of months from a peak to a ", drop, " drawdown is: ", mean(n_months_to_drop_ex_0)))
  assign("results_df", results_df, envir = .GlobalEnv)
}

# Find the amount of time from peak to known bottom
# A known bottom is biased in that we know that we will hit a certain drawdown
peak_to_known_bottom <- function(drop, j, name){
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
  results_df[j, "name"]    <- name
  results_df[j, "median_peak_to_known_bottom"] <- median(n_months_to_drop_ex_0)/12
  print(paste0("The average number of months from a peak to a known ", drop, " drawdown is: ", mean(n_months_to_drop_ex_0)))
  assign("results_df", results_df, envir = .GlobalEnv)
}

# Loop through each drop amount
for (j in 1:length(drops)){
  # Create dataset with drawdown info and S&P 500 prices
  sp500_ret_pe_dd <- sp500_ret_pe %>%
    left_join(dd)
  
  if (j == 1){
    name <- "10 Percent"
  } else if (j == 2){
    name <- "20 Percent"
  } else if (j == 3){
    name <- "30 Percent"
  } else if (j == 4){
    name <- "40 Percent"
  } else if (j == 5){
    name <- "50 Percent"
  }
  
  peak_to_known_bottom(drops[j], j, name)
  bottom_to_peak(drops[j], j, name)
  peak_to_bottom(drops[j], j, name)
  
  # First tag the recoveries
  sp500_ret_pe_dd <- arrange(sp500_ret_pe_dd, date)
  limit <- 0
  for (i in 1:nrow(sp500_ret_pe_dd)){
   if(sp500_ret_pe_dd[i, "drawdown"] < drops[j] & limit == 0){
     sp500_ret_pe_dd[i, "drop"] <- 1
     sp500_ret_pe_dd[i, "peak"] <- 0
     sp500_ret_pe_dd[i, "recovery_date"] <- sp500_ret_pe_dd[(i+1), "date"]
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
  sp500_ret_pe_dd <- arrange(sp500_ret_pe_dd, desc(date))
  limit <- 0
  for (i in 1:nrow(sp500_ret_pe_dd)){
    if(sp500_ret_pe_dd[i, "peak"] == 0 & limit == 0){
      sp500_ret_pe_dd[i, "drop"]   <- 1
      sp500_ret_pe_dd[i, "drop_date"] <- sp500_ret_pe_dd[i, "date"]
      limit <- 1
    } else if (sp500_ret_pe_dd[i, "drawdown"] != 0 & limit == 1){
      sp500_ret_pe_dd[i, "drop"]   <- 1
      sp500_ret_pe_dd[i, "drop_date"] <- sp500_ret_pe_dd[(i-1), "drop_date"]
    } else if (sp500_ret_pe_dd[i, "drawdown"] == 0){
      limit <- 0
    }
  }
  
  first_value <- sp500_ret_pe_dd %>%
                  filter(date == min(sp500_ret_pe_dd$date)) %>%
                  pull(price_plus_div)
  
  to_plot <- sp500_ret_pe_dd %>%
                arrange(date) %>%
                mutate(price_plus_div = price_plus_div/first_value)
  
  drops_df      <- filter(to_plot, drop == 1, drop_date != date, lag(peak) == 1)
  recoveries_df <- filter(to_plot, recovery == 1, recovery_date != date, lead(peak) == 1)
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/sp500_bottoms_and_peaks_", name, ".jpeg")
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Blue bars correspond to drawdowns and green bars correspond to the subsequent recoveries. ",
                                 "Performance includes dividends and is adjusted for inflation."),
                          width = 85)
  
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = date, y = price_plus_div)) +
    geom_rect(data=drops_df, aes(xmin = date, ymin = 0, 
                  xmax = as.Date(drop_date), ymax = price_plus_div),
              fill = "#1f78b4") +
    geom_rect(data=recoveries_df, aes(xmin = date, ymin = 0, 
                              xmax = as.Date(recovery_date), ymax = price_plus_div),
              fill = "green") +
    geom_line() +
    ggtitle(paste0("The S&P 500 Drops Fast and Recovers Slow\n", name, " Drawdowns")) +
    scale_y_continuous(label = function(x) scales::dollar(x, accuracy = 1),
                       trans = log_trans(), breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
    of_dollars_and_data_theme +
    labs(x = "Year", y = "Growth of $1 (Log Scale)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm") 
}

# Export the peak to bottom and bottom to peak results
export_to_excel(df = results_df,
                outfile = paste0(out_path, "/peak_to_bottom_results_df.xlsx"),
                sheetname = "results",
                new_file = 1,
                fancy_formatting = 0)

# ############################  End  ################################## #

  
