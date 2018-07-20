cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(tidyverse)

folder_name <- "82-pray-for-beta"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

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
sp500_ret_pe <- select(sp500_ret_pe, date, price_plus_div) %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")))

plot_years <- function(n_years, start_date, title_string){
  # Subset based on start date
  df <- sp500_ret_pe %>%
          filter(date >= start_date)
  
  # Find number of months available
  n_months_avail <- nrow(df)
  
  # Find the number of groups we can calculate
  n_groups_to_calc <- floor(n_months_avail/(n_years*12))
  n_months_to_calc <- n_years*12
  
  df <- df[(1:(n_months_to_calc*n_groups_to_calc)), ]
  
  counter <- 1
  for (i in 1:n_groups_to_calc){
    final <- df[(counter + n_months_to_calc - 1), "price_plus_div"]
    initial <- df[counter, "price_plus_div"]
    
    df[counter, "ret"] <- (final/initial)^(1/n_years) - 1
    
    counter <- counter + n_months_to_calc
  }
  
  to_plot <- df %>%
              filter(!is.na(ret)) %>%
              mutate(year = year(date)) %>%
              select(year, ret)
  
  start_date_string <- as.character(start_date)
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/returns_", n_years, "yr_", start_date_string, ".jpeg")
  
  # Strings for source and note
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"
  
  note_string <- str_wrap(paste0("Note:  Adjusted for dividends and inflation."),
                          width = 85)
  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = to_plot, aes(x = as.factor(year), y = ret)) +
    geom_bar(stat = "identity", position = "dodge", fill = "blue") +
    geom_text(data = to_plot, 
              aes(as.factor(year), ret, label = paste0(round(100*ret, 1), "%")),
              col = "black", 
              vjust = 1, size = 4) +
    ggtitle(title_string) +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(labels = percent) +
    scale_x_discrete() +
    of_dollars_and_data_theme +
    labs(x = "Starting Year" , y = paste0("Annualized Real Return (%)\nFor ", n_years, " Years"),
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_years(20, as.Date("1960-01-01"), "From 1960-1980, Beating the Market by 5%\nWould Have Made You LESS Money Than\nUnderperforming By 5% From 1980-2000")
plot_years(30, as.Date("1900-01-01"), "30-Year Periods Show Less Dispersion")
plot_years(10, as.Date("1900-01-01"), "Lucky and Unlucky Decades for the S&P 500")


# ############################  End  ################################## #