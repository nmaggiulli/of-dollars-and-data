cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(tidyverse)

folder_name <- "0483_pray_for_beta"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

plot_years <- function(n_years, start_date, end_date, title_string, note_extra){
  # Subset based on start date
  df <- sp500_ret_pe %>%
          filter(date >= start_date,
                 date < end_date)
  
  # Find number of months available
  n_months_avail <- nrow(df)
  
  # 1. Find the number of FULL groups
  n_groups_to_calc <- floor(n_months_avail/(n_years*12))
  n_months_to_calc <- n_years*12
  
  # Initialize the return column
  df$ret <- NA
  
  # Calculate full 10-year periods
  counter <- 1
  for (i in 1:n_groups_to_calc){
    final <- df[(counter + n_months_to_calc - 1), "price_plus_div"]
    initial <- df[counter, "price_plus_div"]
    
    df[counter, "ret"] <- (final/initial)^(1/n_years) - 1
    counter <- counter + n_months_to_calc
  }
  
  # 2. ADD LOGIC FOR THE PARTIAL DECADE (2020s so far)
  # Check if there is data left after the last full decade
  if (n_months_avail > (n_groups_to_calc * n_months_to_calc)) {
    last_start_index <- (n_groups_to_calc * n_months_to_calc) + 1
    
    # Calculate how many years have actually passed in this partial period
    # This ensures the "Annualized" math is correct
    actual_months <- n_months_avail - last_start_index + 1
    actual_years_passed <- actual_months / 12
    
    final_val <- df[n_months_avail, "price_plus_div"]
    initial_val <- df[last_start_index, "price_plus_div"]
    
    # Annualize based on the actual time elapsed so far
    df[last_start_index, "ret"] <- (final_val/initial_val)^(1/actual_years_passed) - 1
  }
  
  # Prepare plotting data
  to_plot <- df %>%
    filter(!is.na(ret)) %>%
    mutate(year = year(date)) %>%
    select(year, ret)
  
  start_date_string <- as.character(start_date)
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/period_returns_", n_years, "yr_", start_date_string, ".jpeg")
  
  # Strings for source and note
  source_string <- "Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"
  
  note_string <- str_wrap(paste0("Note:  Adjusted for dividends and inflation. ", note_extra),
                          width = 85)
  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = to_plot, aes(x = as.factor(year), y = ret)) +
    geom_bar(stat = "identity", position = "dodge", fill = "blue") +
    geom_text(data = to_plot, 
              aes(as.factor(year), ret + (0.005 * sign(ret)), label = paste0(round(100*ret, 1), "%")),
              col = "black", 
              size = 4) +
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

plot_years(10, as.Date("1900-01-01"), as.Date("2026-01-01"), "Lucky and Unlucky Decades for the S&P 500", "Where complete decade data is not available, partial data is shown.")
plot_years(20, as.Date("1960-01-01"), as.Date("2000-01-01"), "From 1960-1980, Beating the Market by 5%\nWould Have Made You LESS Money Than\nUnderperforming By 5% From 1980-2000", "")

all_20yr_returns <- sp500_ret_pe %>%
                      mutate(ret_20yr = (lead(price_plus_div, 240)/price_plus_div)^(1/20) - 1) %>%
                      select(date, ret_20yr)

#Plot rolling 20-year annualized returns
# Set the file_path based on the function input 
file_path = paste0(out_path, "/rolling_20yr_annualized_returns.jpeg")

# Strings for source and note
source_string <- "Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"

note_string <- str_wrap(paste0("Note:  Adjusted for dividends and inflation."),
                        width = 85)

# Plot the returns to show how much they change over time
plot <- ggplot(data = all_20yr_returns, aes(x = date, y = ret_20yr)) +
  geom_line() +
  ggtitle("Rolling 20-Year Annualized Real Return\nS&P 500") +
  scale_y_continuous(labels = percent) +
  of_dollars_and_data_theme +
  labs(x = "Date" , y = paste0("Annualized Real Return (%)\nFor 20 Years"),
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #