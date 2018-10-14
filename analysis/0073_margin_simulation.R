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
library(fTrading)
library(quantmod)
library(stats)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

# This seed allows us to have reproducible random sampling
set.seed(12345)         

# Make a list of the best stocks (tickers) of 2014-2017 (from Motley Fool)
# I toss any that Yahoo cannot pull the data for
best_stocks_2014 <- c("AGIO", "BLUE", "ACHN",
                      "PTCT", "TTPH", "HA", "CMRX", "SWKS")
best_stocks_2015 <- c("NFLX", "AMZN", "ATVI", "NVDA",
                      "HRL", "VRSN", "SBUX", "FSLR")
best_stocks_2016 <- c("NVDA", "OKE", "FCX", "NEM", 
                      "AMAT", "PWR", "CMA", "MLM", "HAL")
best_stocks_2017 <- c("MDGL", "SGMO", "PIRS", "ESPR", 
                       "NKTR", "DVAX", "IMMU", "SPPI")

# Create a data frame for the year and the tickers
# Subset this data frame if need be
to_pull <- data.frame(ticker = 
                        c(
                        best_stocks_2014,
                        best_stocks_2015,
                        best_stocks_2016, 
                        best_stocks_2017),
                      year = 
                        c(
                        rep(2014, length(best_stocks_2014)),
                        rep(2015, length(best_stocks_2015)),
                        rep(2016, length(best_stocks_2016)), 
                        rep(2017, length(best_stocks_2017)))
                      ) %>%
            mutate(ticker = as.character(ticker))

# Starting capital
starting_capital <- 10000

# Number of trading days to simulate (NYSE average)
n_trading_days <- 252

# Grab data if dummy is on
grab_data <- 0

if (grab_data == 1){
  
  
  # Create a function to grab the data from Yahoo
  grab_data <- function(ticker, year){
    print(ticker)
    getSymbols(ticker, from = paste0(year,'-01-01'), to = paste0(year,'-12-31'), 
               src="yahoo", periodicity = "daily")
    df <- data.frame(date = index(get(ticker)), 
                     get(ticker), row.names=NULL) %>%
      select(date, contains("Adjust"))
    
    colnames(df) <- c("date", "price")
    
    for (i in 1:nrow(df)){
      df[i, "ticker"] <- ticker
      if (i > 1){
        df[i, "ret"] <- df[i, "price"]/df[(i-1), "price"]
      }
    }
    return(df)
  }
  
  # Download the data for all the top performing stocks and stack it
  for (s in 1:nrow(to_pull)){
    temp <- grab_data(to_pull[s, "ticker"], to_pull[s, "year"])
    if (s == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
    }
  }
  
  final_df <- df %>%
                filter(!is.na(ret)) %>%
                group_by(date) %>%
                summarize(ret = sum(ret/n())) %>%
                ungroup() %>%
                mutate(trading_day = row_number())
  
  write.csv(final_df, paste0(exportdir, "73-margin-simulation/daily_ret_top_stocks.csv"), row.names = FALSE)
}

# Read in data saved on disk to save time
final_df <- read.csv(paste0(exportdir, "73-margin-simulation/daily_ret_top_stocks.csv")) %>%
              mutate(year = year(date)) %>%
              filter(year > 2013)

ret_matrix <- as.matrix(final_df$ret)

# Create a function to simulate the leveraged returns (resampling)
simulate_with_leverage <- function(n_simulations, leverage, sample_length){

  # Pick days randomly to use in the simulation
  days_sample <- sample(1:nrow(ret_matrix), n_trading_days*n_simulations/sample_length, replace = TRUE)

  days_sample_final <- vector()
  for (i in 1:(length(days_sample))){
    for (j in 1:sample_length){
      if (j == 1){
        days_sample_final[sample_length*(i-1)+1] <- days_sample[i]
      } else{
        if (days_sample[i] > (nrow(final_df) - sample_length)){
          days_sample_final[sample_length*(i-1)+j] <- nrow(ret_matrix) - sample_length + j - 1
        } else {
          days_sample_final[sample_length*(i-1)+j] <- days_sample_final[sample_length*(i-1)+1] + (j-1)
        }
      }
    }
  }
  
  # Calculate the return for each day by looping through the number of trading days and
  # summing n_stocks worth (since they have already been weighted)
  results_matrix <- matrix(data = NA, nrow = n_trading_days, ncol = (n_simulations+1))
  
  for (j in 1:n_simulations){
    for (i in 1:n_trading_days){
      # Calculate the equal weighted return across your N stocks
      ret <- ret_matrix[days_sample_final[i+(j-1)*n_trading_days]]
      
      if (i == 1){
        results_matrix[i, j] <- starting_capital*leverage*ret
      } else{
        results_matrix[i, j] <- results_matrix[(i-1), j]*ret
      }
      
      # If your capital ever drops below your starting capital, game over
      if (results_matrix[i, j] - (starting_capital*(leverage-1)) <= 0){
        results_matrix[i, j] <- 0
      }
    }
  }
  broke_pct <- rowSums(results_matrix[, 1:n_simulations] == 0)[n_trading_days]/n_simulations
  
  print(paste0("With ", leverage, "x leverage and 0% maintenance margin, you go broke in ",
        100*(broke_pct),
        "% of all simulations.")
      )
  
  results_matrix[, (n_simulations+1)] <- seq(1, n_trading_days)
  colnames(results_matrix) <- c(paste0("sim_",seq(1, n_simulations)), "trading_day")
  
  to_plot <- as.data.frame(results_matrix) %>%
                  gather(key=key, value = value, -trading_day)
  
  file_path <- paste0(exportdir, "73-margin-simulation/", leverage, "x_leverage_with_", sample_length, "_sample.jpeg")
  
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  Yahoo Finance (OfDollarsAndData.com)"),
                            width = 85)
  
  note_string <- str_wrap(paste0("Note:  Re-samples the equal weighted return of the top performing stocks over a " 
                      , sample_length,  
                      " day period with no maintenance margin.  Over ", format(n_simulations, big.mark = ",", scientific = FALSE), " simulations, you go broke ", 100*broke_pct, "% of the time."),
                      width = 85)       
  
  plot <- ggplot(to_plot, aes(x=trading_day, y=value, col = key)) +
            geom_line() +
            geom_hline(linetype = "dashed", col = "red", yintercept = starting_capital*(leverage-1)) +
            scale_color_discrete(guide = FALSE) +
            scale_y_continuous(label = dollar, limits = c(0, starting_capital*(leverage+1))) +
            of_dollars_and_data_theme +
            ggtitle(paste0("Account Value of Top Stock Portfolio\n",
                   "Using ", leverage, "x Leverage")) +
            labs(x = "Trading Day", y = "Account Value",
                 caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  return(broke_pct)
}

# Set sample lengths and maintenance margins to test
sample_lengths <-c(1)
leverage_ratios <- c(2, 4, 5, 10, 20, 50, 100, 200)
n_simulations <- 1000

# Loop through different sample lengths and leverage ratios
# Note that sample_length must be 1 or an even number
final_results_df <- data.frame(matrix(NA, 
                    nrow = length(leverage_ratios)*length(sample_lengths), 
                    ncol = 4)
                    )
counter <- 1
for (s in sample_lengths){
  print(paste0("The day sample length is ", s))
  for (l in leverage_ratios){
    broke_pct <- simulate_with_leverage(n_simulations, l, s)
    final_results_df[counter, 1] <- l
    final_results_df[counter, 2] <- s
    final_results_df[counter, 3] <- n_simulations
    final_results_df[counter, 4] <- broke_pct
    
    counter <- counter + 1
  }
}

colnames(final_results_df) <- c("leverage", "sample_length", "n_simulations", "broke_pct")

to_plot <- final_results_df 

file_path <- paste0(exportdir, "73-margin-simulation/all_simulation_results.jpeg")

# Add a source and note string for the plots
source_string <- str_wrap(paste0("Source:  Yahoo Finance (OfDollarsAndData.com)"),
                          width = 85)

note_string <- str_wrap(paste0("Note:  Re-samples the equal weighted return of the top performing stocks and assumes 0% maintenance margin."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=leverage, y=broke_pct, col = as.factor(sample_length))) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = percent, limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("The Dangers of More Leverage")) +
  labs(x = "Leverage", y = "Percentage of Simulations Where You Go Broke",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #