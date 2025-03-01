cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(tidyverse)
library(scales)
library(lubridate)

folder_name <- "_fl/0017_accumulation_rebal"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Define target weights
target_weights <- c(
  VTSMX = 0.35,
  VBMFX = 0.30,
  VGTSX = 0.20,
  VBISX = 0.10,
  VEIEX = 0.05
)

pre_raw <- read.csv(paste0(importdir, "/", folder_name, "/timeseries_3-1-2025.csv"), skip = 6) %>%
          rename(symbol = Symbol,
                 name = Name) %>%
          select(-Metric) %>%
          gather(-symbol, -name, key=key, value=value) %>%
          mutate(date = as.Date(key, format= "X%Y.%m.%d"),
                 symbol = gsub("M:", "", symbol)) %>%
          arrange(symbol, date) %>%
          select(date, symbol, value) %>%
          drop_na()

data_dates <- pre_raw %>%
                group_by(date) %>%
                summarise(n_symbols = n()) %>%
                ungroup() %>%
                filter(n_symbols == length(target_weights))

raw <- pre_raw %>%
          inner_join(data_dates)

# Function to prepare data - convert to wide format and calculate returns
prepare_data <- function(data) {
  # Convert date to proper date format
  data$date <- as.Date(data$date, format = "%m/%d/%Y")
  
  # Pivot to wide format
  price_wide <- data %>%
    pivot_wider(names_from = symbol, values_from = value)
  
  # Calculate daily returns
  returns_wide <- price_wide %>%
    arrange(date) %>%
    mutate(across(-date, ~ . / lag(.) - 1)) %>%
    filter(!is.na(across(-date, ~ .)[1])) # Remove first row with NAs
  
  return(list(
    prices = price_wide,
    returns = returns_wide
  ))
}

# Function to run portfolio simulation with pro-rata contributions
simulate_prorata_portfolio <- function(returns_wide, prices_wide, initial_portfolio, monthly_contribution) {
  # Initialize portfolio
  symbols <- names(target_weights)
  
  # Find first date with complete data for all symbols
  first_valid_row <- 1
  while (first_valid_row <= nrow(prices_wide)) {
    if (all(!is.na(prices_wide[first_valid_row, symbols]))) {
      break
    }
    first_valid_row <- first_valid_row + 1
  }
  
  if (first_valid_row > nrow(prices_wide)) {
    stop("No complete data row found for all symbols")
  }
  
  # Initialize with the first valid date
  portfolio <- data.frame(date = prices_wide$date[first_valid_row])
  
  # Initial allocation using prices from the first valid date
  for (symbol in symbols) {
    initial_price <- prices_wide[[symbol]][first_valid_row]
    portfolio[[paste0(symbol, "_units")]] <- initial_portfolio * target_weights[symbol] / initial_price
    portfolio[[paste0(symbol, "_value")]] <- initial_portfolio * target_weights[symbol]
    portfolio[[paste0(symbol, "_weight")]] <- target_weights[symbol]  # Add initial weights
  }
  
  portfolio$total_value <- initial_portfolio
  portfolio$equity_weight <- target_weights[1] +  target_weights[3] +  target_weights[5]
  portfolio$contribution_to_date <- 0
  
  # Simulate portfolio over time
  for (i in 2:nrow(returns_wide)) {
    current_date <- returns_wide$date[i]
    prev_date <- returns_wide$date[i-1]
    new_row <- data.frame(date = current_date)
    
    # Apply daily returns to existing units
    for (symbol in symbols) {
      units_col <- paste0(symbol, "_units")
      value_col <- paste0(symbol, "_value")
      
      # Copy units from previous day
      new_row[[units_col]] <- portfolio[[units_col]][i-1]
      
      # Apply return to value
      daily_return <- returns_wide[[symbol]][i]
      new_row[[value_col]] <- portfolio[[value_col]][i-1] * (1 + daily_return)
    }
    
    # Calculate total portfolio value before contribution
    total_before_contribution <- sum(sapply(symbols, function(symbol) {
      new_row[[paste0(symbol, "_value")]]
    }))
    
    # Calculate current weights before contribution
    for (symbol in symbols) {
      weight_col <- paste0(symbol, "_weight")
      value_col <- paste0(symbol, "_value")
      new_row[[weight_col]] <- new_row[[value_col]] / total_before_contribution
    }
    
    # Add monthly contribution (on the first day of the month)
    new_row$contribution_to_date <- portfolio$contribution_to_date[i-1]
    
    if (month(current_date) != month(prev_date) || (i == 2 && day(current_date) == 1)) {
      # It's a new month, add contribution pro-rata
      for (symbol in symbols) {
        units_col <- paste0(symbol, "_units")
        value_col <- paste0(symbol, "_value")
        
        # Calculate additional units to buy
        contribution_to_symbol <- monthly_contribution * target_weights[symbol]
        additional_units <- contribution_to_symbol / (new_row[[value_col]] / new_row[[units_col]])
        
        # Update units and value
        new_row[[units_col]] <- new_row[[units_col]] + additional_units
        new_row[[value_col]] <- new_row[[value_col]] + contribution_to_symbol
      }
      
      new_row$contribution_to_date <- new_row$contribution_to_date + monthly_contribution
      
      # Recalculate weights after contribution
      total_after_contribution <- sum(sapply(symbols, function(symbol) {
        new_row[[paste0(symbol, "_value")]]
      }))
      
      for (symbol in symbols) {
        weight_col <- paste0(symbol, "_weight")
        value_col <- paste0(symbol, "_value")
        new_row[[weight_col]] <- new_row[[value_col]] / total_after_contribution
      }
    }
    
    # Calculate total portfolio value
    new_row$total_value <- sum(sapply(symbols, function(symbol) new_row[[paste0(symbol, "_value")]]))
    
    new_row$equity_weight <- new_row[["VTSMX_weight"]] + new_row[["VGTSX_weight"]] + new_row[["VEIEX_weight"]]
    
    # Add row to portfolio
    portfolio <- rbind(portfolio, new_row)
  }
  
  return(portfolio)
}

# Fixed strategic portfolio function that adds contributions to the most underweight asset
simulate_strategic_portfolio <- function(returns_wide, prices_wide, initial_portfolio, monthly_contribution) {
  # Initialize portfolio
  symbols <- names(target_weights)
  
  # Find first date with complete data for all symbols
  first_valid_row <- 1
  while (first_valid_row <= nrow(prices_wide)) {
    if (all(!is.na(prices_wide[first_valid_row, symbols]))) {
      break
    }
    first_valid_row <- first_valid_row + 1
  }
  
  if (first_valid_row > nrow(prices_wide)) {
    stop("No complete data row found for all symbols")
  }
  
  # Initialize with the first valid date
  portfolio <- data.frame(date = prices_wide$date[first_valid_row])
  
  # Initial allocation using prices from the first valid date
  for (symbol in symbols) {
    initial_price <- prices_wide[[symbol]][first_valid_row]
    portfolio[[paste0(symbol, "_units")]] <- initial_portfolio * target_weights[symbol] / initial_price
    portfolio[[paste0(symbol, "_value")]] <- initial_portfolio * target_weights[symbol]
    portfolio[[paste0(symbol, "_weight")]] <- target_weights[symbol]
  }
  
  portfolio$total_value <- initial_portfolio
  portfolio$equity_weight <- target_weights[1] +  target_weights[3] +  target_weights[5]
  portfolio$contribution_to_date <- 0
  
  # Create an index to track our position in the portfolio dataframe
  portfolio_idx <- 1
  
  # Simulate portfolio over time, starting from the second row of returns
  for (i in 2:nrow(returns_wide)) {
    current_date <- returns_wide$date[i]
    prev_date <- returns_wide$date[i-1]
    new_row <- data.frame(date = current_date)
    
    # Apply daily returns to existing units
    for (symbol in symbols) {
      units_col <- paste0(symbol, "_units")
      value_col <- paste0(symbol, "_value")
      weight_col <- paste0(symbol, "_weight")
      
      # Copy units from previous portfolio row (use portfolio_idx, not i-1)
      if (portfolio_idx <= nrow(portfolio)) {
        new_row[[units_col]] <- portfolio[[units_col]][portfolio_idx]
      } else {
        print(paste("Warning: Invalid portfolio index:", portfolio_idx, "for date:", current_date))
        next
      }
      
      # Apply return to value
      daily_return <- returns_wide[[symbol]][i]
      if (!is.na(daily_return)) {
        new_row[[value_col]] <- portfolio[[value_col]][portfolio_idx] * (1 + daily_return)
      } else {
        # If return is NA, keep the previous value
        new_row[[value_col]] <- portfolio[[value_col]][portfolio_idx]
      }
    }
    
    # Calculate current weights before contribution
    total_before_contribution <- sum(sapply(symbols, function(symbol) {
      val <- new_row[[paste0(symbol, "_value")]]
      if (is.null(val) || length(val) == 0) return(0)
      return(val)
    }))
    
    for (symbol in symbols) {
      weight_col <- paste0(symbol, "_weight")
      value_col <- paste0(symbol, "_value")
      
      if (total_before_contribution > 0) {
        new_row[[weight_col]] <- new_row[[value_col]] / total_before_contribution
      } else {
        new_row[[weight_col]] <- target_weights[symbol] # Default to target if can't calculate
      }
    }
    
    # Add monthly contribution (on the first day of the month)
    new_row$contribution_to_date <- portfolio$contribution_to_date[portfolio_idx]
    
    if (month(current_date) != month(prev_date) || (i == 2 && day(current_date) == 1)) {
      # Calculate weight differences from target - ONLY CONSIDERING UNDERWEIGHT
      # Positive differences mean the asset is underweight compared to target
      weight_diffs <- sapply(symbols, function(symbol) {
        weight_val <- new_row[[paste0(symbol, "_weight")]]
        if (is.null(weight_val) || length(weight_val) == 0 || is.na(weight_val)) return(-Inf)
        
        # Calculate how underweight the asset is (target - current)
        # Negative values mean overweight, so we'll replace with -Inf to ensure they're not selected
        underweight <- target_weights[symbol] - weight_val
        return(ifelse(underweight > 0, underweight, -Inf))
      })
      
      # Find the most underweight symbol (highest positive difference)
      if (all(weight_diffs == -Inf)) {
        # If all assets are overweight, distribute according to target weights
        print("All assets are at or above target weights, using pro-rata distribution")
        for (symbol in symbols) {
          units_col <- paste0(symbol, "_units")
          value_col <- paste0(symbol, "_value")
          
          contribution_to_symbol <- monthly_contribution * target_weights[symbol]
          if (!is.null(new_row[[value_col]]) && !is.null(new_row[[units_col]]) && 
              length(new_row[[value_col]]) > 0 && length(new_row[[units_col]]) > 0 && 
              new_row[[value_col]] > 0 && new_row[[units_col]] > 0) {
            
            additional_units <- contribution_to_symbol / (new_row[[value_col]] / new_row[[units_col]])
            new_row[[units_col]] <- new_row[[units_col]] + additional_units
            new_row[[value_col]] <- new_row[[value_col]] + contribution_to_symbol
          }
        }
      } else {
        # Find the most underweight symbol
        most_underweight_symbol <- symbols[which.max(weight_diffs)]
        
        # Add contribution to that symbol
        units_col <- paste0(most_underweight_symbol, "_units")
        value_col <- paste0(most_underweight_symbol, "_value")
        
        # Calculate additional units to buy - with safety check
        current_value <- new_row[[value_col]]
        current_units <- new_row[[units_col]]
        
        if (!is.null(current_value) && !is.null(current_units) && 
            length(current_value) > 0 && length(current_units) > 0 &&
            current_value > 0 && current_units > 0) {
          
          price_per_unit <- current_value / current_units
          additional_units <- monthly_contribution / price_per_unit
          
          # Update units and value
          new_row[[units_col]] <- current_units + additional_units
          new_row[[value_col]] <- current_value + monthly_contribution
        } else {
          # If we can't calculate, distribute according to target weights
          print(paste("Warning: Cannot calculate units for", most_underweight_symbol, "- using target weight instead"))
          for (symbol in symbols) {
            units_col <- paste0(symbol, "_units")
            value_col <- paste0(symbol, "_value")
            
            contribution_to_symbol <- monthly_contribution * target_weights[symbol]
            if (!is.null(new_row[[value_col]]) && !is.null(new_row[[units_col]]) && 
                length(new_row[[value_col]]) > 0 && length(new_row[[units_col]]) > 0 && 
                new_row[[value_col]] > 0 && new_row[[units_col]] > 0) {
              
              additional_units <- contribution_to_symbol / (new_row[[value_col]] / new_row[[units_col]])
              new_row[[units_col]] <- new_row[[units_col]] + additional_units
              new_row[[value_col]] <- new_row[[value_col]] + contribution_to_symbol
            }
          }
        }
      }
      
      new_row$contribution_to_date <- new_row$contribution_to_date + monthly_contribution
      
      # Recalculate weights after contribution
      total_after_contribution <- sum(sapply(symbols, function(symbol) {
        val <- new_row[[paste0(symbol, "_value")]]
        if (is.null(val) || length(val) == 0) return(0)
        return(val)
      }))
      
      if (total_after_contribution > 0) {
        for (symbol in symbols) {
          weight_col <- paste0(symbol, "_weight")
          value_col <- paste0(symbol, "_value")
          new_row[[weight_col]] <- new_row[[value_col]] / total_after_contribution
        }
      }
    }
    
    # Calculate total portfolio value
    new_row$total_value <- sum(sapply(symbols, function(symbol) {
      val <- new_row[[paste0(symbol, "_value")]]
      if (is.null(val) || length(val) == 0) return(0)
      return(val)
    }))
    
    new_row$equity_weight <- new_row[["VTSMX_weight"]] + new_row[["VGTSX_weight"]] + new_row[["VEIEX_weight"]]
    
    # Add row to portfolio
    portfolio <- rbind(portfolio, new_row)
    
    # Increment portfolio index for next iteration
    portfolio_idx <- portfolio_idx + 1
  }
  
  # Remove debug prints
  options(warn = -1)
  print("Strategic portfolio simulation complete.")
  options(warn = 0)
  
  return(portfolio)
}

# Prepare data
prepared_data <- prepare_data(raw)
print("data prepared")

# Run simulations
run_sim <- function(portfolio_size, monthly_investment){
  
  prorata_portfolio <- simulate_prorata_portfolio(prepared_data$returns, prepared_data$prices, portfolio_size, 0)
  strategic_portfolio <- simulate_strategic_portfolio(prepared_data$returns, prepared_data$prices, portfolio_size, monthly_investment)
  
  results <- prorata_portfolio %>%
    select(date, prorata_value = total_value,
           `Never Rebalance` = equity_weight) %>%
    left_join(
      strategic_portfolio %>%
        select(date, strategic_value = total_value,
               `Accumulation Rebal` = equity_weight),
      by = "date"
    )
  
  to_plot <- results %>%
                select(date, `Never Rebalance`, `Accumulation Rebal`) %>%
                gather(-date, key=key, value=value)
  
  file_path <- paste0(out_path, "/equity_pct_", portfolio_size, "_", monthly_investment, ".jpeg")
  source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: The Accumulation Rebalance Strategy assumes a starting portfolio value of ", format_as_dollar(portfolio_size), 
                                 " and a monthly investment of ", format_as_dollar(monthly_investment), "."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
    geom_line() +
    geom_hline(yintercept = 0.6, linetype = "dashed") +
    of_dollars_and_data_theme +
    scale_y_continuous(label = percent) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Equity Allocations Over Time")) +
    labs(x = "Date", y = "Equity Allocation",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")  
}

run_sim(100000, 1000)
run_sim(10000, 1000)







# ############################  End  ################################## #