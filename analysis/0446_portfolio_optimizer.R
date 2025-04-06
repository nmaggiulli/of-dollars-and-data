cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(quadprog)
library(lubridate)
library(fTrading)
library(tidyverse)

folder_name <- "0446_portfolio_optimizer"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "0006_bv_returns.Rds"))

# Define the number of simulations (this will be used later)
n_simulations <- 1000

# This seed allows us to have reproducible random sampling
set.seed(12345)    

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

# Define the number of years
n_years <- nrow(full_bv_returns)
note_width <- 95

melted_returns <- melt(full_bv_returns ,  id.vars = 'year', variable.name = 'asset')

to_plot <- melted_returns %>%
              mutate(value = case_when(
                value > 0.5 ~ 0.5,
                value < -0.5 ~ -0.5,
                TRUE ~ value
              ))

############################### First Returns Plot ###############################  
  # Set the file_path for the output
  file_path <- paste0(out_path, "/bv_asset_returns_", min_year, "_", max_year,".jpeg")

  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- str_wrap(paste0("Note:  Returns are adjusted for inflation using the U.S. Consumer Price Index. Annual returns greater than 50% (or less than -50%) have been capped at 50% (or -50%) for visual simplcity."),
                            width = note_width)

  # Plot the returns to show how much they change over time
  plot <- ggplot(data = to_plot, aes(x = year, y = value, col = asset, fill = asset)) +
    geom_bar(stat = "identity") +
    facet_wrap(~asset) +
    ggtitle(paste0("Returns Vary by Asset Class\n", min_year, "-", max_year)) +
    scale_y_continuous(label = percent, limits = c(-0.5, 0.5)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%y",
                 limits = c(min(full_bv_returns$year), max(full_bv_returns$year))) +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Annual Real Return (%)",
         caption = paste0(source_string, "\n", note_string))

  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")

  
############################### Second Returns Plot ###############################  
  # Set the file_path for the next output
  file_path <- paste0(out_path, "/bv_asset_returns_all_", min_year, "_", max_year, ".jpeg")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted for inflation using the U.S. Consumer Price Index.") 
  
  plot <- ggplot(data = melted_returns, aes(x = year, y = value, col = asset)) +
      #geom_bar(stat = "identity", position = "dodge") +
    geom_line() +
    ggtitle(paste0("Asset Returns Are Hard to Predict\n", min_year, "-", max_year)) +
      scale_y_continuous(label = percent, limits = c(-0.55, 1.25), breaks = seq(-0.5, 1.25, .25)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y",
                 limits = c(min(full_bv_returns$year), max(full_bv_returns$year))) +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
      of_dollars_and_data_theme +
    labs(x = "Year" , y = "Annual Real Return (%)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")

############################### Optimal Portfolio Solver ###############################  
  
  # Drop the year and the risk free rate from the data so we can optimize on the remaining assets
  returns <- full_bv_returns[, -which(names(full_bv_returns) %in% c("year", "Tbill 3m"))]
  
  n_assets <- ncol(returns)
  
  avg_rf  <- colMeans(full_bv_returns[, "Tbill 3m"])
  
  eff_frontier <- function (returns, short = "no", max_allocation = NULL, risk_premium_upper_limit = .5, risk_increment = .005){
    # return argument should be a m x n matrix with one column per security
    # short argument is whether short-selling is allowed; default is no (short selling prohibited)
    # max.allocation is the maximum % allowed for any one security (reduces concentration)
    # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
    # risk.increment is the increment (by) value used in the for loop
  
    # Create the covariance of returns
    cov_matrix <- cov(returns)
    n          <- ncol(cov_matrix)
    
    # Create initial Amat and bvec assuming only equality constraint is that weight >= 0
    Amat <- matrix (1, nrow = n)
    bvec <- 1
    meq <- 1
    
    # Then modify the Amat and bvec if short-selling is prohibited
    if(short == "no"){
      Amat <- cbind(1, diag(n))
      bvec <- c(bvec, rep(0, n))
    }
    
    # And modify Amat and bvec if a max allocation (concentration) is specified
    if(!is.null(max_allocation)){
      if(max_allocation > 1 | max_allocation <0){
        stop("max.allocation must be greater than 0 and less than 1")
      }
      if(max_allocation * n < 1){
        stop("Need to set max_allocation higher; not enough assets to add to 1")
      }
      Amat <- cbind(Amat, -diag(n))
      bvec <- c(bvec, rep(-max_allocation, n))
    }
    
    # Calculate the number of loops based on how high to vary the risk premium and by what increment
    loops <- risk_premium_upper_limit / risk_increment + 1
    loop  <- 1
    
    # Initialize a matrix to contain allocation and statistics
    # This is not necessary, but speeds up processing and uses less memory
    eff <- matrix(nrow = loops, ncol = n + 3)
    # Now I need to give the matrix column names
    colnames(eff) <- c(colnames(returns), "sd", "exp_return", "sharpe")
    
    # Loop through the quadratic program solver
    for (i in seq(from = 0, to = risk_premium_upper_limit, by = risk_increment)){
      dvec                   <- colMeans(returns) * i # This moves the solution up along the efficient frontier
      sol                    <- solve.QP(cov_matrix, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
      utc                    <- upper.tri(cov_matrix)
      wt_var                 <- sum(diag(cov_matrix) * sol$solution^2)
      wt_cov                 <- sum(sol$solution[row(cov_matrix)[utc]] *
                                      sol$solution[col(cov_matrix)[utc]] *
                                     cov_matrix[utc])
      eff[loop,"sd"]         <- sqrt(wt_var + 2 * wt_cov)
     #eff[loop,"sd"]         <- sqrt(sum(sol$solution * colSums((cov_matrix * sol$solution))))
      eff[loop,"exp_return"] <- as.numeric(sol$solution %*% colMeans(returns))
      eff[loop,"sharpe"]     <- (eff[loop,"exp_return"] - avg_rf) / eff[loop,"sd"]
      eff[loop,1:n]          <- sol$solution
      loop <- loop+1
    }
    
    return(as.data.frame(eff))
  }
  
  # Set the maximum allocation for any one asset
  # 1 means that any one asset can comprise the entire portfolio
  max_alloc <- 1
  
  eff <- eff_frontier(returns=returns, short = "no", max_allocation = max_alloc, risk_premium_upper_limit = 0.5, risk_increment = .001)

############################### Create Additional Portfolios ###############################   

  # Initialize all weights as empty data frames
  stock_bond_50_50 <- data.frame(matrix(nrow = 1, ncol = 0))
  equal_weighted   <- data.frame(matrix(nrow = 1, ncol = 0))
  all_stock        <- data.frame(matrix(nrow = 1, ncol = 0))
  all_gold         <- data.frame(matrix(nrow = 1, ncol = 0))
    
  # Stock + Bond 50-50  
  for (j in colnames(eff[1:n_assets])){
    if (j == "S&P 500" | j == "Treasury 10yr"){
      stock_bond_50_50[j] <- 0.5
    } else{
      stock_bond_50_50[j] <- 0
    }
  }
    
  # Equal Weighted 
  for (j in colnames(eff[1:n_assets])){
    equal_weighted[j] <- 1/n_assets
  }
  
  # All U.S. Stock  
  for (j in colnames(eff[1:n_assets])){
    if (j == "S&P 500"){
      all_stock[j] <- 1
    } else{
      all_stock[j] <- 0
    }
  }  
  
  # All Gold  
  for (j in colnames(eff[1:n_assets])){
    if (j == "Gold"){
      all_gold[j] <- 1
    } else{
      all_gold[j] <- 0
    }
  }  
  
  find_ret_sd_sharpe <- function(df){
    cov_matrix <- cov(returns)
    utc <- upper.tri(cov_matrix)
    wt_var <- sum(diag(cov_matrix) * df^2)
    wt_cov <- sum(df[row(cov_matrix)[utc]] *
                    df[col(cov_matrix)[utc]] *
                    cov_matrix[utc])
    df$sd <- sqrt(wt_var + 2 * wt_cov)
    df$exp_return <- as.numeric(sum(t(df[1:n_assets]) * colMeans(returns)))
    df$sharpe     <- (df$exp_return - avg_rf) / df$sd
    return(df)
  }
  
  stock_bond_50_50 <- find_ret_sd_sharpe(stock_bond_50_50)
  equal_weighted   <- find_ret_sd_sharpe(equal_weighted)
  all_stock        <- find_ret_sd_sharpe(all_stock)
  all_gold         <- find_ret_sd_sharpe(all_gold)
  
############################### Efficient Frontier Plot ###############################    
  # Plot the efficient frontier
  optimal <- eff[eff$sharpe == max(eff$sharpe),]
  
  # Color Scheme
  ealred  <- "#7D110C"
  ealtan  <- "#CDC4B6"
  eallighttan <- "#F7F6F0"
  ealdark  <- "#423C30"
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes no asset can be >", max_alloc*100 ,"% of the portfolio and shorting is not allowed.") 
  
  plot <- ggplot(eff, aes(x = sd, y = exp_return)) + geom_point(alpha = .1, color = ealdark) +
    # Add optimal point
    geom_point(data = optimal, aes(x = sd, y = exp_return), color = ealred, size = 5) +
    geom_text_repel(data = optimal, label = "Optimal Portfolio", family = "my_font", size = 3.5, nudge_x = -0.02, nudge_y = 0.009, max.iter = 5000) +
    # Add S&P 500 only
    geom_point(data = all_stock, aes(x = sd, y = exp_return), color = "green", size = 2) +
    geom_text_repel(data = all_stock, label = "S&P 500 Only", family = "my_font", size = 3, nudge_y = -0.004, max.iter = 5000) +
    # Add 50-50 portfolio
    geom_point(data = stock_bond_50_50, aes(x = sd, y = exp_return), color = "blue", size = 2) +
    geom_text_repel(data = stock_bond_50_50, label = "50-50 Stock/Bond", family = "my_font", size = 3, nudge_x = 0.03, max.iter = 5000) +
    # Add all Gold
    geom_point(data = all_gold, aes(x = sd, y = exp_return), color = "#FFD700", size = 2) +
    geom_text_repel(data = all_gold, label = "Gold Only", family = "my_font", size = 3, nudge_x = -0.015, max.iter = 5000) +
    # Add Equal weighted portfolio
    geom_point(data = equal_weighted, aes(x = sd, y = exp_return), color = "purple", size = 2) +
    geom_text_repel(data = equal_weighted, label = "Equal Weighted", family = "my_font", size = 3, nudge_y = -0.004, nudge_x = 0.002, max.iter = 5000) +
    ggtitle(paste0("Efficient Frontier and Optimal Portfolio\n")) + labs(x = "Risk (standard deviation of portfolio variance)", y ="Real Return", caption = paste0(source_string, "\n", note_string)) +
    of_dollars_and_data_theme +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = percent, limits = c(0.02, .10), breaks = seq(0.02, 0.10, 0.02))
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/bv_efficient_frontier_", min_year, "_", max_year,".jpeg")

  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")

############################### Simulate the Value Paths ###############################  
  
  # Setup a yearly cash addition into the portfolio.  
  # This cash addition happens at the beginning of each return year
  yearly_cash_add <- 5000 
  
  # Create a simulation vector
  sim_vec <- seq(1, n_years, 1)
  
  # Initialize a matrix for the sampled years
  sampled_years_matrix    <- matrix(NA, nrow = n_simulations, ncol = n_years)
  
  # Sample returns only once if you want to do sensitivities
  for (i in 1:n_years){
    sampled_years_matrix[, i]    <- sample(sim_vec, n_simulations, replace = TRUE)
  }

  # Make a function to simulate the value path of the portfolio
  simulate_value_paths <- function(weights){
    wts <- get(weights)
    
    # Drop unneeded columns
    optimal_weights <- as.data.frame((wts[1:n_assets]))

    # Round any weights less than 0.05% to zero
    optimal_weights <- t(apply(optimal_weights[,], 2, function(x) ifelse(x < 0.0005, 0, x)))
    
    # Initialize all matrices used for returns and value paths
    sampled_returns         <- matrix(NA, nrow = n_simulations, ncol = n_assets)
    portfolio_return_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years)
    value_matrix            <- matrix(NA, nrow = n_simulations, ncol = n_years)
    
    returns_for_simulation <- full_bv_returns[, -which(names(full_bv_returns) %in% c("year", "Tbill 3m"))]
    
    # Simulate the value paths now that you have the sampled return year indices
    # Make sure to grab the whole year so the correlations are preserved
    for (i in 1:n_years){
      for (j in 1:n_assets){
        sampled_returns[, j] <- 1 + unlist(returns_for_simulation[sampled_years_matrix[,i], j])
      }
      portfolio_return_matrix[, i] <- rowSums(t(as.vector(optimal_weights) * t(sampled_returns)))
      if (i == 1){
        value_matrix[, i] <- yearly_cash_add * (portfolio_return_matrix[ , i])
      } else {
        value_matrix[, i] <- (value_matrix[, i - 1] + yearly_cash_add) * (portfolio_return_matrix[, i])
      }
    }
    return(value_matrix)
  }
  
  # Create a portfolio list to loop through
  portfolio_list <- c("optimal", 
                      "all_stock", 
                      "stock_bond_50_50", 
                      "equal_weighted",
                      "all_gold")
  
  # Simulate the value paths for all portfolios in list
  for (n in portfolio_list){
    vp <- simulate_value_paths(n)
    assign(paste0("vp_", n), vp, envir = .GlobalEnv)
    rm(vp)
  }
  
############################### Calculate Useful Stats ###############################  
    
  # Initialize results data.frame and a counter for the rows
  results_df <- data.frame(matrix(nrow = length(portfolio_list), ncol = 0))
  i <- 1
    
  # Calculate end values and invested capital
  for (n in portfolio_list){
    value_matrix                            <- get(paste0("vp_",n))
    results_df[i, "portfolio"]              <- n
    results_df[i, "total_invested_capital"] <- n_years * yearly_cash_add
    results_df[i, "min_end_value"]          <- min(value_matrix[, n_years])
    results_df[i, "median_end_value"]       <- quantile(value_matrix[, n_years], probs = 0.5)
    results_df[i, "max_end_value"]          <- max(value_matrix[, n_years])
    
    # Caluclate the maximum drawdown for each simulation
    max_drawdown <- 0
    max_drawdown_pct_matrix      <- matrix(NA, nrow = n_simulations, ncol = 1)
    max_drawdown_dollar_matrix   <- matrix(NA, nrow = n_simulations, ncol = 1)
    
    for (x in 1:n_simulations){
      drawdown <- maxDrawDown(value_matrix[x,])$maxdrawdown
      from     <- maxDrawDown(value_matrix[x,])$from
      to       <- maxDrawDown(value_matrix[x,])$to
      if (drawdown > 0){
        max_drawdown_pct_matrix[x, 1]    <- drawdown / value_matrix[x, from]
        max_drawdown_dollar_matrix[x, 1] <- drawdown
      } else{
        max_drawdown_pct_matrix[x, 1]    <- 0
        max_drawdown_dollar_matrix[x, 1] <- drawdown
      }
    }
    
    # Calculate summary statistics on the max, min, and median drawdowns
    calculate_drawdown <- function(name){
      results <- get("results_df")
      type <- deparse(substitute(name))
      matrix <- get(paste0("max_drawdown_", type, "_matrix"))
      minname <- paste0("min_drawdown_", type)
      medianname <- paste0("median_drawdown_", type)
      maxname <- paste0("max_drawdown_", type)
      results[i, minname]    <- min(matrix)
      results[i, medianname] <- quantile(matrix, probs = 0.5)
      results[i, maxname]    <- max(matrix)
      assign("results_df", results, envir = .GlobalEnv)
    }

    calculate_drawdown(dollar)
    calculate_drawdown(pct)
    i <- i + 1
  }
  
  write.csv(results_df, paste0(out_path,"/portfolio_stats_", min_year, "_", max_year,".csv"))
  
#Final plot
to_plot <- melted_returns %>%
            filter(asset %in% c("S&P 500", "REIT", "Treasury 10yr", "Gold", "U.S. Home Price")) %>%
            mutate(pos = ifelse(value > 0, 1, 0)) %>%
            group_by(year) %>%
            summarise(n_neg = 5 - sum(pos)) %>%
            ungroup()

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/neg_returns_op_portfolio_", min_year, "_", max_year, ".jpeg")
source_string <- "Source:  Simulated returns, BullionVault (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x = year, y = n_neg)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("At Least One Asset in the Optimal Portfolio\nLoses Money in 87% of All Years")) +
  labs(x = "Year" , y = "Number of Assets with a Negative Annual Return",
       caption = paste0(source_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Do growth of dollar of optimal portfolio versus 
select_cols <- c(2:6, 8:10)
data_matrix <- as.matrix(full_bv_returns[, select_cols])

# Extract weights from optimal data frame
weights_vector <- as.numeric(optimal[1, 1:8])

# Perform the matrix multiplication
full_bv_returns$optimal <- data_matrix %*% weights_vector
        
growth_of_dollar <- data.frame(year = full_bv_returns$year)  

for(i in 1:nrow(full_bv_returns)){
  if(i == 1){
    growth_of_dollar[i, "port_sp500"] <- 1
    growth_of_dollar[i, "port_optimal"] <- 1
  } else{
    growth_of_dollar[i, "port_sp500"] <- growth_of_dollar[(i-1), "port_sp500"]*(1+full_bv_returns[i, "S&P 500"])
    growth_of_dollar[i, "port_optimal"] <- growth_of_dollar[(i-1), "port_optimal"]*(1+full_bv_returns[i, "optimal"])
  }
}

to_plot <- growth_of_dollar %>%
              rename(`S&P 500` = port_sp500,
                     `Optimal Portfolio` = port_optimal) %>%
            gather(-year, key=key, value=value)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/growth_of_dollar_log_optimal_", min_year, "_", max_year, ".jpeg")
source_string <- "Source:  Simulated returns, BullionVault (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Returns are adjusted for inflation using the U.S. Consumer Price Index.") 

plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  scale_color_manual(values = c("black", "green")) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Growth of $1\nS&P 500 vs. Optimal Portfolio")) +
  labs(x = "Year" , y = "Portfolio Value (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do bar plot of annual returns
to_plot <- full_bv_returns %>%
            select(year, `S&P 500`, optimal) %>%
              rename(`Optimal Portfolio` = optimal) %>%
  gather(-year, key=key, value=value)

file_path <- paste0(out_path, "/annual_rets_optimal_", min_year, "_", max_year, ".jpeg")
source_string <- "Source:  Simulated returns, BullionVault (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Returns are adjusted for inflation using the U.S. Consumer Price Index.") 

plot <- ggplot(to_plot, aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "green")) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Annual Returns\nS&P 500 vs. Optimal Portfolio")) +
  labs(x = "Year" , y = "Annual Real Return",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")