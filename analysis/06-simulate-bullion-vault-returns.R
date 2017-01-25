cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

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
library(quadprog)
library(lubridate)
library(fTrading)

# ############################  End  ################################## #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds"))

# Define the number of simulations (this will be used later)
n_simulations <- 10000

# This seed allows us to have reproducible random sampling
set.seed(12345)    

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

# Define the number of years
n_years <- nrow(full_bv_returns)

# Convert year to a date object
full_bv_returns$year <- as.Date(full_bv_returns$year)

melted_returns <- melt(full_bv_returns ,  id.vars = 'year', variable.name = 'asset')

############################### First Returns Plot ###############################  
  # Set the file_path for the output
  file_path = paste0(exportdir, "06-simulate-bv-returns/bv-asset-returns.jpeg")
  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = melted_returns, aes(x = year, y = value, col = asset, fill = asset)) +
    geom_bar(stat = "identity") +
    facet_wrap(~asset) +
    ggtitle("Returns Vary by Asset Class\n1976 - 2015") +
    scale_y_continuous(label = percent, limits = c(-0.5, 1.25)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%y",
                 limits = c(min(full_bv_returns$year), max(full_bv_returns$year))) +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Annual Real Return (%)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 
  
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

  
############################### Second Returns Plot ###############################  
  # Set the file_path for the next output
  file_path = paste0(exportdir, "06-simulate-bv-returns/bv-asset-returns-all.jpeg")
  
  plot <- ggplot(data = melted_returns, aes(x = year, y = value, col = asset)) +
      #geom_bar(stat = "identity", position = "dodge") +
    geom_line() +
    ggtitle("Asset Returns Are Hard to Predict\n1976 - 2015") +
      scale_y_continuous(label = percent, limits = c(-0.5, 1.25), breaks = seq(-0.5, 1.25, .25)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y",
                 limits = c(min(full_bv_returns$year), max(full_bv_returns$year))) +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
      of_dollars_and_data_theme +
    labs(x = "Year" , y = "Annual Return (%)")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index.") 
  
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

############################### Portfolio Solver ###############################  
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
    eff <- matrix(nrow=loops, ncol=n+3)
    # Now I need to give the matrix column names
    colnames(eff) <- c(colnames(returns), "sd", "exp_return", "sharpe")
    
    # Loop through the quadratic program solver
    for (i in seq(from = 0, to = risk_premium_upper_limit, by = risk_increment)){
      dvec                   <- colMeans(returns) * i # This moves the solution up along the efficient frontier
      sol                    <- solve.QP(cov_matrix, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
      eff[loop,"sd"]         <- sqrt(sum(sol$solution * colSums((cov_matrix * sol$solution))))
      eff[loop,"exp_return"] <- as.numeric(sol$solution %*% colMeans(returns))
      eff[loop,"sharpe"]     <- (eff[loop,"exp_return"] - avg_rf) / eff[loop,"sd"]
      eff[loop,1:n]          <- sol$solution
      loop <- loop+1
    }
    
    return(as.data.frame(eff))
  }
  
  eff <- eff_frontier(returns=returns, short = "no", max_allocation = .33, risk_premium_upper_limit = .5, risk_increment = .001)

############################### Efficient Frontier Plot ###############################    
  # Plot the efficient frontier
  eff_optimal_point <- eff[eff$sharpe == max(eff$sharpe),]
  
  # Color Scheme
  ealred  <- "#7D110C"
  ealtan  <- "#CDC4B6"
  eallighttan <- "#F7F6F0"
  ealdark  <- "#423C30"
  plot <- ggplot(eff, aes(x = sd, y = exp_return)) + geom_point(alpha = .1, color = ealdark) +
    geom_point(data = eff_optimal_point, aes(x = sd, y = exp_return), color = ealred, size=5) +
    annotate(geom="text", x = eff_optimal_point$sd, y = eff_optimal_point$exp_return, family = "my_font",
             label=paste("Real Return: ", round(eff_optimal_point$exp_return * 100, digits = 2),
                         "%\nRisk: ", round(eff_optimal_point$sd * 100, digits = 2),"%\nSharpe: ",
                         round(eff_optimal_point$sharpe * 100, digits = 2), "%", sep=""), hjust=0, vjust=1.2) +
    ggtitle(paste0("Efficient Frontier and Optimal Portfolio\n")) + labs(x = "Risk (standard deviation of portfolio variance)", y ="Real Return") +
    of_dollars_and_data_theme +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = percent)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "06-simulate-bv-returns/bv-efficient-frontier.jpeg")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  BullionVault U.S. Asset Class Performance Data, ", min_year, "-", max_year," (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  Assumes no asset can be >33% of the portfolio and shorting is not allowed.") 
  
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

############################### Simulate the Value Paths ###############################    
  # Simulate the portfolio value
  # Create a simulation vector
  sim_vec <- seq(1, n_years, 1)
  
  # Drop unneeded columns
  optimal_weights <- as.data.frame((eff_optimal_point[1:n_assets]))
  
  # Round any weights less than 0.05% to zero
  optimal_weights <- t(apply(optimal_weights[,], 2, function(x) ifelse(x < 0.0005, 0, x)))
  
  # Initialize all matrices used for returns and value paths
  sampled_years_matrix    <- matrix(NA, nrow = n_simulations, ncol = n_years)
  sampled_returns         <- matrix(NA, nrow = n_simulations, ncol = n_assets)
  portfolio_return_matrix <- matrix(NA, nrow = n_simulations, ncol = n_years)
  value_matrix            <- matrix(NA, nrow = n_simulations, ncol = n_years)
  
  # Setup a yearly cash addition into the portfolio.  
  # This cash addition happens at the beginning of each return year
  yearly_cash_add <- 5000 
  
  returns_for_simulation <- full_bv_returns[, -which(names(full_bv_returns) %in% c("year", "tbill_3m"))]
  
  # Sample returns only once if you want to do sensitivities
  for (i in 1:n_years){
    sampled_years_matrix[, i]    <- sample(sim_vec, n_simulations, replace = TRUE)
  }
  
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

############################### Plot the Drawdowns ###############################  
  # Define the number of periods for plotting
  periods <- seq(1, n_years)
  
  # Plot a subset of the simulations to see the differences that certain weightings have.
  to_plot <- as.data.frame(t(value_matrix))
  to_plot <- as.data.frame(cbind(rowMeans(to_plot), periods))
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "06-simulate-bv-returns/bv-drawdowns.jpeg")
  
  top_title <- ""
    
  # Plot the fund capital over client capital
  # Each colored line is its own simulation
  plot <- ggplot()
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Simulated returns, BullionVault (OfDollarsAndData.com)"
  note_string <- paste0("Note:  Assumes annual investment of $", formatC(yearly_cash_add, format="d", big.mark=','),".") 
  
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

############################### Calculate Useful Stats ###############################    
  # Calculate end values and invested capital
  total_invested_capital <- n_years * yearly_cash_add
  max_end_value <- max(value_matrix[, n_years])
  min_end_value <- min(value_matrix[, n_years])
  median_end_value <- quantile(value_matrix[, n_years], probs = 0.5)
  
  # Caluclate the maximum drawdown for each simulation
  max_drawdown <- 0
  max_drawdown_pct_matrix      <- matrix(NA, nrow = n_simulations, ncol = 1)
  max_drawdown_dollar_matrix   <- matrix(NA, nrow = n_simulations, ncol = 1)
  
  for (x in 1:n_simulations){
    drawdown <- maxDrawDown(value_matrix[x,])$maxdrawdown
    from <- maxDrawDown(value_matrix[x,])$from
    to <- maxDrawDown(value_matrix[x,])$to
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
    type <- deparse(substitute(name))
    matrix <- get(paste0("max_drawdown_", type, "_matrix"))
    max    <- max(matrix)
    min    <- min(matrix)
    median <- quantile(matrix, probs = 0.5)
    assign(paste0("max_drawdown_", type), max)
    assign(paste0("min_drawdown_", type), min)
    assign(paste0("median_drawdown_", type), median)
    if (type == "pct"){
    print(paste0("Maximum drawdown: ", max*100, "%"))
    print(paste0("Minimum drawdown: ", min*100, "%"))
    print(paste0("Median drawdown: ", median*100, "%"))
    } else if (type == "dollar"){
      print(paste0("Maximum drawdown: $", max))
      print(paste0("Minimum drawdown: $", min))
      print(paste0("Median drawdown: $", median))
    }
  }
  
  calculate_drawdown(pct)
  calculate_drawdown(dollar)
  
  # Print other summary stats as well
  print(paste0("Total invested capital: $", total_invested_capital))
  print(paste0("Maximum Ending Value: $", max_end_value))
  print(paste0("Minimum Ending Value: $", min_end_value))
  print(paste0("Median Ending Value: $", median_end_value))
