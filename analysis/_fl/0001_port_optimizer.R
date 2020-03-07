cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(quadprog)
library(lubridate)
library(fTrading)
library(tidyverse)

folder_name <- "/_fl/0001_port_optimizer"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in BV returns
full_bv_returns <- readRDS(paste0(localdir, "0006_bv_returns.Rds")) %>%
                      mutate(year = as.Date(year, "%d/%m/%y")) %>%
                      rename(`TBill 3m` = `Tbill 3m`)

min_year <- min(year(full_bv_returns$year))
max_year <- max(year(full_bv_returns$year))

# Define the number of years
n_years <- nrow(full_bv_returns)

melted_returns <- full_bv_returns %>%
                    gather(-year, key=asset, value=value)

############################### First Returns Plot ###############################  
  # Set the file_path for the output
  file_path = paste0(out_path, "/bv_asset_returns.jpeg")

  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)"),
                            width = 85)
  note_string   <- str_wrap(paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index."),
                            width = 85)

  
  # Plot the returns to show how much they change over time
  plot <- ggplot(data = melted_returns, aes(x = year, y = value, col = asset, fill = asset)) +
    geom_bar(stat = "identity") +
    facet_wrap(~asset) +
    ggtitle(paste0("Returns Vary by Asset Class\n", min_year, "-", max_year)) +
    scale_y_continuous(label = percent, limits = c(-0.5, 1.25)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%y") +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "Annual Real Return (%)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
############################### Plot Risk v. Return ###############################   
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/bv_risk_return.jpeg")
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)"),
                            width = 85)
  note_string   <- str_wrap(paste0("Note:  Returns are adjusted using the U.S. Consumer Price Index."),
                            width = 85)
  
  to_plot <- melted_returns %>%
              group_by(asset) %>%
              summarize(ret = mean(value),
                        sd = sd(value)) %>%
              ungroup()
  
  plot <- ggplot(data=to_plot, aes(x = sd, y = ret, col = asset)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_text(data=to_plot, aes(x=sd, y=ret, col=asset, label=asset), vjust = -1) +
          scale_color_discrete(guide = FALSE) +
          scale_x_continuous(label = percent, limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
          scale_y_continuous(label = percent, limits = c(-0.05, .10), breaks = seq(-0.05, 0.10, 0.05)) +
          of_dollars_and_data_theme +
          ggtitle(paste0("Risk vs. Return")) + 
          labs(x = "Risk (standard deviation)", y ="Annual Real Return",
               caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
############################### Optimal Portfolio Solver ###############################  
  
  # Drop the year and the risk free rate from the data so we can optimize on the remaining assets
  returns <- full_bv_returns[, -which(names(full_bv_returns) %in% c("year", "TBill 3m"))]
  
  n_assets <- ncol(returns)
  
  avg_rf  <- colMeans(full_bv_returns[, "TBill 3m"])
  
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
  all_reit         <- data.frame(matrix(nrow = 1, ncol = 0))
  all_10yr         <- data.frame(matrix(nrow = 1, ncol = 0))
    
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
  
  # All REIT  
  for (j in colnames(eff[1:n_assets])){
    if (j == "Treasury 10yr"){
      all_10yr[j] <- 1
    } else{
      all_10yr[j] <- 0
    }
  }  
  
  # All 10yr  
  for (j in colnames(eff[1:n_assets])){
    if (j == "REIT"){
      all_reit[j] <- 1
    } else{
      all_reit[j] <- 0
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
  all_reit         <- find_ret_sd_sharpe(all_reit)
  all_10yr         <- find_ret_sd_sharpe(all_10yr)
  
############################### Efficient Frontier Plot ###############################    
  # Plot the efficient frontier
  optimal <- eff[eff$sharpe == max(eff$sharpe),]
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/bv_efficient_frontier.jpeg")
  
  # Add a source and note string for the plots
  source_string <- str_wrap(paste0("Source:  BullionVault, ", min_year, "-", max_year," (OfDollarsAndData.com)"),
                            width = 85)
  note_string   <- str_wrap(paste0("Note:  Assumes no asset can be >", max_alloc*100 ,"% of the portfolio and shorting is not allowed."),
                            width = 85)
  
  # Color Scheme
  color_optimal  <- "#7D110C"
  color_sp500  <- "green"
  color_reit <- "cyan"
  color_10yr <- chart_standard_color
  color_5050 <- "blue"
  color_gold  <- "#FFD700"
  color_ew <- "purple"
  color_dark <- "#423C30"
  
  plot <- ggplot(eff, aes(x = sd, y = exp_return)) + geom_point(alpha = .1, color = color_dark) +
    # Add optimal point
    geom_point(data = optimal, aes(x = sd, y = exp_return), color = color_optimal, size = 5) +
    geom_text_repel(data = optimal, label = "Optimal Portfolio", family = "my_font", size = 3.5, nudge_x = -0.02, nudge_y = 0.009, max.iter = 5000, segment.color = "transparent") +
    # Add S&P 500 only
    geom_point(data = all_stock, aes(x = sd, y = exp_return), color = color_sp500, size = 2) +
    geom_text_repel(data = all_stock, label = "S&P 500 Only", family = "my_font", size = 3, nudge_y = -0.004, max.iter = 5000, segment.color = "transparent") +
    # Add 50-50 portfolio
    geom_point(data = stock_bond_50_50, aes(x = sd, y = exp_return), color = color_5050, size = 2) +
    geom_text_repel(data = stock_bond_50_50, label = "50-50 Stock/Bond", family = "my_font", size = 3, nudge_x = 0.03, max.iter = 5000, segment.color = "transparent") +
    # Add all Gold
    geom_point(data = all_gold, aes(x = sd, y = exp_return), color = color_gold, size = 2) +
    geom_text_repel(data = all_gold, label = "Gold Only", family = "my_font", size = 3, nudge_x = -0.015, max.iter = 5000, segment.color = "transparent") +
    # Add all REIT
    geom_point(data = all_reit, aes(x = sd, y = exp_return), color = color_reit, size = 2) +
    geom_text_repel(data = all_reit, label = "REITs Only", family = "my_font", size = 3, nudge_x = -0.015, max.iter = 5000, segment.color = "transparent") +
    # Add Equal weighted portfolio
    geom_point(data = equal_weighted, aes(x = sd, y = exp_return), color = color_ew, size = 2) +
    geom_text_repel(data = equal_weighted, label = "Equal Weighted", family = "my_font", size = 3, nudge_y = -0.004, nudge_x = 0.002, max.iter = 5000, segment.color = "transparent") +
    # Add all 10yr
    geom_point(data = all_10yr, aes(x = sd, y = exp_return), color = color_10yr, size = 2) +
    geom_text_repel(data = all_10yr, label = "Treasury 10yr\nOnly", family = "my_font", size = 3, nudge_y = -0.004, nudge_x = 0.002, max.iter = 5000, segment.color = "transparent") +
    ggtitle(paste0("Efficient Frontier and Optimal Portfolio\n")) + 
    labs(x = "Risk (standard deviation)", y ="Real Return",
         caption = paste0(source_string, "\n", note_string)) +
    of_dollars_and_data_theme +
    scale_x_continuous(label = percent, limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
    scale_y_continuous(label = percent, limits = c(0, .10), breaks = seq(0, 0.10, 0.02))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
# ############################  End  ################################## #

  
