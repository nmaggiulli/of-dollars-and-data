cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

library(ggplot2)
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
library(quantmod)
library(tidyr)
library(dplyr)

# ############################  End  ################################## #

# Load in Bitcoin data and then scrape the other data from Yahoo Finance
bcoin <- readRDS(paste0(localdir, "27-quandl-bitcoin.Rds")) %>%
          mutate(day = day(date),
                 asset = "Bitcoin") %>%
          filter(day == 1) %>%
          select(date, value, asset)

first_year <- min(year(bcoin$date))
last_year <- max(year(bcoin$date))

bcoin_date <- min(bcoin$date)

# REIT, S&P 500, EM, Gold, Int. Markets, Total Bond, Commodities
tickers     <- c("VGSIX", "SPY", "VWO", "GLD", "VTMGX", "VBLTX", "VAW", "VBISX")
asset_class <- c("U.S. REIT", "U.S. Equities", "Emerging Markets", "Gold", "Int. Equities", "U.S. Long-Term Bond", "Commodities", "U.S. Short-Term Bond")


for (a in 1:length(tickers)){
  getSymbols(tickers[a], from=bcoin_date, src="yahoo", periodicity = "monthly")
  df <- data.frame(date = index(get(tickers[a])), 
                   get(tickers[a]), row.names=NULL) %>%
            select(date, contains("Adjust")) 
  names(df) <- c("date", "value")
  df[, "asset"] <- asset_class[a]
  if (a == 1){
    all_prices <- bind_rows(bcoin, df)
  } else {
    all_prices <- bind_rows(all_prices, df)
  }
}

for (i in 2:nrow(all_prices)){
  if(all_prices[i, "asset"] == all_prices[(i - 1), "asset"]){
    all_prices[i, "ret"] <- (all_prices[i, "value"]/all_prices[(i-1), "value"] - 1)
  } else {
    all_prices[i, "ret"] <- NA
  }
}

all_prices %<>% mutate(value = ret) %>%
                select(-ret)


############################### First Returns Plot ###############################  
  # Set the file_path for the output
  file_path = paste0(exportdir, "43-bitcoin-optimizer/asset-returns.jpeg")
  
  max_y <- 0.5
  
  count_above_max <- nrow(filter(all_prices, value > max_y))

  # Plot the returns to show how much they change over time
  plot <- ggplot(data = all_prices, aes(x = date, y = value, col = asset, fill = asset)) +
    geom_bar(stat = "identity") +
    facet_wrap(~asset) +
    ggtitle(paste0("Bitcoin's Volatility is Off the Charts\n", first_year, "-", last_year)) +
    scale_y_continuous(label = percent, limits = c(-0.5, max_y)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%y",
                 limits = c(min(all_prices$date), max(all_prices$date))) +
    scale_color_discrete(guide = FALSE) +
    scale_fill_discrete(guide = FALSE) +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "1-Month Nominal Return")
  
  # Add a source and note string for the plots
  source_string <- paste0("Source:  Quandl, Yahoo Finance (OfDollarsAndData.com)")
  note_string   <- paste0("Note:  ", count_above_max, " monthly returns have been excluded for Bitcoin because they were greater than ", max_y*100, "%.") 
  
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


# Create a wide return series for the optimizer  
wide_returns <- all_prices %>%
                  spread(key=asset, value=value) %>%
                  filter(!is.na(Bitcoin))


############################### Optimal Portfolio Solver ###############################  
  
  # Drop the year and the risk free rate from the data so we can optimize on the remaining assets
  returns <- wide_returns[, -which(names(all_prices) %in% c("date", "U.S. Short-Term Bond"))]
  
  n_assets <- ncol(returns)
  
  avg_rf  <- mean(wide_returns[, "U.S. Short-Term Bond"])
  
  eff_frontier <- function (returns, short = "no", max_allocation, risk_premium_upper_limit, risk_increment){
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

  # Find the optimal portfolio
  optimal <- eff[eff$sharpe == max(eff$sharpe),]
  
  
############################### Create Additional Portfolios ###############################   

  # Initialize all weights as empty data frames
  all_gold <- data.frame(matrix(nrow = 1, ncol = 0))
  equal_weighted   <- data.frame(matrix(nrow = 1, ncol = 0))
  all_stock        <- data.frame(matrix(nrow = 1, ncol = 0))
  all_btc         <- data.frame(matrix(nrow = 1, ncol = 0))
    
  # Stock + Bond 50-50  
  for (j in colnames(eff[1:n_assets])){
    if (j == "Gold"){
      all_gold[j] <- 1
    } else{
      all_gold[j] <- 0
    }
  }
    
  # Equal Weighted 
  for (j in colnames(eff[1:n_assets])){
    equal_weighted[j] <- 1/n_assets
  }
  
  # All U.S. Stock  
  for (j in colnames(eff[1:n_assets])){
    if (j == "U.S. Equities"){
      all_stock[j] <- 1
    } else{
      all_stock[j] <- 0
    }
  }  
  
  # All Bitcoin 
  for (j in colnames(eff[1:n_assets])){
    if (j == "Bitcoin"){
      all_btc[j] <- 1
    } else{
      all_btc[j] <- 0
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
  
  all_gold        <- find_ret_sd_sharpe(all_gold)
  equal_weighted  <- find_ret_sd_sharpe(equal_weighted)
  all_stock       <- find_ret_sd_sharpe(all_stock)
  all_btc         <- find_ret_sd_sharpe(all_btc)
  
############################### Efficient Frontier Plot ###############################    
  # Plot the efficient frontier
  
  # Color Scheme
  ealred  <- "#7D110C"
  ealtan  <- "#CDC4B6"
  eallighttan <- "#F7F6F0"
  ealdark  <- "#423C30"
  plot <- ggplot(eff, aes(x = sd, y = exp_return)) + geom_point(alpha = .1, color = ealdark) +
    # Add optimal point
    geom_point(data = optimal, aes(x = sd, y = exp_return), color = ealred, size = 5) +
    geom_text_repel(data = optimal, label = "Optimal Portfolio", family = "my_font", size = 3.5, nudge_x = -0.02, nudge_y = 0.009, max.iter = 5000) +
    # Add S&P 500 only
    #geom_point(data = all_stock, aes(x = sd, y = exp_return), color = "green", size = 2) +
    #geom_text_repel(data = all_stock, label = "S&P 500 Only", family = "my_font", size = 3, nudge_y = -0.004, max.iter = 5000) +
    # Add All Gold portfolio
    geom_point(data = all_gold, aes(x = sd, y = exp_return), color = "gold", size = 2) +
    geom_text_repel(data = all_gold, label = "Gold Only", family = "my_font", size = 3, nudge_x = 0.03, max.iter = 5000) +
    # Add all Bitcoin
    geom_point(data = all_btc, aes(x = sd, y = exp_return), color = "red", size = 2) +
    geom_text_repel(data = all_btc, label = "Bitcoin Only", family = "my_font", size = 3, nudge_x = -0.015, max.iter = 5000) +
    # Add Equal weighted portfolio
    #geom_point(data = equal_weighted, aes(x = sd, y = exp_return), color = "purple", size = 2) +
    #geom_text_repel(data = equal_weighted, label = "Equal Weighted", family = "my_font", size = 3, nudge_y = -0.004, nudge_x = 0.002, max.iter = 5000) +
    ggtitle(paste0("Did I Mention Bitcoin is Off the Charts?")) + labs(x = "Risk (standard deviation of portfolio variance)", y ="1-Month Return") +
    of_dollars_and_data_theme +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = percent)
  
  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "43-bitcoin-optimizer/efficient-frontier.jpeg")
  
  # Add a source and note string for the plots
  note_string   <- paste0("Note:  Assumes no asset can be >", max_alloc*100 ,"% of the portfolio and shorting is not allowed.") 
  
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

print(summary(wide_returns))