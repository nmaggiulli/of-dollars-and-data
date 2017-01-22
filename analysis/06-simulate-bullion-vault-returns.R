cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

library(dplyr)
library(ggplot2)
library(tidyr)
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
bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds"))

# Subset the returns to a smaller time period (for robustness)
# bv_returns <- filter(bv_returns, year(year) > 1985)

# This seed allows us to have reproducible random sampling
set.seed(12345)    

# Define the number of simulations (this will be used later)
n_simulations <- 100

# Define the number of years
n_years <- nrow(bv_returns)

# Drop the year and the risk free rate from the return to just have returns
returns <- bv_returns[, -which(names(bv_returns) %in% c("year", "tbill_3m"))]

n_assets <- ncol(returns)

avg_rf  <- colMeans(bv_returns[, "tbill_3m"])

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
           label=paste("Risk: ", round(eff_optimal_point$sd * 100, digits = 2),"%\nReal Return: ",
                       round(eff_optimal_point$exp_return * 100, digits = 2),"%\nSharpe: ",
                       round(eff_optimal_point$sharpe * 100, digits = 2), "%", sep=""), hjust=0, vjust=1.2) +
  ggtitle("Efficient Frontier\nand Optimal Portfolio") + labs(x = "Risk (standard deviation of portfolio variance)", y ="Real Return") +
  of_dollars_and_data_theme +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)

# Set the file_path based on the function input 
file_path = paste0(exportdir, "06-simulate-bv-returns/bv-efficient-frontier.jpeg")

# Add a source and note string for the plots
source_string <- "Source:  Bullion Vault (OfDollarsAndData.com)"
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

# Simulate the portfolio value

# Create a simulation vector
sim_vec <- seq(1, n_years, 1)

# Drop unneeded columns
optimal_weights <- eff_optimal_point[, -which(names(eff_optimal_point) %in% c("sd", "exp_return", "sharpe"))]

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

# Do this in a for loop over each year
for (i in 1:n_years){
  sampled_years_matrix[, i]    <- sample(sim_vec, n_simulations, replace = TRUE)
  for (j in 1:n_assets){
    sampled_returns[, j] <- 1 + unlist(returns[sampled_years_matrix[,i], j])
  }
  portfolio_return_matrix[, i] <- rowSums(t(as.vector(optimal_weights) * t(sampled_returns)))
  if (i == 1){
    value_matrix[, i] <- yearly_cash_add * (portfolio_return_matrix[ , i])
  } else {
    value_matrix[, i] <- (value_matrix[, i - 1] + yearly_cash_add) * (portfolio_return_matrix[, i])
  }
}

# Calculate some statistics
total_invested_capital <- n_years * yearly_cash_add
max_end_value <- max(value_matrix[, n_years])
min_end_value <- min(value_matrix[, n_years])
median_end_value <- quantile(value_matrix[, n_years], probs = 0.5)

# Caluclate the maximum drawdown for each simulation
max_drawdown <- 0
max_drawdown_matrix      <- matrix(NA, nrow = n_simulations, ncol = 1)

for (k in 1:n_simulations){
  drawdown <- maxDrawDown(value_matrix[k,])$maxdrawdown
  from <- maxDrawDown(value_matrix[k,])$from
  to <- maxDrawDown(value_matrix[k,])$to
  max_drawdown_matrix[k, 1] <- drawdown / value_matrix[k, from]
}

max_drawdown <- max(max_drawdown_matrix)
min_drawdown <- min(max_drawdown_matrix)
median_drawdown <- quantile(max_drawdown_matrix, probs = 0.5)

print(total_invested_capital)
print(max_end_value)
print(min_end_value)
print(median_end_value)

print(max_drawdown)
print(min_drawdown)
print(median_drawdown)
