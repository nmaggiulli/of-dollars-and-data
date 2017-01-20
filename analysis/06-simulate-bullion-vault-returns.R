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

# ############################  End  ################################## #

# Load in BV returns
bv_returns <- readRDS(paste0(localdir, "06-bv-returns.Rds"))

# Define the number of years
n_years <- nrow(bv_returns)

# Define the number of simulations
n_simulations <- 100

# Create a simulation vector
sim_vec <- seq(1, n_years, 1)

# Drop the year from the return to just have returns
returns <- bv_returns[, -1]

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
    eff[loop,"sharpe"]     <- eff[loop,"exp_return"] / eff[loop,"sd"]
    eff[loop,1:n]          <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
}

eff <- eff_frontier(returns=returns, short = "no", max_allocation = .4, risk_premium_upper_limit = .3, risk_increment = .001)

# Plot the efficient frontier
eff_optimal_point <- eff[eff$sharpe == max(eff$sharpe),]

# Color Scheme
ealred  <- "#7D110C"
ealtan  <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark  <- "#423C30"
ggplot(eff, aes(x = sd, y = exp_return)) + geom_point(alpha = .1, color = ealdark) +
  geom_point(data = eff_optimal_point, aes(x = sd, y = exp_return), color = ealred, size=5) +
  annotate(geom="text", x = eff_optimal_point$sd, y = eff_optimal_point$exp_return,
           label=paste("Risk: ", round(eff_optimal_point$sd * 100, digits = 2),"%\nReal Return: ",
                       round(eff_optimal_point$exp_return * 100, digits = 2),"%\nSharpe: ",
                       round(eff_optimal_point$sharpe * 100, digits = 2), "%", sep=""), hjust=0, vjust=1.2) +
  ggtitle("Efficient Frontier\nand Optimal Portfolio") + labs(x = "Risk (standard deviation of portfolio variance)", y ="Real Return") +
  of_dollars_and_data_theme +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)

# Simulate the portfolio value

