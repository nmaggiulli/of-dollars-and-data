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

folder_name <- "xxxx_bullion_vault_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #
# ------------------------------------------------------------------
# 0. SETUP -- point this at your raw CSV
# ------------------------------------------------------------------
raw_path   <- paste0(importdir, "0006_bullion_vault_asset_returns/asset-returns-bullion-vault.csv")

yearly_cash_add <- 5000      # for the value-path simulation below
n_simulations   <- 1000
set.seed(12345)

# ------------------------------------------------------------------
# 1. LOAD + CONVERT TO REAL RETURNS
# ------------------------------------------------------------------
raw <- read_csv(raw_path, show_col_types = FALSE) %>%
  mutate(year = as.Date(year, format = "%m/%d/%y")) %>%
  mutate(year = if_else(year > Sys.Date(), year - years(100), year))

asset_cols <- setdiff(names(raw), c("year", "CPI"))

# Real return = (1 + nominal) / (1 + CPI) - 1
real_returns <- raw %>%
  mutate(across(all_of(asset_cols), ~ (1 + .x) / (1 + CPI) - 1)) %>%
  select(year, all_of(asset_cols))

min_year <- year(min(real_returns$year))
max_year <- year(max(real_returns$year))
n_years  <- nrow(real_returns)

# Risk-free = mean real return on 3-month T-bills
avg_rf <- mean(real_returns$`Tbill 3m`)

# Assets actually going into the optimizer (drop the risk-free asset)
returns    <- real_returns %>% select(-year, -`Tbill 3m`)
n_assets   <- ncol(returns)
mu_vec     <- colMeans(returns)
cov_matrix <- cov(returns)

# ------------------------------------------------------------------
# 2. ASSET RETURNS PLOT (all asset classes, faceted)
# ------------------------------------------------------------------
melted_returns <- melt(real_returns, id.vars = "year", variable.name = "asset")

returns_plot <- ggplot(data = melted_returns, aes(x = year, y = value, col = asset, fill = asset)) +
  geom_bar(stat = "identity") +
  facet_wrap(~asset) +
  ggtitle(paste0("Returns Vary by Asset Class\n", min_year, "-", max_year)) +
  scale_y_continuous(label = percent) +
  scale_x_date(breaks = seq(min(real_returns$year), max(real_returns$year), by = "10 years"),
               date_labels = "%y",
               limits = c(min(real_returns$year) - years(1), max(real_returns$year) + years(1))) +
  scale_color_discrete(guide = "none") +
  scale_fill_discrete(guide = "none") +
  of_dollars_and_data_theme +
  labs(x = "Year", y = "Annual Real Return (%)",
       caption = paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)\n",
                        "Note: Returns are adjusted using the U.S. Consumer Price Index."))

ggsave(paste0(out_path, "/bv-asset-returns-", min_year, "-", max_year, ".jpeg"), returns_plot, width = 15, height = 12, units = "cm")

# ------------------------------------------------------------------
# 3. EFFICIENT FRONTIER (long-only, fully invested)
# ------------------------------------------------------------------
eff_frontier <- function(returns, short = "no", max_allocation = NULL,
                         risk_premium_upper_limit = 0.5, risk_increment = 0.001) {
  
  cov_matrix <- cov(returns)
  n <- ncol(cov_matrix)
  
  Amat <- matrix(1, nrow = n)
  bvec <- 1
  meq  <- 1
  
  if (short == "no") {
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  if (!is.null(max_allocation)) {
    if (max_allocation <= 0 | max_allocation > 1) stop("max_allocation must be in (0, 1]")
    if (max_allocation * n < 1) stop("max_allocation too low for number of assets")
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max_allocation, n))
  }
  
  loop_seq <- seq(0, risk_premium_upper_limit, by = risk_increment)
  eff <- matrix(NA, nrow = length(loop_seq), ncol = n + 3)
  colnames(eff) <- c(colnames(returns), "sd", "exp_return", "sharpe")
  
  for (k in seq_along(loop_seq)) {
    i <- loop_seq[k]
    dvec <- colMeans(returns) * i
    sol  <- solve.QP(cov_matrix, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
    w    <- sol$solution
    port_var <- as.numeric(t(w) %*% cov_matrix %*% w)
    
    eff[k, 1:n]          <- w
    eff[k, "sd"]         <- sqrt(port_var)
    eff[k, "exp_return"] <- sum(w * colMeans(returns))
    eff[k, "sharpe"]     <- (eff[k, "exp_return"] - avg_rf) / eff[k, "sd"]
  }
  
  as.data.frame(eff)
}

max_alloc <- 1   # any single asset can be up to 100% of the portfolio
eff <- eff_frontier(returns, short = "no", max_allocation = max_alloc,
                    risk_premium_upper_limit = 0.5, risk_increment = 0.001)

optimal <- eff[which.max(eff$sharpe), ]

cat("\n=== OPTIMAL (MAX SHARPE) PORTFOLIO ===\n")
print(round(optimal[optimal > 0.001 & !names(optimal) %in% c("sd","exp_return","sharpe")], 3))
cat(sprintf("Real return: %.2f%%   Vol: %.2f%%   Sharpe: %.3f\n",
            optimal$exp_return*100, optimal$sd*100, optimal$sharpe))

# ------------------------------------------------------------------
# 4. BENCHMARK PORTFOLIOS FOR COMPARISON
# ------------------------------------------------------------------
find_ret_sd_sharpe <- function(w_named) {
  w <- w_named[colnames(returns)]
  w[is.na(w)] <- 0
  port_var <- as.numeric(t(w) %*% cov_matrix %*% w)
  tibble(
    sd         = sqrt(port_var),
    exp_return = sum(w * mu_vec),
    sharpe     = (exp_return - avg_rf) / sd
  )
}

zero_w <- setNames(rep(0, n_assets), colnames(returns))

benchmarks <- bind_rows(
  "All S&P 500"      = find_ret_sd_sharpe(replace(zero_w, "S&P 500", 1)),
  "All Gold"         = find_ret_sd_sharpe(replace(zero_w, "Gold", 1)),
  "50/50 Stock/Bond" = find_ret_sd_sharpe(replace(zero_w, c("S&P 500","Treasury 10yr"), c(0.5,0.5))),
  "60/40"            = find_ret_sd_sharpe(replace(zero_w, c("S&P 500","Treasury 10yr"), c(0.6,0.4))),
  "Equal Weighted"   = find_ret_sd_sharpe(setNames(rep(1/n_assets, n_assets), colnames(returns))),
  .id = "portfolio"
) %>%
  add_row(portfolio = "Optimal (Max Sharpe)",
          sd = optimal$sd, exp_return = optimal$exp_return, sharpe = optimal$sharpe)

print(benchmarks %>% mutate(across(where(is.numeric), ~round(.x, 4))))

# ------------------------------------------------------------------
# 5. EFFICIENT FRONTIER PLOT (with all comparison portfolios labeled)
# ------------------------------------------------------------------
ealred <- "#7D110C"; ealdark <- "#423C30"

all_stock        <- find_ret_sd_sharpe(replace(zero_w, "S&P 500", 1))
all_gold         <- find_ret_sd_sharpe(replace(zero_w, "Gold", 1))
all_home        <- find_ret_sd_sharpe(replace(zero_w, "U.S. Home Price", 1))
stock_bond_50_50 <- find_ret_sd_sharpe(replace(zero_w, c("S&P 500","Treasury 10yr"), c(0.5,0.5)))
equal_weighted   <- find_ret_sd_sharpe(setNames(rep(1/n_assets, n_assets), colnames(returns)))

frontier_plot <- ggplot(eff, aes(x = sd, y = exp_return)) +
  geom_point(alpha = .15, color = ealdark) +
  # Optimal portfolio
  geom_point(data = optimal, aes(x = sd, y = exp_return), color = ealred, size = 5) +
  geom_text_repel(data = optimal, label = "Optimal Portfolio", family = "my_font", size = 3.5,
                  nudge_x = -0.02, nudge_y = 0.009, max.iter = 5000) +
  # All S&P 500
  geom_point(data = all_stock, aes(x = sd, y = exp_return), color = "green", size = 2) +
  geom_text_repel(data = all_stock, label = "S&P 500 Only", family = "my_font", size = 3,
                  nudge_y = -0.004, max.iter = 5000) +
  # 50-50 Stock/Bond
  geom_point(data = stock_bond_50_50, aes(x = sd, y = exp_return), color = "blue", size = 2) +
  geom_text_repel(data = stock_bond_50_50, label = "50-50 Stock/Bond", family = "my_font", size = 3,
                  max.iter = 5000) +
  # All Gold
  geom_point(data = all_gold, aes(x = sd, y = exp_return), color = "#FFD700", size = 2) +
  geom_text_repel(data = all_gold, label = "Gold Only", family = "my_font", size = 3,
                  nudge_x = -0.015, max.iter = 5000) +
  
  # All U.S. Home
  geom_point(data = all_home, aes(x = sd, y = exp_return), color = "cyan", size = 2) +
  geom_text_repel(data = all_home, label = "U.S. Home Only", family = "my_font", size = 3, max.iter = 5000) +
  
  # Equal Weighted
  geom_point(data = equal_weighted, aes(x = sd, y = exp_return), color = "purple", size = 2) +
  geom_text_repel(data = equal_weighted, label = "Equal Weighted", family = "my_font", size = 3,
                  nudge_y = -0.004, nudge_x = 0.002, max.iter = 5000) +
  
  ggtitle("Efficient Frontier and Optimal Portfolio\n") +
  labs(x = "Risk (standard deviation of portfolio variance)", y = "Real Return",
       caption = paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)\n",
                        "Note: Assumes no asset can be >", max_alloc * 100, "% of the portfolio and shorting is not allowed.")) +
  of_dollars_and_data_theme +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)

ggsave(paste0(out_path, "/efficient-frontier-", min_year, "-", max_year, ".jpeg"), frontier_plot, width = 15, height = 12, units = "cm")

# ------------------------------------------------------------------
# 6. HOW OFTEN DOES THE OPTIMAL PORTFOLIO LOSE MONEY?
#    (i.e. how many of ITS holdings are negative in a given year)
# ------------------------------------------------------------------
opt_holdings <- names(returns)[as.numeric(optimal[names(returns)]) > 0.001]
n_holdings   <- length(opt_holdings)

neg_by_year <- real_returns %>%
  select(year, all_of(opt_holdings)) %>%
  mutate(across(all_of(opt_holdings), ~ if_else(.x < 0, 1, 0))) %>%
  rowwise() %>%
  mutate(n_neg = sum(c_across(all_of(opt_holdings)))) %>%
  ungroup() %>%
  select(year, n_neg)

pct_any_neg <- round(mean(neg_by_year$n_neg >= 1) * 100)

neg_plot <- ggplot(neg_by_year, aes(x = year, y = n_neg)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(limits = c(0, n_holdings), breaks = seq(0, n_holdings, 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle(paste0("At Least One Asset in the Optimal Portfolio\nLoses Money in ", pct_any_neg, "% of All Years")) +
  labs(x = "Year", y = "Number of Assets with a Negative Return",
       caption = paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)\n",
                        "Note: Optimal portfolio holds ", paste(opt_holdings, collapse = ", "), "."))

ggsave(paste0(out_path, "/optimal-portfolio-neg-returns-", min_year, "-", max_year, ".jpeg"), neg_plot, width = 15, height = 12, units = "cm")

# ------------------------------------------------------------------
# 7. ACTUAL DRAWDOWNS: OPTIMAL PORTFOLIO VS. S&P 500 (single overlaid panel)
#    $1 invested in 1972, actual historical return sequence, no
#    contributions along the way -- just the % drawdown from the
#    running peak of that $1.
# ------------------------------------------------------------------
opt_w <- as.numeric(optimal[colnames(returns)])

opt_port_returns <- as.numeric(as.matrix(returns) %*% opt_w)
sp500_returns    <- returns$`S&P 500`

growth_opt <- cumprod(1 + opt_port_returns)
growth_sp  <- cumprod(1 + sp500_returns)

drawdown_pct <- function(value) value / cummax(value) - 1

dd_compare <- bind_rows(
  tibble(year = real_returns$year, pct = drawdown_pct(growth_opt), portfolio = "Optimal Portfolio"),
  tibble(year = real_returns$year, pct = drawdown_pct(growth_sp),  portfolio = "S&P 500 Only")
) %>%
  mutate(portfolio = factor(portfolio, levels = c("S&P 500 Only", "Optimal Portfolio")))

drawdown_compare_plot <- ggplot(dd_compare, aes(x = year, y = pct, fill = portfolio)) +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("S&P 500 Only" = "red", "Optimal Portfolio" = chart_standard_color)) +
  ggtitle("The Optimal Portfolio Has Far Smaller Drawdowns\nThan the S&P 500") +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(label = percent) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(x = paste0("Year"), y = "Percentage of Value Lost",
       caption = paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)\n",
                        "Note: Assumes $1 invested in ", min_year, " with no additional contributions."))

ggsave(paste0(out_path, "/actual-drawdowns-optimal-vs-sp500-", min_year, "-", max_year, ".jpeg"), drawdown_compare_plot,
       width = 15, height = 12, units = "cm")

# ------------------------------------------------------------------
# 8. INTERESTING TIDBITS FOR THE POST
# ------------------------------------------------------------------

# 8a. Correlation matrix
cat("\n=== CORRELATION MATRIX ===\n")
print(round(cor(returns), 2))

# 8b. Best / worst year per asset
cat("\n=== BEST / WORST YEAR PER ASSET ===\n")
for (a in asset_cols) {
  vals <- real_returns[[a]]
  yrs  <- year(real_returns$year)
  cat(sprintf("%-18s Best: %d (%5.1f%%)   Worst: %d (%5.1f%%)\n",
              a, yrs[which.max(vals)], max(vals)*100,
              yrs[which.min(vals)], min(vals)*100))
}

# 8c. How often was each asset the year's best performer?
best_each_year <- apply(returns, 1, function(x) names(returns)[which.max(x)])
cat("\n=== TIMES EACH ASSET WAS THE YEAR'S TOP PERFORMER ===\n")
print(sort(table(best_each_year), decreasing = TRUE))

# 8d. Growth of $10,000 (real) since 1972
growth_10k <- sapply(returns, function(col) prod(1 + col) * 10000)
cat("\n=== GROWTH OF $10,000 (REAL), FULL PERIOD ===\n")
print(sort(round(growth_10k), decreasing = TRUE))

# 8e. Diversification: % of years at least 1 of 5 core assets was negative
core <- c("S&P 500","REIT","Treasury 10yr","Gold","U.S. Home Price")
neg_count <- rowSums(returns[, core] < 0)
cat(sprintf("\nAt least 1 of 5 core assets negative in %.0f%% of years\n", mean(neg_count >= 1)*100))
cat(sprintf("At least 2 of 5 core assets negative in %.0f%% of years\n", mean(neg_count >= 2)*100))

# 8f. Max drawdown: optimal portfolio vs. S&P 500 alone (real $10k lump sum, no contributions)
port_path <- cumprod(1 + as.matrix(returns) %*% opt_w) * 10000
sp500_path <- cumprod(1 + returns$`S&P 500`) * 10000
mdd <- function(path) min(path / cummax(path) - 1)
cat(sprintf("\nMax drawdown, Optimal Portfolio: %.1f%%\n", mdd(port_path)*100))
cat(sprintf("Max drawdown, S&P 500 Only:      %.1f%%\n", mdd(sp500_path)*100))

# Calculate the highest return asset in each year (summarize after)
real_returns_w_top <- real_returns %>%
  rowwise() %>%
  mutate(highest_ret_asset = names(real_returns)[-1][which.max(c_across(all_of(asset_cols)))]) %>%
  ungroup()

# Sanity check: tally of how often each asset was the year's top performer
real_returns_w_top %>%
  count(highest_ret_asset, sort = TRUE)

# Now do bottom assets by year
real_returns_w_bottom <- real_returns %>%
  rowwise() %>%
  mutate(lowest_ret_asset = names(real_returns)[-1][which.min(c_across(all_of(asset_cols)))]) %>%
  ungroup()

# Sanity check: tally of how often each asset was the year's worst performer
real_returns_w_bottom %>%
  count(lowest_ret_asset, sort = TRUE)

# Now do growth of $1
growth_of_1 <- real_returns %>%
  summarise(across(all_of(asset_cols), ~ prod(1 + .x))) %>%
  pivot_longer(everything(), names_to = "asset", values_to = "growth_of_1") %>%
  arrange(desc(growth_of_1))

print(growth_of_1)

# ------------------------------------------------------------------
# HIGH VS. LOW INFLATION REGIME: AVERAGE & MEDIAN REAL RETURNS BY ASSET
# ------------------------------------------------------------------
level_cpi <- 0.04
real_returns_w_regime <- real_returns %>%
  mutate(cpi = raw$CPI,
         inflation_regime = if_else(cpi > level_cpi, "High Inflation", "Low Inflation"))

regime_summary_avg <- real_returns_w_regime %>%
  group_by(inflation_regime) %>%
  summarise(across(all_of(asset_cols), mean), .groups = "drop") %>%
  pivot_longer(-inflation_regime, names_to = "asset", values_to = "avg_real_return") %>%
  pivot_wider(names_from = inflation_regime, values_from = avg_real_return) %>%
  mutate(diff_high_minus_low = `High Inflation` - `Low Inflation`) %>%
  arrange(desc(diff_high_minus_low))

regime_summary_median <- real_returns_w_regime %>%
  group_by(inflation_regime) %>%
  summarise(across(all_of(asset_cols), median), .groups = "drop") %>%
  pivot_longer(-inflation_regime, names_to = "asset", values_to = "median_real_return") %>%
  pivot_wider(names_from = inflation_regime, values_from = median_real_return) %>%
  mutate(diff_high_minus_low = `High Inflation` - `Low Inflation`) %>%
  arrange(desc(diff_high_minus_low))

cat("=== AVERAGE REAL RETURN BY REGIME ===\n")
print(regime_summary_avg %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n=== MEDIAN REAL RETURN BY REGIME ===\n")
print(regime_summary_median %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat(sprintf("\nLevel CPI (inflation split point): %.1f%%\n", level_cpi * 100))
cat(sprintf("Years classified as High Inflation: %d\n", sum(real_returns_w_regime$inflation_regime == "High Inflation")))
cat(sprintf("Years classified as Low Inflation:  %d\n", sum(real_returns_w_regime$inflation_regime == "Low Inflation")))

# Gold specifically
gold_high_avg <- regime_summary_avg %>% filter(asset == "Gold") %>% pull(`High Inflation`)
gold_low_avg  <- regime_summary_avg %>% filter(asset == "Gold") %>% pull(`Low Inflation`)
gold_high_med <- regime_summary_median %>% filter(asset == "Gold") %>% pull(`High Inflation`)
gold_low_med  <- regime_summary_median %>% filter(asset == "Gold") %>% pull(`Low Inflation`)

cat(sprintf("\nGold's AVG real return, High Inflation years: %.1f%%\n", gold_high_avg * 100))
cat(sprintf("Gold's AVG real return, Low Inflation years:  %.1f%%\n", gold_low_avg * 100))
cat(sprintf("Gold's avg return is %.1fx higher in high-inflation years\n", gold_high_avg / gold_low_avg))

cat(sprintf("\nGold's MEDIAN real return, High Inflation years: %.1f%%\n", gold_high_med * 100))
cat(sprintf("Gold's MEDIAN real return, Low Inflation years:  %.1f%%\n", gold_low_med * 100))
cat(sprintf("Gold's median return is %.1fx higher in high-inflation years\n", gold_high_med / gold_low_med))