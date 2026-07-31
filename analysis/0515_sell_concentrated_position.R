cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "0515_sell_concentrated_position"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

set.seed(12345)

n_employees <- 22000
n_mm        <- 4400
n_100mm     <- 400
equity_cap  <- 5e8            # ceiling on any single stake

target_p1   <- n_mm    / n_employees
target_p100 <- n_100mm / n_employees

# ---- Analytic Pareto fit (untruncated) --------------------------------- #
alpha_start <- log(target_p1 / target_p100) / log(1e8 / 1e6)
xmin_start  <- 1e6 * target_p1^(1 / alpha_start)

# ---- Re-solve so the TRUNCATED Pareto still hits both targets ----------- #
surv_trunc <- function(x, xmin, alpha, cap){
  ((xmin / x)^alpha - (xmin / cap)^alpha) / (1 - (xmin / cap)^alpha)
}

obj <- function(par){
  xmin  <- exp(par[1])
  alpha <- exp(par[2])
  log(surv_trunc(1e6, xmin, alpha, equity_cap) / target_p1)^2 +
    log(surv_trunc(1e8, xmin, alpha, equity_cap) / target_p100)^2
}

fit   <- optim(c(log(xmin_start), log(alpha_start)), obj,
               control = list(reltol = 1e-12, maxit = 5000))
xmin  <- exp(fit$par[1])
alpha <- exp(fit$par[2])

# ---- Simulate via inverse CDF ------------------------------------------ #
K <- 1 - (xmin / equity_cap)^alpha
u <- runif(n_employees)

data <- tibble(
  employee_id   = 1:n_employees,
  equity_amount = xmin * (1 - u * K)^(-1 / alpha)
) %>%
  arrange(equity_amount) %>%
  mutate(percentile = row_number() / n_employees)

# ---- Diagnostics -------------------------------------------------------- #
median_equity <- median(data$equity_amount)

cat("Alpha:              ", round(alpha, 4), "\n", sep = "")
cat("Minimum holding:    ", dollar(xmin, accuracy = 1), "\n", sep = "")
cat("Employees >= $1M:   ", sum(data$equity_amount >= 1e6), " (target ", n_mm, ")\n", sep = "")
cat("Employees >= $100M: ", sum(data$equity_amount >= 1e8), " (target ", n_100mm, ")\n", sep = "")
cat("Median equity:      ", dollar(median_equity, accuracy = 1), "\n", sep = "")
cat("Mean equity:        ", dollar(mean(data$equity_amount), accuracy = 1), "\n", sep = "")
cat("Total equity value: ", dollar(sum(data$equity_amount), accuracy = 1), "\n", sep = "")
cat("Top 400 share:      ",
    percent(sum(tail(data$equity_amount, 400)) / sum(data$equity_amount), accuracy = 1), "\n", sep = "")

bucket_levels <- c("Under\n$100K", "$100K-\n$300K", "$300K-\n$1M", "$1M-\n$3M",
                   "$3M-\n$10M", "$10M-\n$30M", "$30M-\n$100M", "$100M+")

to_plot <- data %>%
  mutate(bucket = case_when(
    equity_amount <     1e5 ~ bucket_levels[1],
    equity_amount <     3e5 ~ bucket_levels[2],
    equity_amount <     1e6 ~ bucket_levels[3],
    equity_amount <     3e6 ~ bucket_levels[4],
    equity_amount <     1e7 ~ bucket_levels[5],
    equity_amount <     3e7 ~ bucket_levels[6],
    equity_amount <     1e8 ~ bucket_levels[7],
    TRUE                    ~ bucket_levels[8]),
    bucket = factor(bucket, levels = bucket_levels)) %>%
  count(bucket, .drop = FALSE) %>%
  mutate(pct = n / sum(n))

file_path <- paste0(out_path, "/spacex_employee_equity_dist.jpeg")

source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note: Equity simulated as a Pareto distribution calibrated so that ",
                                 "4,400 of 22,000 employees hold $1M or more and 400 hold $100M or ",
                                 "more. Median holding is ", dollar(median_equity, accuracy = 1),
                                 ". Buckets are equal width on a log scale."),
                          width = 85)

plot <- ggplot(to_plot, aes(x = bucket, y = pct)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.5, size = 2.5) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     limits = c(0, max(to_plot$pct) * 1.15)) +
  of_dollars_and_data_theme +
  ggtitle("Estimated SpaceX Employee Equity Distribution") +
  labs(x = "Equity Value", y = "Percentage of Employees",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #