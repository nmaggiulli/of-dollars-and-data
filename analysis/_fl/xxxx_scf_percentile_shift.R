cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(Hmisc)
library(tidyverse)

########################## Inputs ##################################### #

dollar_year <- 2025   # dollar base of 0003_scf_stack.Rds
age_band    <- 2      # compare against ages (my age - 2) to (my age + 2)

# Year-end net worth, NOMINAL dollars. Kept outside the repo in localdir.
# Columns: year, age, nw_nominal
my_nw <- read_csv(paste0(importdir, "_fl/xxxx_scf_percentile_shift/my_nw.csv"),
                  show_col_types = FALSE)

stopifnot(all(c("year", "age", "nw_nominal") %in% names(my_nw)))

########################## Load SCF ################################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  select(year, age, networth, wgt)

scf_years <- sort(unique(scf_stack$year))

# Confirm the stack is in 2025 dollars: back out the Fed's 2022 -> 2025
# factor from the published 2022 median ($192,700 in 2022 dollars)
d22 <- scf_stack %>% filter(year == 2022)
fed_factor_22 <- as.numeric(wtd.quantile(d22$networth, weights = d22$wgt,
                                         probs = 0.5)) / 192700

if(fed_factor_22 < 1.01){
  stop("0003_scf_stack.Rds does not look like it is in 2025 dollars. ",
       "Rebuild it from the 2025 SCF release first.")
}

########################## Inflation (CPI-U) ########################## #

# Year-end values are deflated with that year's December CPI (or the latest
# month available for the current year) and brought to dollar_year dollars
# using the dollar_year annual average CPI.
# CPI-U, U.S. city average, all items, not seasonally adjusted (BLS, 1982-84=100).
# Hardcoded - no download needed. 2026 uses the latest month available
# (August 2026) because December 2026 isn't out yet.
cpi_ye <- tribble(
  ~year, ~cpi_ye,
  2012,  229.601,
  2013,  233.049,
  2014,  234.812,
  2015,  236.525,
  2016,  241.432,
  2017,  246.524,
  2018,  251.233,
  2019,  256.974,
  2020,  260.474,
  2021,  278.802,
  2022,  296.797,
  2023,  306.746,
  2024,  315.605,
  2025,  324.054,
  2026,  334.980
)

cpi_base   <- 321.943   # 2025 annual average
cpi_avg_22 <- 292.655   # 2022 annual average (for the cross-check below)

# Cross-check: CPI-U 2022 -> 2025 factor vs the Fed's factor from the stack

print(paste0("2022 -> ", dollar_year, " factor | Fed (from stack): ",
             round(fed_factor_22, 4), " | CPI-U: ",
             round(cpi_base / cpi_avg_22, 4)))

########################## Percentile functions ####################### #

band_data <- function(yr, my_age){
  scf_stack %>%
    filter(year == yr,
           age >= my_age - age_band,
           age <= my_age + age_band)
}

p_grid <- seq(0.001, 0.999, by = 0.001)

band_quantiles <- function(yr, my_age){
  d <- band_data(yr, my_age)
  as.numeric(wtd.quantile(d$networth, weights = d$wgt, probs = p_grid))
}

# SCF years: exact weighted share of households at or below my net worth.
# Between SCF years: linearly interpolate the age-band distribution (quantile
#   by quantile, in real dollars) between the surrounding waves.
# After the last SCF year: hold the last wave's distribution constant in
#   real terms.
# Note: SCF wave Y is treated as matching year-end Y.
pct_for_year <- function(yr, my_age, x){
  
  if(yr %in% scf_years){
    d <- band_data(yr, my_age)
    return(tibble(pct    = sum(d$wgt[d$networth <= x]) / sum(d$wgt),
                  method = paste0("SCF ", yr)))
  }
  
  before <- scf_years[scf_years < yr]
  after  <- scf_years[scf_years > yr]
  
  if(length(before) == 0) stop("No SCF wave before ", yr)
  
  y0 <- max(before)
  
  if(length(after) == 0){
    q      <- band_quantiles(y0, my_age)
    method <- paste0("SCF ", y0, " held constant (real)")
  } else {
    y1     <- min(after)
    t      <- (yr - y0) / (y1 - y0)
    q      <- (1 - t) * band_quantiles(y0, my_age) +
      t  * band_quantiles(y1, my_age)
    method <- paste0("Interpolated ", y0, "-", y1)
  }
  
  tibble(pct = sum(q <= x) * 0.001, method = method)
}

ordinal <- function(n){
  suffix <- c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]
  suffix[n %% 100 %in% 11:13] <- "th"
  paste0(n, suffix)
}

########################## Run ######################################## #

results <- my_nw %>%
  left_join(cpi_ye, by = "year") %>%
  mutate(nw_real = nw_nominal * cpi_base / cpi_ye)

stopifnot(!any(is.na(results$nw_real)))

results <- bind_cols(results,
                     pmap_dfr(list(results$year, results$age, results$nw_real),
                              pct_for_year))

# Percentile label is rounded to the nearest percentile (95.7 -> "96th")
results_print <- results %>%
  transmute(Year          = year,
            Age           = age,
            `NW (nominal)` = format_as_dollar(nw_nominal),
            `NW (2025$)`   = format_as_dollar(nw_real),
            `Pct (exact)`  = round(100 * pct, 1),
            Percentile     = ordinal(floor(100 * pct + 0.5)),
            Method         = method)

print(results_print, n = Inf, width = Inf)

# ############################  End  ################################## #