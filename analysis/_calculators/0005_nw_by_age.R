cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(jsonlite)
library(zoo)
library(readxl)
library(lubridate)
library(quantmod)
library(Hmisc)
library(scales)
library(tidyverse)

folder_name <- "_calculators/0005_nw_by_age"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year   <- 2025
half_window <- 2       # +/- years around the selected age (2 = 5-year window)
ages        <- 20:79   # ages offered in the calculator

# Pull ages beyond the 20-79 range so the end ages still get a full window
# (age 20 uses 18-22, age 79 uses 77-81)
df <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(year == data_year,
         age >= min(ages) - half_window,
         age <= max(ages) + half_window) %>%
  select(hh_id, imp_id, networth, homeeq, wgt, age) %>%
  mutate(networth_ex_home = networth - homeeq) %>%   # net worth excluding home equity
  arrange(hh_id, imp_id)

pcts <- seq(0.01, 0.99, 0.01)

# All 99 percentiles in one call per group, for both measures:
#   value         = total net worth
#   value_ex_home = net worth excluding home equity (networth - homeeq)
calc_pcts <- function(d, label){
  tibble(agecl         = label,
         pct           = pcts,
         value         = unname(wtd.quantile(d$networth,         weights = d$wgt, probs = pcts)),
         value_ex_home = unname(wtd.quantile(d$networth_ex_home, weights = d$wgt, probs = pcts)))
}

# "All Ages" keeps its old definition (household heads aged 20-79)
all_ages <- calc_pcts(df %>% filter(age >= min(ages), age <= max(ages)), "All Ages")

# One group per exact age, using households within +/- half_window years
by_age <- map_dfr(ages, function(a){
  calc_pcts(df %>% filter(abs(age - a) <= half_window), as.character(a))
})

final_pct_stack <- bind_rows(all_ages, by_age)

# Sample-size check: unique households behind each age's window
sample_check <- map_dfr(ages, function(a){
  w <- df %>% filter(abs(age - a) <= half_window)
  tibble(age = a, households = n_distinct(w$hh_id))
})
print(as.data.frame(sample_check))

saveRDS(final_pct_stack, paste0(localdir, "/nw_by_age_percentiles.Rds"))


to_calc <- readRDS(paste0(localdir, "/nw_by_age_percentiles.Rds"))

# ---------------------------------------------------------------------------
# Output: ONE data file, net_worth_data.js
#
# Upload it to wp-content/themes/odad/calculators/ (replace the old one), then
# flush Flywheel and Cloudflare. The calculator code, the age list and the
# "Data from the [year] Survey of Consumer Finances" line all come from the
# theme + this file.
# ---------------------------------------------------------------------------

groups <- unique(to_calc$agecl)   # "All Ages" first, then "20" ... "79"

pct_values <- to_calc %>%
  filter(agecl == groups[1]) %>%
  arrange(pct) %>%
  pull(pct)

data_list <- list(
  year   = data_year,
  window = half_window,   # JS shows e.g. "36 (34–38)"
  groups = groups,
  pct    = round(pct_values * 100),
  values = setNames(lapply(groups, function(g) {
    to_calc %>% filter(agecl == g) %>% arrange(pct) %>% pull(value) %>% round(4)
  }), groups),
  # Used when the reader picks "Include Home Equity? No"
  values_ex_home = setNames(lapply(groups, function(g) {
    to_calc %>% filter(agecl == g) %>% arrange(pct) %>% pull(value_ex_home) %>% round(4)
  }), groups)
)

# Every age needs one value per percentile, for both measures
stopifnot(all(sapply(data_list$values,         length) == length(data_list$pct)),
          all(sapply(data_list$values_ex_home, length) == length(data_list$pct)),
          !anyNA(unlist(data_list$values_ex_home)))

json_data <- toJSON(data_list, auto_unbox = TRUE, digits = NA)

writeLines(paste0("window.NW_DATA = ", json_data, ";"),
           paste0(out_path, "/net_worth_data.js"))

print(paste0("Wrote net_worth_data.js for ", data_year, " (", length(groups), " groups)"))

# ############################  End  ################################## #