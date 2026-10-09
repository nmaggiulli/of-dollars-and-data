cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(Hmisc)
library(xtable)
library(tidyverse)

# ##################################################################### #
# THE TOP 1% IN THE SCF
#
# Sections:
#   1. Who the 1% are          (thresholds, counts above $15M/$30M,
#                               wealth vs. income overlap, demographics)
#   2. The balance sheet       (asset mix by tier, top-1% mix over time,
#                               prior -> data year change, wealth shares)
#   3. The transfer window     (share of top-1% wealth held by 70+)
#   4. Embedded gains          (unrealized gains / net worth, retirement
#                               share contrast)
#   5. Inheritance & bequests  (received, expected, bequest intent, giving,
#                               trusts, use of professionals)
#   6. The exemption           (households above / near their own line,
#                               illustrative 10-year projection)
#   7. Sanity checks
#
# Sections 1-3, 6 and the inheritance-received part of 5 run on build 0003
# as it stands. Everything else checks for its variables and skips with a
# message if they are not in the stack yet.
#
# VARIABLES THIS SCRIPT EXPECTS THE BUILD TO ADD LATER (clean forms, so the
# recoding lives in the build, not here):
#   kgtotal, kgbus, kghouse, kgore, kgstmf  - unrealized capital gains,
#        straight from the summary extract (already in real dollars there)
#   inh_expect         1 = expects to receive a substantial inheritance, 0 = no
#   inh_amt            total inheritances received, dollar_year dollars
#   bequest_expect     "Yes" / "Possibly" / "No"  (expects to leave a
#                      sizable estate)
#   charity_500        1 = made charitable contributions last year (x5822)
#   charity_amt        dollars given to charity last year (x5823, 0 if none)
#   trusts             value of trusts with an equity interest (summary var)
#   has_trust          1 = trusts > 0, 0 = no
#   ifinpro, ifinplan  1 = uses a lawyer/accountant/banker/broker, or a
#                      financial planner, for saving/investment information
#                      (summary vars, 1998+)
#
# Already in the build and used here: networth, asset, debt, income, bus,
# stocks, nmmf, retqliq, houses, oresre, nnresre, liq, cds, bond, savbnd,
# othma, age, married, edcl, inheritance (x5801), wgt.
#
# ON THE WEIGHTS: wgt is a population weight. sum(wgt) is a household count.
# Never divide by n_distinct(imp_id) - imp_id is a record id, not 1-5.
# Unweighted respondent counts are n_distinct(hh_id).
# ##################################################################### #

########################## Parameters ################################# #
# CHANGE THESE WHEN THE 2025 DATA LANDS.

data_year   <- 2022   # -> 2025
prior_year  <- 2019   # -> 2022
dollar_year <- 2022   # -> 2025  (dollar basis of 0003_scf_stack.Rds)

baseline_year <- 1989
compare_years <- c(1989, 2007)   # "versus 1989 and 2007" in Section 3

# Federal estate/gift exemption under OBBBA, per person / married couple.
# Set in 2026 dollars and indexed to inflation from there. With dollar_year
# 2025 the gap is a couple of percent, so treat the lines as approximate.
exemption_single  <- 15 * 10^6
exemption_married <- 30 * 10^6

# "Near the line" = within this fraction below it (0.5 -> 50%-100% of line)
near_band <- 0.5

# Illustrative projection: real annual growth rates and horizon. The
# exemption is indexed to inflation and the data are in real dollars, so
# real growth against a fixed real line is the right comparison.
growth_rates     <- c(0.02, 0.04, 0.06)
projection_years <- 10

older_age  <- 70    # "transfer window" cutoff (age of the household head)
min_cell_n <- 30    # suppress any cell built on fewer respondents

# Tier colors: bottom 90% gray, next 9% light blue, top 1% navy
tier_levels <- c("Bottom 90%", "Next 9%", "Top 1%")
tier_colors <- setNames(c("#B3B3B3", "#6baed6", chart_standard_color),
                        tier_levels)

prior_year_color <- "#B3B3B3"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(data_year))))
}

# Asset-mix components. Buckets sum to SCF `asset`; "Other" is the
# remainder (cash value life insurance, other financial, vehicles, other
# nonfinancial). Retirement accounts are their own bucket even though they
# hold stocks.
mix_levels <- c("Private business", "Stocks & funds", "Retirement accounts",
                "Primary residence", "Other real estate", "Cash & bonds",
                "Trusts & managed", "Other")
mix_colors <- setNames(c("#08306b", "#2171b5", "#6baed6", "#9ecae1",
                         "#c6dbef", "#d9d9d9", "#969696", "#525252"),
                       mix_levels)

caption_wrap     <- 80
label_size_small <- 2.4
label_size       <- 3.0

########################## Output paths ############################### #

folder_name <- "_fl/xxxx_scf_top_1pct"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE, recursive = TRUE)

########################## Console log ################################ #
# Everything printed to the console also goes to a text file in out_path:
#   - print()/cat() output via sink(split = TRUE), so it still shows in
#     the console
#   - message(), warning() and error text via global calling handlers,
#     which log the text and then let R show it as usual
# If a previous run died mid-script, its sink is still open - close it
# first so this run's log starts clean. Needs R 4.0+.

while(sink.number() > 0) sink()
globalCallingHandlers(NULL)

# basename() so a folder_name with a subfolder ("_fl/xxxx_...") still
# gives a plain file name
log_file <- paste0(out_path, "/", basename(folder_name), "_", data_year, "_log.txt")
log_con  <- file(log_file, open = "wt")
sink(log_con, split = TRUE)

log_condition <- function(prefix){
  function(cond){
    cat(prefix, sub("\n$", "", conditionMessage(cond)), "\n",
        sep = "", file = log_con)
  }
}

globalCallingHandlers(message = log_condition(""),
                      warning = log_condition("Warning: "),
                      error   = log_condition("Error: "))

cat("Run started: ", format(Sys.time()), "\n",
    "data_year = ", data_year, ", prior_year = ", prior_year,
    ", dollar_year = ", dollar_year, "\n\n", sep = "")

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack$year,
          prior_year %in% scf_stack$year)

has_vars <- function(v){ all(v %in% names(scf_stack)) }

core_vars <- c("year", "hh_id", "imp_id", "wgt", "networth", "asset", "debt",
               "income", "age", "married", "edcl", "inheritance",
               "bus", "stocks", "nmmf", "retqliq", "houses", "oresre",
               "nnresre", "liq", "cds", "bond", "savbnd", "othma")

missing_core <- setdiff(core_vars, names(scf_stack))
if(length(missing_core) > 0){
  stop("Core variables missing from 0003_scf_stack.Rds: ",
       paste(missing_core, collapse = ", "))
}

kg_vars      <- c("kgtotal", "kgbus", "kghouse", "kgore", "kgstmf")
later_vars   <- c(kg_vars, "inh_expect", "inh_amt", "bequest_expect",
                  "charity_500", "charity_amt", "trusts", "has_trust", "ifinpro",
                  "ifinplan")
present_later <- intersect(later_vars, names(scf_stack))

message("Later-build variables found: ",
        ifelse(length(present_later) == 0, "none",
               paste(present_later, collapse = ", ")))
message("Not yet in stack (those pieces will be skipped): ",
        paste(setdiff(later_vars, present_later), collapse = ", "))

df <- scf_stack %>%
  select(all_of(c(core_vars, present_later))) %>%
  arrange(year, hh_id, imp_id)

all_years <- sort(unique(df$year))
year_min  <- min(all_years)
year_max  <- max(all_years)

# Step back from year_max so the latest wave always gets a tick
year_breaks <- sort(seq(year_max, year_min, by = -3))

# ---- Tiers, assigned within each year -------------------------------- #
# Percentiles are computed on the pooled implicates with the population
# weight, same as the other SCF scripts. A respondent can land in different
# tiers across implicates; the weights handle that.
df <- df %>%
  group_by(year) %>%
  mutate(nw_p90  = wtd.quantile(networth, weights = wgt, probs = 0.90),
         nw_p99  = wtd.quantile(networth, weights = wgt, probs = 0.99),
         inc_p90 = wtd.quantile(income,   weights = wgt, probs = 0.90),
         inc_p99 = wtd.quantile(income,   weights = wgt, probs = 0.99)) %>%
  ungroup() %>%
  mutate(tier = factor(case_when(networth >= nw_p99 ~ "Top 1%",
                                 networth >= nw_p90 ~ "Next 9%",
                                 TRUE               ~ "Bottom 90%"),
                       levels = tier_levels),
         top1        = networth >= nw_p99,
         top1_income = income >= inc_p99,
         top10_income = income >= inc_p90,
         over_30m    = networth >= exemption_married,
         older       = age >= older_age,
         married_flag = married == 1,
         # edcl is a 4-level factor ordered from no diploma to college degree
         college     = if(is.factor(edcl)) as.integer(edcl) == nlevels(edcl) else edcl == 4,
         business_owner = bus > 0,
         # x5801: 1 = has received an inheritance, 5 = no
         inherited   = inheritance == 1,
         exempt_line = ifelse(married_flag, exemption_married, exemption_single),
         above_line  = networth >= exempt_line,
         near_line   = networth >= near_band * exempt_line &
           networth <  exempt_line,
         # Asset-mix buckets
         mix_business    = bus,
         mix_stocks      = stocks + nmmf,
         mix_retirement  = retqliq,
         mix_home        = houses,
         mix_other_re    = oresre + nnresre,
         mix_cash        = liq + cds + bond + savbnd,
         mix_trusts      = othma,
         mix_other       = asset - (mix_business + mix_stocks + mix_retirement +
                                      mix_home + mix_other_re + mix_cash +
                                      mix_trusts))

df_year  <- df %>% filter(year == data_year)
df_prior <- df %>% filter(year == prior_year)

n_hh <- n_distinct(df_year$hh_id)

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households. All figures are in ",
                               dollar_year, " dollars. Tiers are by net worth."),
                        width = caption_wrap)

note_string_ts <- str_wrap(paste0("Note: All figures are adjusted for inflation (",
                                  dollar_year, " dollars). Tiers are by net worth ",
                                  "within each year."),
                           width = caption_wrap)

source_string_ts <- paste0("Source:  Survey of Consumer Finances, ",
                           year_min, "-", year_max, " (OfDollarsAndData.com)")

########################## Helper Functions ########################### #

wtd_share <- function(condition, weights){
  as.numeric(wtd.mean(as.numeric(condition), weights = weights))
}

wtd_median <- function(x, weights){
  as.numeric(wtd.quantile(x, weights = weights, probs = 0.5))
}

# Aggregate ratio: share of the group's total X held in Y. This is what
# "share of top-1% wealth in businesses" means - not a mean of ratios.
agg_ratio <- function(num, den, weights){
  sum(weights * num, na.rm = TRUE) / sum(weights * den, na.rm = TRUE)
}

make_pct_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(formatC(100 * values, format = "f", digits = digits), "%"))
}

# "$13.7M", "$825k", "$1.2T" - one decimal for M and T
dollar_short <- function(x){
  ifelse(is.na(x), "n/a",
         ifelse(abs(x) >= 10^12,
                paste0("$", formatC(x/10^12, format = "f", digits = 1), "T"),
                ifelse(abs(x) >= 10^6,
                       paste0("$", formatC(x/10^6, format = "f", digits = 1), "M"),
                       paste0("$", formatC(x/10^3, format = "f", digits = 0), "k"))))
}

make_round_count <- function(values){
  ifelse(abs(values) >= 10^6,
         paste0(formatC(values/10^6, format = "f", digits = 1), "M"),
         paste0(formatC(round(values/10^3), format = "d", big.mark = ","), "k"))
}

make_caption <- function(note = note_string, src = source_string){
  paste0(src, "\n", note)
}

save_chart <- function(plot, file_path){
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

write_html_table <- function(table_out, file_path){
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        file = file_path)
}

period_factor <- function(year_vector){
  factor(as.character(year_vector),
         levels = c(as.character(prior_year), as.character(data_year)))
}

# Every summary carries n_unw (unweighted respondents). Cells below
# min_cell_n get their value columns blanked and are logged for Section 7.
thin_log <- tibble()

suppress_thin <- function(tbl, value_cols, label){
  thin <- tbl %>% filter(n_unw < min_cell_n)
  if(nrow(thin) > 0){
    message("Suppressed ", nrow(thin), " thin cell(s) in: ", label)
    thin_log <<- bind_rows(thin_log,
                           thin %>%
                             mutate(across(everything(), as.character)) %>%
                             mutate(table = label))
  }
  tbl %>%
    mutate(across(all_of(value_cols), ~ ifelse(n_unw < min_cell_n, NA, .x)))
}

# Asset mix for any grouping: aggregate share of total assets per bucket
asset_mix <- function(d, ...){
  d %>%
    group_by(...) %>%
    summarise(n_unw = n_distinct(hh_id),
              `Private business`    = agg_ratio(mix_business,   asset, wgt),
              `Stocks & funds`      = agg_ratio(mix_stocks,     asset, wgt),
              `Retirement accounts` = agg_ratio(mix_retirement, asset, wgt),
              `Primary residence`   = agg_ratio(mix_home,       asset, wgt),
              `Other real estate`   = agg_ratio(mix_other_re,   asset, wgt),
              `Cash & bonds`        = agg_ratio(mix_cash,       asset, wgt),
              `Trusts & managed`    = agg_ratio(mix_trusts,     asset, wgt),
              Other                 = agg_ratio(mix_other,      asset, wgt),
              .groups = "drop") %>%
    pivot_longer(cols = all_of(mix_levels), names_to = "component",
                 values_to = "share") %>%
    mutate(component = factor(component, levels = rev(mix_levels)))
}

# Line chart with an end-of-line label per series
line_with_end_labels <- function(data, y_var, col_var, label_fun, colors,
                                 title, y_lab, file_name,
                                 y_scale = percent_format(accuracy = 1)){
  end_labels <- data %>%
    filter(year == max(year)) %>%
    mutate(label = paste0(label_fun(.data[[y_var]]), " (", .data[[col_var]], ")"))
  
  plot <- ggplot(data, aes(x = year, y = .data[[y_var]],
                           col = .data[[col_var]])) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    geom_text(data = end_labels,
              aes(x = year, y = .data[[y_var]], label = label),
              hjust = -0.1, vjust = 0.4, size = label_size_small,
              show.legend = FALSE) +
    scale_y_continuous(label = y_scale,
                       expand = expansion(mult = c(0.06, 0.12))) +
    scale_x_continuous(breaks = year_breaks,
                       expand = expansion(mult = c(0.04, 0.30))) +
    scale_color_manual(values = colors) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ggtitle(title) +
    labs(x = "Year", y = y_lab,
         caption = make_caption(note_string_ts, source_string_ts))
  
  save_chart(plot, paste0(out_path, "/", file_name))
}

# ##################################################################### #
# SECTION 1: Who the 1% are
# ##################################################################### #

# ---- 1a: entry thresholds over time --------------------------------- #
thresholds <- df %>%
  group_by(year) %>%
  summarise(nw_p90  = first(nw_p90),
            nw_p99  = first(nw_p99),
            inc_p90 = first(inc_p90),
            inc_p99 = first(inc_p99),
            .groups = "drop")

print("Top 1% and top 10% entry thresholds by year:")
print(thresholds %>%
        mutate(across(-year, dollar_short)) %>%
        as.data.frame())

threshold_long <- thresholds %>%
  select(year, `Net worth` = nw_p99, Income = inc_p99) %>%
  pivot_longer(-year, names_to = "measure", values_to = "value")

line_with_end_labels(
  threshold_long, "value", "measure", dollar_short,
  setNames(c(chart_standard_color, "#6baed6"), c("Net worth", "Income")),
  paste0("What It Takes to Be in the 1%\n",
         "Net Worth and Income Cutoffs"),
  "Top 1% Threshold",
  "01_top1_thresholds_over_time.jpeg",
  y_scale = function(x) paste0("$", formatC(x/10^6, format = "f", digits = 0), "M"))

write_html_table(
  thresholds %>%
    transmute(Year = as.character(year),
              `Top 10% net worth` = dollar_short(nw_p90),
              `Top 1% net worth`  = dollar_short(nw_p99),
              `Top 10% income`    = dollar_short(inc_p90),
              `Top 1% income`     = dollar_short(inc_p99)),
  paste0(out_path, "/01_thresholds_table.html"))

# ---- 1b: households above the exemption amounts --------------------- #
exempt_counts <- map_dfr(c(exemption_single, exemption_married), function(th){
  df %>%
    group_by(year) %>%
    summarise(households = sum(wgt[networth >= th]),
              n_unw      = n_distinct(hh_id[networth >= th]),
              share      = wtd_share(networth >= th, wgt),
              .groups = "drop") %>%
    mutate(threshold = paste0("$", th/10^6, "M+"))
}) %>%
  mutate(threshold = factor(threshold,
                            levels = paste0("$", c(exemption_single,
                                                   exemption_married)/10^6, "M+")))

print("Unweighted households behind the exemption-count lines:")
print(exempt_counts %>%
        group_by(threshold) %>%
        summarise(min_n = min(n_unw),
                  n_latest = n_unw[year == data_year], .groups = "drop") %>%
        as.data.frame())

line_with_end_labels(
  exempt_counts, "households", "threshold", make_round_count,
  setNames(c("#6baed6", chart_standard_color), levels(exempt_counts$threshold)),
  paste0("Households Above the New\n",
         "Estate Tax Exemption Amounts"),
  "Total U.S. Households",
  "01_households_above_exemption_amounts.jpeg",
  y_scale = comma)

write_html_table(
  exempt_counts %>%
    transmute(Year = as.character(year), threshold,
              display = paste0(make_round_count(households), " (",
                               make_pct_labels(share, 2), ")")) %>%
    pivot_wider(names_from = threshold, values_from = display),
  paste0(out_path, "/01_households_above_exemption_table.html"))

# ---- 1c: wealth vs. income overlap ---------------------------------- #
overlap <- df %>%
  filter(top1) %>%
  group_by(year) %>%
  summarise(n_unw = n_distinct(hh_id),
            in_top1_income  = wtd_share(top1_income,  wgt),
            in_top10_income = wtd_share(top10_income, wgt),
            .groups = "drop")

print("Share of the top 1% by WEALTH who are also top earners:")
print(overlap %>%
        mutate(across(c(in_top1_income, in_top10_income), make_pct_labels)) %>%
        as.data.frame())

overlap_long <- overlap %>%
  select(year, `Top 1% income` = in_top1_income,
         `Top 10% income` = in_top10_income) %>%
  pivot_longer(-year, names_to = "measure", values_to = "share")

line_with_end_labels(
  overlap_long, "share", "measure", make_pct_labels,
  setNames(c(chart_standard_color, "#6baed6"),
           c("Top 1% income", "Top 10% income")),
  paste0("Rich Isn't the Same as High Pay\n",
         "Top 1% Wealth Also in Top Income"),
  "Share of Top 1% (Net Worth)",
  "01_wealth_income_overlap.jpeg")

# ---- 1d: demographics by tier, prior vs. data year ------------------ #
demographics <- df %>%
  filter(year %in% c(prior_year, data_year)) %>%
  group_by(year, tier) %>%
  summarise(n_unw          = n_distinct(hh_id),
            households     = sum(wgt),
            median_nw      = wtd_median(networth, wgt),
            median_income  = wtd_median(income, wgt),
            median_age     = wtd_median(age, wgt),
            share_older    = wtd_share(older, wgt),
            share_married  = wtd_share(married_flag, wgt),
            share_college  = wtd_share(college, wgt),
            share_business = wtd_share(business_owner, wgt),
            share_inherited = wtd_share(inherited, wgt),
            .groups = "drop")

# Same row for the $30M+ group inside the top 1%
demographics_30m <- df %>%
  filter(year %in% c(prior_year, data_year), over_30m) %>%
  group_by(year) %>%
  summarise(n_unw          = n_distinct(hh_id),
            households     = sum(wgt),
            median_nw      = wtd_median(networth, wgt),
            median_income  = wtd_median(income, wgt),
            median_age     = wtd_median(age, wgt),
            share_older    = wtd_share(older, wgt),
            share_married  = wtd_share(married_flag, wgt),
            share_college  = wtd_share(college, wgt),
            share_business = wtd_share(business_owner, wgt),
            share_inherited = wtd_share(inherited, wgt),
            .groups = "drop") %>%
  mutate(tier = "$30M+")

demo_value_cols <- c("households", "median_nw", "median_income", "median_age",
                     "share_older", "share_married", "share_college",
                     "share_business", "share_inherited")

demographics_all <- bind_rows(demographics %>% mutate(tier = as.character(tier)),
                              demographics_30m) %>%
  suppress_thin(demo_value_cols, "1d demographics")

write_html_table(
  demographics_all %>%
    arrange(desc(year), factor(tier, levels = c(rev(tier_levels), "$30M+"))) %>%
    transmute(Year = as.character(year),
              Tier = tier,
              Households      = make_round_count(households),
              `Median net worth` = dollar_short(median_nw),
              `Median income`    = dollar_short(median_income),
              `Median age`       = formatC(median_age, format = "f", digits = 0),
              `Age 70+`          = make_pct_labels(share_older),
              Married            = make_pct_labels(share_married),
              `College degree`   = make_pct_labels(share_college),
              `Owns a business`  = make_pct_labels(share_business),
              `Ever inherited`   = make_pct_labels(share_inherited),
              `Respondents`      = formatC(n_unw, format = "d", big.mark = ",")),
  paste0(out_path, "/01_demographics_by_tier_table.html"))

# ##################################################################### #
# SECTION 2: The balance sheet
# ##################################################################### #

# ---- 2a: asset mix by tier, data year ------------------------------- #
mix_tier <- asset_mix(df_year, tier)

text_labels <- mix_tier %>%
  group_by(tier) %>%
  arrange(desc(component)) %>%
  mutate(pos = cumsum(share) - share / 2) %>%
  ungroup() %>%
  filter(share >= 0.05)

plot <- ggplot(mix_tier, aes(x = tier, y = share, fill = component)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(data = text_labels,
            aes(x = tier, y = pos, label = make_pct_labels(share)),
            col = "white", size = label_size_small) +
  scale_fill_manual(values = mix_colors, breaks = mix_levels) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 7)) +
  ggtitle(paste0("What the 1% Own\n",
                 "Share of Total Assets, ", data_year)) +
  labs(x = NULL, y = "Share of Total Assets",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/02_asset_mix_by_tier.jpeg"))

write_html_table(
  mix_tier %>%
    mutate(component = factor(component, levels = mix_levels)) %>%
    arrange(component) %>%
    transmute(tier, component, share = make_pct_labels(share, 1)) %>%
    pivot_wider(names_from = tier, values_from = share) %>%
    rename(Component = component),
  paste0(out_path, "/02_asset_mix_by_tier_table.html"))

# ---- 2b: top-1% asset mix over time ---------------------------------- #
mix_top1_time <- asset_mix(df %>% filter(top1), year)

plot <- ggplot(mix_top1_time, aes(x = year, y = share, fill = component)) +
  geom_area() +
  scale_fill_manual(values = mix_colors, breaks = mix_levels) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0, 0))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 7)) +
  ggtitle(paste0("How the 1%'s Assets Have Shifted\n",
                 "Share of Total Assets")) +
  labs(x = "Year", y = "Share of Total Assets",
       caption = make_caption(note_string_ts, source_string_ts))

save_chart(plot, paste0(out_path, "/02_top1_asset_mix_over_time.jpeg"))

# ---- 2c: prior -> data year change, per top-1% household ------------ #
# Means, not medians: most components have a median of zero even at the
# top. The top 1% is a different set of households in each wave, so this
# is "the average top-1% balance sheet then vs. now", not a panel.
component_means <- df %>%
  filter(top1, year %in% c(prior_year, data_year)) %>%
  group_by(year) %>%
  summarise(n_unw = n_distinct(hh_id),
            `Private business`    = weighted.mean(mix_business,   wgt),
            `Stocks & funds`      = weighted.mean(mix_stocks,     wgt),
            `Retirement accounts` = weighted.mean(mix_retirement, wgt),
            `Primary residence`   = weighted.mean(mix_home,       wgt),
            `Other real estate`   = weighted.mean(mix_other_re,   wgt),
            `Cash & bonds`        = weighted.mean(mix_cash,       wgt),
            `Trusts & managed`    = weighted.mean(mix_trusts,     wgt),
            Other                 = weighted.mean(mix_other,      wgt),
            Debt                  = -weighted.mean(debt,          wgt),
            `Net worth`           = weighted.mean(networth,       wgt),
            .groups = "drop")

component_change <- component_means %>%
  select(-n_unw) %>%
  pivot_longer(-year, names_to = "component", values_to = "mean") %>%
  pivot_wider(names_from = year, values_from = mean, names_prefix = "y") %>%
  mutate(change = .data[[paste0("y", data_year)]] - .data[[paste0("y", prior_year)]],
         component = factor(component, levels = rev(c("Net worth", mix_levels, "Debt"))))

print(paste0("Average top-1% balance sheet, ", prior_year, " vs ", data_year, ":"))
print(component_change %>%
        mutate(across(starts_with("y"), dollar_short),
               change = dollar_short(change)) %>%
        as.data.frame())

plot <- ggplot(component_change %>% filter(component != "Net worth"),
               aes(x = component, y = change)) +
  geom_bar(stat = "identity",
           aes(fill = change >= 0), show.legend = FALSE) +
  geom_text(aes(label = dollar_short(change),
                hjust = ifelse(change >= 0, -0.1, 1.1)),
            col = chart_standard_color, size = label_size_small) +
  geom_hline(yintercept = 0, col = "black") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = chart_standard_color,
                               `FALSE` = prior_year_color)) +
  scale_y_continuous(label = function(x) paste0("$", formatC(x/10^6, format = "f", digits = 1), "M"),
                     expand = expansion(mult = c(0.25, 0.25))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("What Drove the 1%'s Gains\n",
                 "Avg. Change, ", prior_year, "-", data_year)) +
  labs(x = NULL, y = "Change per Top 1% Household",
       caption = make_caption(str_wrap(paste0(
         "Note: Average balance sheet of top 1% households in each year, ",
         dollar_year, " dollars. Debt shown as negative, so a rise in debt ",
         "is a negative bar."), width = caption_wrap)))

save_chart(plot, paste0(out_path, "/02_top1_component_change.jpeg"))

# ---- 2d: share of all net worth by tier, over time ------------------ #
wealth_shares <- df %>%
  group_by(year) %>%
  mutate(total_nw = sum(wgt * networth)) %>%
  group_by(year, tier) %>%
  summarise(share = sum(wgt * networth) / first(total_nw), .groups = "drop")

line_with_end_labels(
  wealth_shares %>% mutate(tier = as.character(tier)),
  "share", "tier", make_pct_labels, tier_colors,
  paste0("Who Owns America's Wealth\n",
         "Share of Total Net Worth"),
  "Share of U.S. Net Worth",
  "02_wealth_share_by_tier.jpeg")

# ---- 2e: business ownership and concentration by tier --------------- #
# The SCF records up to two actively managed businesses plus passive
# stakes. It cannot see single-stock positions in public companies.
business <- df %>%
  filter(year %in% c(prior_year, data_year)) %>%
  group_by(year, tier) %>%
  summarise(n_unw           = n_distinct(hh_id),
            owns_business   = wtd_share(business_owner, wgt),
            bus_share_nw    = agg_ratio(bus, networth, wgt),
            # Share of the tier whose business is half or more of net worth
            bus_half_plus   = wtd_share(networth > 0 & bus >= 0.5 * networth, wgt),
            .groups = "drop") %>%
  suppress_thin(c("owns_business", "bus_share_nw", "bus_half_plus"),
                "2e business")

write_html_table(
  business %>%
    arrange(desc(year), desc(tier)) %>%
    transmute(Year = as.character(year), Tier = as.character(tier),
              `Owns a business`        = make_pct_labels(owns_business),
              `Business share of net worth` = make_pct_labels(bus_share_nw),
              `Business is 50%+ of net worth` = make_pct_labels(bus_half_plus)),
  paste0(out_path, "/02_business_by_tier_table.html"))

# ##################################################################### #
# SECTION 3: The transfer window
# ##################################################################### #

older_share <- df %>%
  filter(top1) %>%
  group_by(year) %>%
  summarise(n_unw        = n_distinct(hh_id[older]),
            wealth_share = agg_ratio(networth * older, networth, wgt),
            hh_share     = wtd_share(older, wgt),
            older_wealth = sum(wgt * networth * older),
            .groups = "drop")

print(paste0("Top 1%: share held by households ", older_age, "+:"))
print(older_share %>%
        mutate(wealth_share = make_pct_labels(wealth_share),
               hh_share = make_pct_labels(hh_share),
               older_wealth = dollar_short(older_wealth)) %>%
        as.data.frame())

print("Comparison years:")
print(older_share %>%
        filter(year %in% c(compare_years, data_year)) %>%
        mutate(wealth_share = make_pct_labels(wealth_share),
               hh_share = make_pct_labels(hh_share),
               older_wealth = dollar_short(older_wealth)) %>%
        as.data.frame())

# Robustness: does the latest-year jump hold at other age cutoffs, or is it
# a cohort bunching right at older_age? Also prints the top-1% median age.
print("Top 1%: share of wealth held by age cutoff, and median age:")
print(df %>%
        filter(top1) %>%
        group_by(year) %>%
        summarise(`65+` = make_pct_labels(agg_ratio(networth * (age >= 65), networth, wgt)),
                  `70+` = make_pct_labels(agg_ratio(networth * (age >= 70), networth, wgt)),
                  `75+` = make_pct_labels(agg_ratio(networth * (age >= 75), networth, wgt)),
                  median_age = wtd_median(age, wgt),
                  .groups = "drop") %>%
        as.data.frame())

older_long <- older_share %>%
  select(year, `Share of wealth` = wealth_share,
         `Share of households` = hh_share) %>%
  pivot_longer(-year, names_to = "measure", values_to = "share")

line_with_end_labels(
  older_long, "share", "measure", make_pct_labels,
  setNames(c(chart_standard_color, "#6baed6"),
           c("Share of wealth", "Share of households")),
  paste0("The 1%'s Wealth Is Getting Older\n",
         "Held by Households ", older_age, "+"),
  paste0("Share of Top 1% Held by ", older_age, "+"),
  "03_top1_older_share.jpeg")

# What older top-1% households hold vs. younger ones (data year)
mix_by_age <- asset_mix(df_year %>%
                          filter(top1) %>%
                          mutate(age_group = factor(ifelse(older,
                                                           paste0(older_age, "+"),
                                                           paste0("Under ", older_age)),
                                                    levels = c(paste0("Under ", older_age),
                                                               paste0(older_age, "+")))),
                        age_group)

text_labels <- mix_by_age %>%
  group_by(age_group) %>%
  arrange(desc(component)) %>%
  mutate(pos = cumsum(share) - share / 2) %>%
  ungroup() %>%
  filter(share >= 0.05)

plot <- ggplot(mix_by_age, aes(x = age_group, y = share, fill = component)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(data = text_labels,
            aes(x = age_group, y = pos, label = make_pct_labels(share)),
            col = "white", size = label_size_small) +
  scale_fill_manual(values = mix_colors, breaks = mix_levels) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 7)) +
  ggtitle(paste0("What Will Transfer First\n",
                 "Top 1% Assets by Age, ", data_year)) +
  labs(x = NULL, y = "Share of Total Assets",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/03_top1_asset_mix_by_age.jpeg"))

# ##################################################################### #
# SECTION 4: Embedded gains and the step-up
# ##################################################################### #

# ---- 4a: retirement-account share of net worth (runs today) --------- #
# The contrast for the next 9%: their problem is the inherited-IRA 10-year
# rule, not the step-up.
retirement_share <- df_year %>%
  group_by(tier) %>%
  summarise(n_unw = n_distinct(hh_id),
            share = agg_ratio(retqliq, networth, wgt),
            .groups = "drop")

# ---- 4b: unrealized gains (needs kg vars) --------------------------- #
if(has_vars(kg_vars)){
  
  gains_tier <- df_year %>%
    group_by(tier) %>%
    summarise(n_unw = n_distinct(hh_id),
              Business            = agg_ratio(kgbus,   networth, wgt),
              `Primary residence` = agg_ratio(kghouse, networth, wgt),
              `Other real estate` = agg_ratio(kgore,   networth, wgt),
              `Stocks & funds`    = agg_ratio(kgstmf,  networth, wgt),
              total               = agg_ratio(kgtotal, networth, wgt),
              .groups = "drop")
  
  print("Unrealized gains as a share of net worth, by tier:")
  print(gains_tier %>% mutate(across(-c(tier, n_unw), make_pct_labels)) %>%
          as.data.frame())
  
  gain_levels <- c("Business", "Stocks & funds", "Other real estate",
                   "Primary residence")
  
  gains_long <- gains_tier %>%
    select(-total, -n_unw) %>%
    pivot_longer(-tier, names_to = "source", values_to = "share") %>%
    mutate(source = factor(source, levels = rev(gain_levels)))
  
  total_labels <- gains_tier %>%
    left_join(retirement_share %>% select(tier, ret_share = share), by = "tier")
  
  plot <- ggplot(gains_long, aes(x = tier, y = share, fill = source)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(data = total_labels,
              aes(x = tier, y = total, label = make_pct_labels(total)),
              inherit.aes = FALSE, vjust = -0.5,
              col = chart_standard_color, size = label_size) +
    scale_fill_manual(values = setNames(c("#08306b", "#2171b5", "#6baed6",
                                          "#c6dbef"), gain_levels),
                      breaks = gain_levels) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.10))) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("How Much Is Untaxed Gains?\n",
                   "Unrealized Gains / Net Worth")) +
    labs(x = NULL, y = "Share of Net Worth",
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/04_unrealized_gains_by_tier.jpeg"))
  
  # Within the top 1%, by age - gains held until death get the step-up
  gains_age <- df_year %>%
    filter(top1) %>%
    mutate(age_group = cut(age, breaks = c(-Inf, 54, older_age - 1, Inf),
                           labels = c("Under 55", paste0("55-", older_age - 1),
                                      paste0(older_age, "+")))) %>%
    group_by(age_group) %>%
    summarise(n_unw = n_distinct(hh_id),
              gains_share = agg_ratio(kgtotal, networth, wgt),
              gains_total = sum(wgt * kgtotal),
              .groups = "drop") %>%
    suppress_thin(c("gains_share", "gains_total"), "4b gains by age")
  
  print("Top 1% unrealized gains by age of head:")
  print(gains_age %>%
          mutate(gains_share = make_pct_labels(gains_share),
                 gains_total = dollar_short(gains_total)) %>%
          as.data.frame())
  
  plot <- ggplot(gains_age, aes(x = age_group, y = gains_share)) +
    geom_bar(stat = "identity", fill = chart_standard_color, width = 0.6) +
    geom_text(aes(label = make_pct_labels(gains_share)), vjust = -0.5,
              col = chart_standard_color, size = label_size) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.10))) +
    of_dollars_and_data_theme +
    ggtitle(paste0("The Step-Up Matters Most Late\n",
                   "Top 1% Gains / Net Worth by Age")) +
    labs(x = "Age of Head of Household", y = "Share of Net Worth",
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/04_top1_gains_by_age.jpeg"))
  
  # Over time for the top 1% (kg vars exist in every wave of the extract)
  gains_time <- df %>%
    group_by(year, tier) %>%
    summarise(share = agg_ratio(kgtotal, networth, wgt), .groups = "drop") %>%
    mutate(tier = as.character(tier))
  
  line_with_end_labels(
    gains_time, "share", "tier", make_pct_labels, tier_colors,
    paste0("Unrealized Gains Over Time\n",
           "Share of Net Worth by Tier"),
    "Unrealized Gains / Net Worth",
    "04_unrealized_gains_over_time.jpeg")
  
  write_html_table(
    total_labels %>%
      arrange(desc(tier)) %>%
      transmute(Tier = as.character(tier),
                Business = make_pct_labels(Business),
                `Stocks & funds` = make_pct_labels(`Stocks & funds`),
                `Other real estate` = make_pct_labels(`Other real estate`),
                `Primary residence` = make_pct_labels(`Primary residence`),
                `Total unrealized gains` = make_pct_labels(total),
                `Retirement accounts` = make_pct_labels(ret_share)),
    paste0(out_path, "/04_gains_vs_retirement_table.html"))
  
} else {
  message("SECTION 4: kg variables not in stack - skipping unrealized gains.")
  
  write_html_table(
    retirement_share %>%
      arrange(desc(tier)) %>%
      transmute(Tier = as.character(tier),
                `Retirement accounts / net worth` = make_pct_labels(share)),
    paste0(out_path, "/04_retirement_share_table.html"))
}

# ##################################################################### #
# SECTION 5: Inheritance in, bequests out
# ##################################################################### #

# Groups for this section: the three tiers plus $30M+
tier_plus_30m <- function(d){
  bind_rows(d %>% mutate(group = as.character(tier)),
            d %>% filter(over_30m) %>% mutate(group = "$30M+")) %>%
    mutate(group = factor(group, levels = c(tier_levels, "$30M+")))
}

# ---- 5a: ever received an inheritance (runs today) ------------------ #
inherited_tier <- tier_plus_30m(df %>% filter(year %in% c(prior_year, data_year))) %>%
  group_by(year, group) %>%
  summarise(n_unw = n_distinct(hh_id),
            share = wtd_share(inherited, wgt),
            .groups = "drop") %>%
  suppress_thin("share", "5a inherited") %>%
  mutate(period = period_factor(year))

plot <- ggplot(inherited_tier, aes(x = group, y = share, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.75) +
  geom_text(aes(label = make_pct_labels(share)),
            position = position_dodge(width = 0.8), vjust = -0.5,
            col = chart_standard_color, size = label_size_small) +
  period_fill_scale() +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.10))) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Who Has Inherited Money?\n",
                 "Ever Received an Inheritance")) +
  labs(x = NULL, y = "Share of Households",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/05_inherited_by_tier.jpeg"))

inherited_time <- df %>%
  group_by(year, tier) %>%
  summarise(share = wtd_share(inherited, wgt), .groups = "drop") %>%
  mutate(tier = as.character(tier))

line_with_end_labels(
  inherited_time, "share", "tier", make_pct_labels, tier_colors,
  paste0("Self-Made or Inherited?\n",
         "Share Who Ever Inherited"),
  "Share of Households",
  "05_inherited_over_time.jpeg")

# ---- 5b: expect to receive / inheritance amounts -------------------- #
if(has_vars("inh_expect")){
  inh_expect_tier <- tier_plus_30m(df_year) %>%
    group_by(group) %>%
    summarise(n_unw = n_distinct(hh_id),
              share = wtd_share(inh_expect == 1, wgt),
              .groups = "drop") %>%
    suppress_thin("share", "5b expects inheritance")
  
  print("Share expecting to receive an inheritance:")
  print(inh_expect_tier %>% mutate(share = make_pct_labels(share)) %>%
          as.data.frame())
} else {
  message("SECTION 5b: inh_expect not in stack - skipping.")
}

if(has_vars("inh_amt")){
  inh_amt_tier <- tier_plus_30m(df_year %>% filter(inherited, inh_amt > 0)) %>%
    group_by(group) %>%
    summarise(n_unw = n_distinct(hh_id),
              median_amt = wtd_median(inh_amt, wgt),
              # Inheritances received as a share of current net worth -
              # a rough "how much of this is inherited" measure
              inh_share_nw = agg_ratio(inh_amt, networth, wgt),
              .groups = "drop") %>%
    suppress_thin(c("median_amt", "inh_share_nw"), "5b inheritance amounts")
  
  print("Among those who inherited:")
  print(inh_amt_tier %>%
          mutate(median_amt = dollar_short(median_amt),
                 inh_share_nw = make_pct_labels(inh_share_nw)) %>%
          as.data.frame())
}

# ---- 5c: bequest intent and the gap --------------------------------- #
# x5825: "Do you expect to leave a sizable estate to others?" (yes /
# possibly / no). The 2022 survey has no importance question, so the gap is
# measured directly: households whose net worth all but guarantees a large
# estate, but who don't say they expect to leave one.
if(has_vars("bequest_expect")){
  
  bequest_levels <- c("Yes", "Possibly", "No")
  
  bequest_tier <- tier_plus_30m(df_year) %>%
    filter(!is.na(bequest_expect)) %>%
    group_by(group) %>%
    summarise(n_unw = n_distinct(hh_id),
              Yes       = wtd_share(bequest_expect == "Yes", wgt),
              Possibly  = wtd_share(bequest_expect == "Possibly", wgt),
              No        = wtd_share(bequest_expect == "No", wgt),
              .groups = "drop") %>%
    suppress_thin(bequest_levels, "5c bequest intent")
  
  print("Expect to leave a sizable estate, by group:")
  print(bequest_tier %>% mutate(across(all_of(bequest_levels), make_pct_labels)) %>%
          as.data.frame())
  
  bequest_long <- bequest_tier %>%
    select(group, all_of(bequest_levels)) %>%
    pivot_longer(-group, names_to = "answer", values_to = "share") %>%
    mutate(answer = factor(answer, levels = rev(bequest_levels)))
  
  text_labels <- bequest_long %>%
    group_by(group) %>%
    arrange(desc(answer)) %>%
    mutate(pos = cumsum(share) - share / 2) %>%
    ungroup() %>%
    filter(share >= 0.05)
  
  plot <- ggplot(bequest_long, aes(x = group, y = share, fill = answer)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(data = text_labels,
              aes(x = group, y = pos, label = make_pct_labels(share)),
              col = "white", size = label_size_small) +
    scale_fill_manual(values = c(Yes = chart_standard_color,
                                 Possibly = "#6baed6", No = "#B3B3B3"),
                      breaks = bequest_levels) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("Who Expects to Leave an Estate?\n",
                   "Share by Wealth Tier, ", data_year)) +
    labs(x = NULL, y = "Share of Households",
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/05_bequest_expect_by_tier.jpeg"))
  
  # The gap inside the top 1%, by age: older households are the ones whose
  # estate is closest to transferring
  gap_age <- df_year %>%
    filter(top1, !is.na(bequest_expect)) %>%
    mutate(age_group = cut(age, breaks = c(-Inf, 54, older_age - 1, Inf),
                           labels = c("Under 55", paste0("55-", older_age - 1),
                                      paste0(older_age, "+")))) %>%
    group_by(age_group) %>%
    summarise(n_unw = n_distinct(hh_id),
              not_yes = wtd_share(bequest_expect != "Yes", wgt),
              says_no = wtd_share(bequest_expect == "No", wgt),
              wealth_not_yes = agg_ratio(networth * (bequest_expect != "Yes"),
                                         networth, wgt),
              .groups = "drop") %>%
    suppress_thin(c("not_yes", "says_no", "wealth_not_yes"), "5c gap by age")
  
  print("Top 1%: not expecting to leave a sizable estate, by age:")
  print(gap_age %>%
          mutate(across(c(not_yes, says_no, wealth_not_yes), make_pct_labels)) %>%
          as.data.frame())
  
  write_html_table(
    bequest_tier %>%
      transmute(Group = as.character(group),
                `Expects: yes` = make_pct_labels(Yes),
                `Expects: possibly` = make_pct_labels(Possibly),
                `Expects: no` = make_pct_labels(No)),
    paste0(out_path, "/05_bequest_by_tier_table.html"))
  
  write_html_table(
    gap_age %>%
      transmute(`Age of head` = as.character(age_group),
                `Not a firm yes` = make_pct_labels(not_yes),
                `Says no` = make_pct_labels(says_no),
                `Share of top-1% wealth held by "not yes"` = make_pct_labels(wealth_not_yes)),
    paste0(out_path, "/05_bequest_gap_top1_by_age_table.html"))
  
} else {
  message("SECTION 5c: bequest_expect not in stack - skipping.")
}

# ---- 5d: giving while living ---------------------------------------- #
# x5822 / x5823: charitable contributions in the past year and amount.
# (Check x5822's wording before calling it "$500 or more".)
if(has_vars(c("charity_500", "charity_amt"))){
  
  giving_tier <- tier_plus_30m(df %>% filter(year %in% c(prior_year, data_year))) %>%
    group_by(year, group) %>%
    summarise(n_unw = n_distinct(hh_id),
              gave           = wtd_share(charity_500 == 1, wgt),
              median_gift    = ifelse(any(charity_amt > 0),
                                      wtd_median(charity_amt[charity_amt > 0],
                                                 wgt[charity_amt > 0]),
                                      NA_real_),
              gifts_share_inc = agg_ratio(charity_amt, income, wgt),
              gifts_share_nw  = agg_ratio(charity_amt, networth, wgt),
              .groups = "drop") %>%
    suppress_thin(c("gave", "median_gift", "gifts_share_inc", "gifts_share_nw"),
                  "5d giving") %>%
    mutate(period = period_factor(year))
  
  print("Charitable giving by group:")
  print(giving_tier %>%
          mutate(gave = make_pct_labels(gave),
                 median_gift = dollar_short(median_gift),
                 gifts_share_inc = make_pct_labels(gifts_share_inc, 1),
                 gifts_share_nw = make_pct_labels(gifts_share_nw, 2)) %>%
          select(-period) %>%
          as.data.frame())
  
  plot <- ggplot(giving_tier, aes(x = group, y = gifts_share_nw, fill = period)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.75) +
    geom_text(aes(label = make_pct_labels(gifts_share_nw, 2)),
              position = position_dodge(width = 0.8), vjust = -0.5,
              col = chart_standard_color, size = label_size_small) +
    period_fill_scale() +
    scale_y_continuous(label = percent_format(accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.12))) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("Giving While Living\n",
                   "Annual Gifts / Net Worth")) +
    labs(x = NULL, y = "Charitable Gifts / Net Worth",
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/05_giving_share_of_nw.jpeg"))
  
  write_html_table(
    giving_tier %>%
      arrange(desc(year), group) %>%
      transmute(Year = as.character(year),
                Group = as.character(group),
                `Gave to charity` = make_pct_labels(gave),
                `Median gift (givers)` = dollar_short(median_gift),
                `Gifts / income` = make_pct_labels(gifts_share_inc, 1),
                `Gifts / net worth` = make_pct_labels(gifts_share_nw, 2)),
    paste0(out_path, "/05_giving_by_tier_table.html"))
  
} else {
  message("SECTION 5d: charity variables not in stack - skipping.")
}

# ---- 5e: trusts ----------------------------------------------------- #
# From the summary variable `trusts`: trusts the household has an equity
# interest in. Cannot separate trusts set up by the household from ones it
# benefits from, and likely undercounts revocable living trusts, which
# respondents often report as the underlying assets.
if(has_vars(c("trusts", "has_trust"))){
  
  trust_tier <- tier_plus_30m(df %>% filter(year %in% c(prior_year, data_year))) %>%
    group_by(year, group) %>%
    summarise(n_unw = n_distinct(hh_id),
              has_trust   = wtd_share(has_trust == 1, wgt),
              trust_share = agg_ratio(trusts, networth, wgt),
              .groups = "drop") %>%
    suppress_thin(c("has_trust", "trust_share"), "5e trusts")
  
  write_html_table(
    trust_tier %>%
      arrange(desc(year), group) %>%
      transmute(Year = as.character(year),
                Group = as.character(group),
                `Has a trust` = make_pct_labels(has_trust),
                `Trusts / net worth` = make_pct_labels(trust_share, 1)),
    paste0(out_path, "/05_trusts_by_tier_table.html"))
  
} else {
  message("SECTION 5e: trust variables not in stack - skipping.")
}

# ---- 5f: who uses professionals ------------------------------------- #
# Sources of information for saving/investment decisions. This is NOT
# "has an estate attorney" - the SCF doesn't ask that.
if(has_vars(c("ifinpro", "ifinplan"))){
  
  pro_time <- df %>%
    filter(!is.na(ifinpro)) %>%
    mutate(uses_pro = (ifinpro == 1 | ifinplan == 1)) %>%
    group_by(year, tier) %>%
    summarise(share = wtd_share(uses_pro, wgt), .groups = "drop") %>%
    mutate(tier = as.character(tier))
  
  line_with_end_labels(
    pro_time, "share", "tier", make_pct_labels, tier_colors,
    paste0("Who Relies on Professionals?\n",
           "For Saving/Investing Decisions"),
    "Share of Households",
    "05_uses_professionals_over_time.jpeg")
  
  pro_tier <- tier_plus_30m(df_year) %>%
    group_by(group) %>%
    summarise(n_unw = n_distinct(hh_id),
              planner = wtd_share(ifinplan == 1, wgt),
              pro     = wtd_share(ifinpro == 1, wgt),
              either  = wtd_share(ifinplan == 1 | ifinpro == 1, wgt),
              .groups = "drop") %>%
    suppress_thin(c("planner", "pro", "either"), "5f professionals")
  
  write_html_table(
    pro_tier %>%
      transmute(Group = as.character(group),
                `Financial planner` = make_pct_labels(planner),
                `Lawyer, accountant, banker or broker` = make_pct_labels(pro),
                `Either` = make_pct_labels(either)),
    paste0(out_path, "/05_uses_professionals_table.html"))
  
} else {
  message("SECTION 5f: ifinpro/ifinplan not in stack - skipping.")
}

# ##################################################################### #
# SECTION 6: Who the new exemption reaches
# ##################################################################### #
# Each household is compared to ITS OWN line: $30M if married, $15M if not.
# "Above the line" is an approximation of exposure: SCF net worth counts
# life insurance at cash value, excludes DB pensions, and is per household.

exempt_status <- df %>%
  group_by(year) %>%
  summarise(`Above their exemption` = sum(wgt[above_line]),
            `Within 50% below it`   = sum(wgt[near_line]),
            n_above = n_distinct(hh_id[above_line]),
            n_near  = n_distinct(hh_id[near_line]),
            .groups = "drop")

print("Households above / near their own exemption line:")
print(exempt_status %>%
        mutate(across(c(`Above their exemption`, `Within 50% below it`),
                      make_round_count)) %>%
        as.data.frame())

line_with_end_labels(
  exempt_status %>%
    select(year, `Above their exemption`, `Within 50% below it`) %>%
    pivot_longer(-year, names_to = "status", values_to = "households") %>%
    mutate(status = ifelse(status == "Above their exemption", "Above", "Near")),
  "households", "status", make_round_count,
  c(Above = chart_standard_color, Near = "#6baed6"),
  paste0("Above or Near the Exemption\n",
         "Married $30M, Single $15M"),
  "Total U.S. Households",
  "06_households_above_near_exemption.jpeg",
  y_scale = comma)

# Married vs. single, data year
exempt_marital <- df_year %>%
  mutate(status = ifelse(married_flag, "Married ($30M line)", "Single ($15M line)")) %>%
  group_by(status) %>%
  summarise(n_unw       = n_distinct(hh_id[above_line | near_line]),
            above       = sum(wgt[above_line]),
            near        = sum(wgt[near_line]),
            # Net worth above the line - a rough, pre-deduction estate base
            excess      = sum(wgt * pmax(networth - exempt_line, 0)),
            .groups = "drop") %>%
  suppress_thin(c("above", "near", "excess"), "6 exemption by marital status")

write_html_table(
  exempt_marital %>%
    transmute(Household = status,
              `Above their line` = make_round_count(above),
              `Within 50% below` = make_round_count(near),
              `Net worth above the line` = dollar_short(excess)),
  paste0(out_path, "/06_exemption_by_marital_table.html"))

# Illustrative projection: how many cross their line after N years of
# real growth? Not a forecast - no spending, gifting, deaths or new entrants.
projection <- map_dfr(growth_rates, function(g){
  df_year %>%
    summarise(households = sum(wgt[networth * (1 + g)^projection_years >= exempt_line])) %>%
    mutate(scenario = paste0(g * 100, "% real growth"))
}) %>%
  bind_rows(tibble(households = sum(df_year$wgt[df_year$above_line]),
                   scenario = paste0("Today (", data_year, ")")), .) %>%
  mutate(scenario = factor(scenario, levels = scenario))

print(paste0("Illustrative: households above their line after ",
             projection_years, " years:"))
print(projection %>% mutate(households = make_round_count(households)) %>%
        as.data.frame())

plot <- ggplot(projection, aes(x = scenario, y = households)) +
  geom_bar(stat = "identity",
           fill = c(prior_year_color, rep(chart_standard_color, length(growth_rates))),
           width = 0.6) +
  geom_text(aes(label = make_round_count(households)), vjust = -0.5,
            col = chart_standard_color, size = label_size) +
  scale_y_continuous(label = comma, expand = expansion(mult = c(0, 0.10))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("How Many Could Cross the Line?\n",
                 "After ", projection_years, " Years of Real Growth")) +
  labs(x = NULL, y = "Households Above Their Exemption",
       caption = make_caption(str_wrap(paste0(
         "Note: Illustrative only. Applies a constant real growth rate to ",
         data_year, " net worth with no spending, gifts, deaths or new ",
         "households. Exemption is inflation-indexed, so the line is held ",
         "fixed in real terms."), width = caption_wrap)))

save_chart(plot, paste0(out_path, "/06_exemption_projection.jpeg"))

# ##################################################################### #
# SECTION 7: Sanity checks
# ##################################################################### #

print("Households represented by year (should match Fed totals):")
print(df %>% group_by(year) %>%
        summarise(households = make_round_count(sum(wgt)), .groups = "drop") %>%
        as.data.frame())

print("Unweighted respondents per tier, by year:")
print(df %>% group_by(year, tier) %>%
        summarise(n = n_distinct(hh_id), .groups = "drop") %>%
        pivot_wider(names_from = tier, values_from = n) %>%
        as.data.frame())

print("Unweighted respondents at $30M+ and above own exemption, by year:")
print(df %>% group_by(year) %>%
        summarise(n_30m = n_distinct(hh_id[over_30m]),
                  n_above_line = n_distinct(hh_id[above_line]),
                  .groups = "drop") %>%
        as.data.frame())

# Top 1% should be ~1% of households by weight in every year
print("Weighted share in top 1% (should be ~1.0%):")
print(df %>% group_by(year) %>%
        summarise(share = make_pct_labels(wtd_share(top1, wgt), 2),
                  .groups = "drop") %>%
        as.data.frame())

# Known 2022 reference: top 1% net worth threshold ~ $13.7M (2022 dollars)
if(2022 %in% all_years & dollar_year == 2022){
  p99_2022 <- thresholds$nw_p99[thresholds$year == 2022]
  print(paste0("2022 top 1% threshold: ", dollar_short(p99_2022),
               " (expected ~$13.7M)"))
  if(abs(p99_2022 / 13.7e6 - 1) > 0.10){
    warning("2022 p99 is more than 10% off the ~$13.7M reference - check the build.")
  }
}

# Asset buckets should never be negative in aggregate
neg_other <- df %>% group_by(year) %>%
  summarise(other = sum(wgt * mix_other), .groups = "drop") %>%
  filter(other < 0)
if(nrow(neg_other) > 0){
  warning("'Other' asset bucket is negative in aggregate for: ",
          paste(neg_other$year, collapse = ", "),
          " - a component is double counted.")
}

if(nrow(thin_log) > 0){
  print("Suppressed cells (fewer than min_cell_n respondents):")
  print(thin_log %>% select(table, everything()) %>% as.data.frame())
} else {
  print("No cells suppressed.")
}

########################## Close the log ############################## #

cat("\nRun finished: ", format(Sys.time()), "\n", sep = "")

globalCallingHandlers(NULL)
sink()
close(log_con)

print(paste0("Log saved to: ", log_file))

# ############################  End  ################################## #