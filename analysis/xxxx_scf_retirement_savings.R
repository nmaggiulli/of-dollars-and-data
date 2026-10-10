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
library(mitools)
library(Hmisc)
library(xtable)
library(tidyverse)

########################## Parameters ################################# #

data_year   <- 2025
prior_year  <- 2022
dollar_year <- 2025

# ---------------------------------------------------------------------- #
# WHAT THIS SCRIPT MEASURES, AND WHAT IT CANNOT
#
# RETQLIQ = quasi-liquid retirement assets = IRAKH + THRIFT + FUTPEN +
# CURRPEN. It captures DC accounts COMPLETELY: a current 401(k), an old
# 401(k) left with a past employer, rollover IRAs, a TSP, a 403(b). Someone
# who job-hopped five times gets counted once, in full.
#
# What it does NOT capture is the VALUE of a traditional defined-benefit
# pension. The SCF gives a coverage flag but no dollar figure. That
# understates retirement resources, and not evenly - it hits older cohorts,
# public sector workers and union households hardest.
#
# So the post's scope is RETIREMENT ACCOUNT BALANCES, not total retirement
# resources. Section 5 splits the sample on DB coverage, which turns the
# limitation into the finding: for households with no pension - most of the
# private sector now - RETQLIQ IS their full retirement wealth, with no gap
# at all.
#
# Social Security is missing from all of this too, and for the median
# household it is worth more than any 401(k) balance. That belongs in the
# post's closing section, not in the code.
# ---------------------------------------------------------------------- #

# Widely cited savings-multiple rule of thumb (salary multiples by age).
# These are the Fidelity milestones; VERIFY before publishing and swap for
# whichever benchmark you want to argue against.
benchmark_multiples <- c(
  `20-24` = NA,   # no published milestone this young
  `25-29` = NA,
  `30-34` = 1,
  `35-39` = 2,
  `40-44` = 3,
  `45-49` = 4,
  `50-54` = 6,
  `55-59` = 7,
  `60-64` = 8,
  `65-69` = 10,
  `70-74` = 10,
  `75-80` = 10
)

# Savings multiples are meaningless at very low income (dividing by $3,000
# of income produces nonsense). Households below this are excluded from the
# multiple charts only - they stay in every balance chart.
income_floor <- 20000

# Percentiles for the distribution charts
dist_probs <- c(0.25, 0.50, 0.75, 0.90)

# Two-series charts (prior year vs. latest year, or split groups)
prior_year_color <- "#B3B3B3"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(data_year))))
}

two_group_fill <- function(){
  scale_fill_manual(values = c(prior_year_color, chart_standard_color))
}

# ---- Chart layout constants -------------------------------------------- #
title_wrap   <- 30
caption_wrap <- 80

label_size_small <- 2.4   # dense charts (12 age bands)
label_size       <- 3.0   # normal charts (6 age buckets)

########################## Output paths ############################### #

folder_name <- "xxxx_scf_retirement_savings"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack$year)
stopifnot("retqliq" %in% names(scf_stack))

# Optional variables. The script degrades gracefully if the build does not
# carry them - each dependent section checks first.
optional_vars <- intersect(c("fin", "liq", "homeeq", "networth",
                             "irakh", "thrift", "futpen", "currpen",
                             "dbplant", "dbplancj", "dcplancj"),
                           names(scf_stack))

message("Optional variables found: ", paste(optional_vars, collapse = ", "))

# Which DB pension flag is available? DBPLANT is broader (current job OR a
# pension from a past job), so prefer it.
db_flag <- intersect(c("dbplant", "dbplancj"), names(scf_stack))[1]

if(is.na(db_flag)){
  message("No DB pension flag (dbplant/dbplancj) in the stack - Section 5 ",
          "will be skipped. That section is the honest counterweight to the ",
          "headline numbers, so consider adding the flag to build 0003.")
} else {
  message("Using DB pension flag: ", db_flag)
}

df <- scf_stack %>%
  select(all_of(unique(c("year", "hh_id", "imp_id", "agecl", "edcl", "age",
                         "income", "retqliq", "wgt", optional_vars)))) %>%
  arrange(year, hh_id, imp_id)

all_years <- sort(unique(df$year))
year_min  <- min(all_years)
year_max  <- max(all_years)

########################## Derived measures ########################### #

add_agecl_new <- function(data){
  data %>%
    filter(age >= 20, age <= 80) %>%
    mutate(agecl_new = case_when(age < 25 ~ "20-24",
                                 age < 30 ~ "25-29",
                                 age < 35 ~ "30-34",
                                 age < 40 ~ "35-39",
                                 age < 45 ~ "40-44",
                                 age < 50 ~ "45-49",
                                 age < 55 ~ "50-54",
                                 age < 60 ~ "55-59",
                                 age < 65 ~ "60-64",
                                 age < 70 ~ "65-69",
                                 age < 75 ~ "70-74",
                                 TRUE     ~ "75-80"),
           agecl_new = factor(agecl_new,
                              levels = c("20-24", "25-29", "30-34", "35-39",
                                         "40-44", "45-49", "50-54", "55-59",
                                         "60-64", "65-69", "70-74", "75-80")))
}

df <- df %>%
  mutate(
    has_ret = retqliq > 0,
    # STRICT: retirement accounts only.
    ret_strict = retqliq,
    # EXPANSIVE: retirement accounts plus non-retirement financial assets.
    # Your retirement spending will not come only out of retirement accounts.
    ret_expansive = if("fin" %in% names(.)) pmax(fin, retqliq) else NA_real_
  )

if(!is.na(db_flag)){
  df <- df %>%
    mutate(has_db = .data[[db_flag]] %in% c(1, TRUE, "1", "Yes", "yes"),
           db_group = factor(ifelse(has_db, "Has a pension", "No pension"),
                             levels = c("Has a pension", "No pension")))
}

df_year <- df %>% filter(year == data_year)
df_fine <- df_year %>% add_agecl_new()

n_hh <- n_distinct(df_year$hh_id)

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households. All figures are in ",
                               dollar_year, " dollars."),
                        width = caption_wrap)

note_string_ts <- str_wrap(paste0("Note: All figures are adjusted for inflation (",
                                  dollar_year, " dollars)."),
                           width = caption_wrap)

note_accounts <- str_wrap(paste0("Note: Retirement accounts include IRAs, 401(k)s and similar plans, but NOT the value of a traditional pension. All figures in ",
                                 dollar_year, " dollars."),
                          width = caption_wrap)

########################## Helper Functions ########################### #

# Weighted share of households meeting a condition. Scaling-invariant, so
# it does not matter that the weights are survey-scaled.
wtd_share <- function(condition, weights){
  as.numeric(wtd.mean(as.numeric(condition), weights = weights))
}

wtd_stat <- function(x, w, quantile_prob){
  if(quantile_prob == 0){
    as.numeric(wtd.mean(x, weights = w))
  } else {
    as.numeric(wtd.quantile(x, weights = w, probs = quantile_prob))
  }
}

summarise_by <- function(data, var, group_vars, quantile_prob){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(value = wtd_stat(.data[[var]], wgt, quantile_prob),
              .groups = "drop")
}

# Dollar labels. One format decision per vector, negatives handled with a
# "-$" prefix rather than letting formatC emit the minus inside the "$".
make_dollar_labels <- function(values){
  max_abs <- max(abs(values), na.rm = TRUE)
  sign_prefix <- ifelse(values < 0, "-$", "$")
  
  out <- if(max_abs >= 10^6){
    paste0(sign_prefix, formatC(abs(values)/10^6, big.mark = ",",
                                format = "f", digits = 1), "M")
  } else {
    paste0(sign_prefix, formatC(abs(values)/10^3, big.mark = ",",
                                format = "f", digits = 0), "k")
  }
  
  ifelse(round(values, 0) == 0, "$0", out)
}

make_pct_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(formatC(100 * values, format = "f", digits = digits), "%"))
}

make_x_labels <- function(values, digits = 1){
  ifelse(is.na(values), "n/a",
         paste0(formatC(values, format = "f", digits = digits), "x"))
}

# NOTE: chart titles are written as explicit paste0() strings with a hard
# "\n" rather than going through str_wrap. Wrapping decided the line breaks
# at render time, which put breaks in bad places ("Median Retirement Savings
# by / Age"). Each line below is kept to ~34 characters, which is what fits
# at 15cm in the blog's serif title face.

make_caption <- function(note = note_string){
  paste0(source_string, "\n", note)
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

# ##################################################################### #
# SECTION 1: What people actually have
# ##################################################################### #
# The headline numbers, and the single most important framing choice in the
# post: median ACROSS ALL HOUSEHOLDS includes the zeros. That is a very
# different number from the median among households that have an account,
# and most articles quote one without saying which.

overall_median_all    <- wtd_stat(df_year$retqliq, df_year$wgt, 0.5)
overall_median_owners <- wtd_stat(df_year$retqliq[df_year$has_ret],
                                  df_year$wgt[df_year$has_ret], 0.5)
overall_mean_all      <- wtd_stat(df_year$retqliq, df_year$wgt, 0)

print(paste0("Median retirement account balance, ALL households: ",
             format_as_dollar(overall_median_all)))
print(paste0("Median among households WITH an account: ",
             format_as_dollar(overall_median_owners)))
print(paste0("Mean, ALL households: ", format_as_dollar(overall_mean_all)))

balances_by_age <- bind_rows(
  summarise_by(df_fine, "retqliq", "agecl_new", 0.5) %>%
    mutate(group = "All households"),
  df_fine %>%
    filter(has_ret) %>%
    summarise_by("retqliq", "agecl_new", 0.5) %>%
    mutate(group = "Households with an account")
) %>%
  mutate(group = factor(group, levels = c("All households",
                                          "Households with an account")))

text_labels <- balances_by_age %>%
  mutate(label = make_dollar_labels(value))

file_path <- paste0(out_path, "/01_median_balance_by_age.jpeg")

plot <- ggplot(balances_by_age, aes(x = agecl_new, y = value, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(data = text_labels,
            aes(x = agecl_new, y = value, label = label, group = group),
            position = position_dodge(width = 0.9),
            col = chart_standard_color, vjust = -0.5, size = label_size_small) +
  scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.14))) +
  two_group_fill() +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Median Retirement Savings by Age\n",
                 "With and Without the Zeros, ", data_year)) +
  labs(x = "Age", y = "Retirement Account Balance",
       caption = make_caption(note_accounts))

save_chart(plot, file_path)

# ---- Table: mean and median side by side, both bases ----
balance_table <- df_fine %>%
  group_by(agecl_new) %>%
  summarise(
    median_all    = wtd_stat(retqliq, wgt, 0.5),
    mean_all      = wtd_stat(retqliq, wgt, 0),
    median_owners = if(sum(wgt[has_ret]) > 0){
      wtd_stat(retqliq[has_ret], wgt[has_ret], 0.5)
    } else NA_real_,
    pct_with      = wtd_share(has_ret, wgt),
    .groups = "drop"
  )

write_html_table(
  balance_table %>%
    transmute(`Age Range` = as.character(agecl_new),
              `Median (All)`    = format_as_dollar(median_all),
              `Median (Savers)` = format_as_dollar(median_owners),
              `Average (All)`   = format_as_dollar(mean_all),
              `% With Account`  = make_pct_labels(pct_with)),
  paste0(out_path, "/01_balance_by_age_table.html"))

print("Retirement balances by age:")
print(balance_table %>%
        transmute(agecl_new,
                  median_all = format_as_dollar(median_all),
                  median_owners = format_as_dollar(median_owners),
                  pct_with = make_pct_labels(pct_with)) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 2: Who even has a retirement account?
# ##################################################################### #
# The "should you have saved X" question presumes access. Roughly half of
# households do not have a retirement account at all, and that is heavily
# tilted by income - which reframes the whole benchmark conversation.

participation_by_age <- df_fine %>%
  group_by(agecl_new) %>%
  summarise(share = wtd_share(has_ret, wgt), .groups = "drop")

text_labels <- participation_by_age %>%
  mutate(label = make_pct_labels(share))

file_path <- paste0(out_path, "/02_participation_by_age.jpeg")

plot <- ggplot(participation_by_age, aes(x = agecl_new, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = agecl_new, y = share, label = label),
            col = chart_standard_color, vjust = -0.5, size = label_size_small) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Who Has a Retirement Account?\n",
                 "Share With Any Balance, ", data_year)) +
  labs(x = "Age", y = "Share of Households",
       caption = make_caption(note_accounts))

save_chart(plot, file_path)

# ---- Participation by income quintile ----
# Weighted income quintile within data_year.
df_quintile <- df_year %>%
  arrange(income) %>%
  mutate(cum_wgt = cumsum(wgt) / sum(wgt),
         income_group = case_when(
           cum_wgt <= 0.20 ~ "Bottom 20%",
           cum_wgt <= 0.40 ~ "20-40%",
           cum_wgt <= 0.60 ~ "40-60%",
           cum_wgt <= 0.80 ~ "60-80%",
           TRUE            ~ "Top 20%"),
         income_group = factor(income_group,
                               levels = c("Bottom 20%", "20-40%", "40-60%",
                                          "60-80%", "Top 20%")))

participation_by_income <- df_quintile %>%
  group_by(income_group) %>%
  summarise(share = wtd_share(has_ret, wgt),
            median_balance = wtd_stat(retqliq, wgt, 0.5),
            .groups = "drop")

text_labels <- participation_by_income %>%
  mutate(label = make_pct_labels(share))

file_path <- paste0(out_path, "/02_participation_by_income.jpeg")

plot <- ggplot(participation_by_income, aes(x = income_group, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = income_group, y = share, label = label),
            col = chart_standard_color, vjust = -0.5, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Saving Is an Income Story\n",
                 "Share With an Account, ", data_year)) +
  labs(x = "Household Income", y = "Share of Households",
       caption = make_caption(note_accounts))

save_chart(plot, file_path)

write_html_table(
  participation_by_income %>%
    transmute(`Income Group` = as.character(income_group),
              `% With Account` = make_pct_labels(share),
              `Median Balance` = format_as_dollar(median_balance)),
  paste0(out_path, "/02_participation_by_income_table.html"))

# ##################################################################### #
# SECTION 3: The spread
# ##################################################################### #
# A single median is a bad benchmark. The gap between the 25th and 90th
# percentile within one age band is the real story.

dist_by_age <- df_fine %>%
  group_by(agecl) %>%
  group_modify(~ tibble(prob = dist_probs,
                        value = as.numeric(wtd.quantile(.x$retqliq,
                                                        weights = .x$wgt,
                                                        probs = dist_probs)))) %>%
  ungroup() %>%
  mutate(key = factor(paste0(formatC(100 * prob, format = "f", digits = 0),
                             "th Percentile"),
                      levels = paste0(formatC(100 * dist_probs, format = "f",
                                              digits = 0),
                                      "th Percentile")))

# Labels formatted PER FACET - a $1M facet should not force "$0.0M" labels
# onto a facet whose values are in the thousands.
text_labels <- dist_by_age %>%
  group_by(key) %>%
  mutate(label = make_dollar_labels(value)) %>%
  ungroup()

file_path <- paste0(out_path, "/03_distribution_by_age.jpeg")

plot <- ggplot(dist_by_age, aes(x = agecl, y = value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = agecl, y = value, label = label),
            col = chart_standard_color, vjust = -0.4,
            size = label_size_small) +
  # ggplot2's own facet_wrap repeats axes on every panel. lemon's
  # facet_rep_wrap breaks on current ggplot2 versions.
  facet_wrap(vars(key), scales = "free_y", axes = "all") +
  scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.18))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("The Spread Is Enormous\n",
                 "Retirement Balances by Age, ", data_year)) +
  labs(x = "Age", y = "Retirement Account Balance",
       caption = make_caption(note_accounts))

save_chart(plot, file_path)

write_html_table(
  dist_by_age %>%
    mutate(display = format_as_dollar(value)) %>%
    select(agecl, key, display) %>%
    pivot_wider(names_from = key, values_from = display) %>%
    rename(Age = agecl),
  paste0(out_path, "/03_distribution_by_age_table.html"))

# ##################################################################### #
# SECTION 4: The "should" part - actual vs. the rule of thumb
# ##################################################################### #
# The title asks "should," the data only knows "do." This section is the
# bridge: compare what households actually have against the salary-multiple
# rule everyone quotes.
#
# Uses the median of household-level ratios, not the ratio of medians. The
# ratio of medians silently pairs a household's balance with a DIFFERENT
# household's income.

df_multiple <- df_fine %>%
  filter(income >= income_floor) %>%
  mutate(savings_multiple = retqliq / income)

multiple_by_age <- df_multiple %>%
  group_by(agecl_new) %>%
  summarise(actual_multiple = as.numeric(wtd.quantile(savings_multiple,
                                                      weights = wgt,
                                                      probs = 0.5)),
            .groups = "drop") %>%
  mutate(benchmark = benchmark_multiples[as.character(agecl_new)])

to_plot <- multiple_by_age %>%
  select(agecl_new, Actual = actual_multiple, Benchmark = benchmark) %>%
  pivot_longer(cols = c(Actual, Benchmark),
               names_to = "series", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(series = factor(series, levels = c("Benchmark", "Actual")))

text_labels <- to_plot %>%
  mutate(label = make_x_labels(value))

file_path <- paste0(out_path, "/04_savings_multiple_vs_benchmark.jpeg")

plot <- ggplot(to_plot, aes(x = agecl_new, y = value, fill = series)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(data = text_labels,
            aes(x = agecl_new, y = value, label = label, group = series),
            position = position_dodge(width = 0.9),
            col = chart_standard_color, vjust = -0.5, size = label_size_small) +
  scale_y_continuous(label = function(x) paste0(x, "x"),
                     expand = expansion(mult = c(0, 0.16))) +
  two_group_fill() +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Nobody Is Hitting the Benchmark\n",
                 "Multiple of Income, ", data_year)) +
  labs(x = "Age", y = "Multiple of Household Income",
       caption = make_caption(str_wrap(paste0("Note: Median household-level ratio of retirement accounts to income, among households earning at least ",
                                              format_as_dollar(income_floor),
                                              ". Benchmark is a widely cited salary-multiple rule of thumb."),
                                       width = caption_wrap)))

save_chart(plot, file_path)

# ---- What share of households actually MEET the benchmark? ----
meets_benchmark <- df_multiple %>%
  mutate(benchmark = benchmark_multiples[as.character(agecl_new)]) %>%
  filter(!is.na(benchmark)) %>%
  group_by(agecl_new) %>%
  summarise(share_meeting = wtd_share(savings_multiple >= benchmark, wgt),
            .groups = "drop")

text_labels <- meets_benchmark %>%
  mutate(label = make_pct_labels(share_meeting))

file_path <- paste0(out_path, "/04_share_meeting_benchmark.jpeg")

plot <- ggplot(meets_benchmark, aes(x = agecl_new, y = share_meeting)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels,
            aes(x = agecl_new, y = share_meeting, label = label),
            col = chart_standard_color, vjust = -0.5, size = label_size_small) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Who Actually Hits the Target?\n",
                 "Share Meeting the Benchmark, ", data_year)) +
  labs(x = "Age", y = "Share of Households",
       caption = make_caption(str_wrap(paste0("Note: Among households earning at least ",
                                              format_as_dollar(income_floor),
                                              ". Retirement accounts only - excludes pension value and Social Security."),
                                       width = caption_wrap)))

save_chart(plot, file_path)

write_html_table(
  multiple_by_age %>%
    left_join(meets_benchmark, by = "agecl_new") %>%
    transmute(`Age Range` = as.character(agecl_new),
              `Benchmark` = make_x_labels(benchmark),
              `Actual (Median)` = make_x_labels(actual_multiple),
              `% Meeting It` = make_pct_labels(share_meeting)),
  paste0(out_path, "/04_savings_multiple_table.html"))

print("Actual vs. benchmark savings multiple:")
print(multiple_by_age %>%
        transmute(agecl_new,
                  benchmark = make_x_labels(benchmark),
                  actual = make_x_labels(actual_multiple)) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 5: The pension caveat
# ##################################################################### #
# The honest counterweight. Low balances look less alarming once you see who
# has a pension behind them - and for households WITHOUT one, RETQLIQ is
# their complete retirement wealth, so the numbers above have no gap at all.

if(!is.na(db_flag)){
  
  # ---- 5a: DB coverage by age ----
  db_by_age <- df_fine %>%
    group_by(agecl_new) %>%
    summarise(share = wtd_share(has_db, wgt), .groups = "drop")
  
  text_labels <- db_by_age %>%
    mutate(label = make_pct_labels(share))
  
  file_path <- paste0(out_path, "/05_db_coverage_by_age.jpeg")
  
  plot <- ggplot(db_by_age, aes(x = agecl_new, y = share)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels, aes(x = agecl_new, y = share, label = label),
              col = chart_standard_color, vjust = -0.5,
              size = label_size_small) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.14))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Who Still Has a Pension?\n",
                   "Share With a DB Plan, ", data_year)) +
    labs(x = "Age", y = "Share of Households",
         caption = make_caption())
  
  save_chart(plot, file_path)
  
  # ---- 5b: balances split on pension status ----
  # Expect households WITH a pension to hold LESS in accounts - they did not
  # need to accumulate as much. Same "low" balance, entirely different
  # meaning. This is the chart that reframes the post.
  balance_by_db <- df_fine %>%
    group_by(agecl, db_group) %>%
    summarise(value = wtd_stat(retqliq, wgt, 0.5), .groups = "drop")
  
  text_labels <- balance_by_db %>%
    mutate(label = make_dollar_labels(value))
  
  file_path <- paste0(out_path, "/05_balance_by_pension_status.jpeg")
  
  plot <- ggplot(balance_by_db, aes(x = agecl, y = value, fill = db_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(data = text_labels,
              aes(x = agecl, y = value, label = label, group = db_group),
              position = position_dodge(width = 0.9),
              col = chart_standard_color, vjust = -0.5,
              size = label_size_small) +
    scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.14))) +
    two_group_fill() +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("A Pension Changes the Math\n",
                   "Median Account Balance, ", data_year)) +
    labs(x = "Age", y = "Retirement Account Balance",
         caption = make_caption(note_accounts))
  
  save_chart(plot, file_path)
  
  # ---- 5c: the structural shift over time ----
  db_over_time <- df %>%
    group_by(year) %>%
    summarise(share = wtd_share(has_db, wgt), .groups = "drop")
  
  file_path <- paste0(out_path, "/05_db_coverage_over_time.jpeg")
  
  lab_df <- db_over_time %>%
    filter(year %in% c(min(year), max(year))) %>%
    mutate(label = make_pct_labels(share),
           hj = ifelse(year == min(year), 0, 1))
  
  plot <- ggplot(db_over_time, aes(x = year, y = share)) +
    geom_line(col = chart_standard_color, linewidth = 0.9) +
    geom_point(data = lab_df, aes(x = year, y = share),
               col = chart_standard_color, size = 1.4) +
    geom_text(data = lab_df, aes(x = year, y = share, label = label, hjust = hj),
              col = chart_standard_color,
              vjust = -1.2, size = label_size) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.08, 0.16))) +
    scale_x_continuous(breaks = seq(year_min, year_max, 3),
                       expand = expansion(mult = c(0.08, 0.08))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("The Pension Disappeared\n",
                   "Share of Households Covered")) +
    labs(x = "Year", y = "Share of Households",
         caption = make_caption(note_string_ts))
  
  save_chart(plot, file_path)
  
  write_html_table(
    db_by_age %>%
      transmute(`Age Range` = as.character(agecl_new),
                `% With a Pension` = make_pct_labels(share)),
    paste0(out_path, "/05_db_coverage_table.html"))
}

# ##################################################################### #
# SECTION 6: Strict vs. expansive
# ##################################################################### #
# Retirement spending will not come only out of retirement accounts. The
# honest answer is a range: accounts alone (strict) versus all financial
# assets (expansive).

if("fin" %in% names(df_fine)){
  
  range_by_age <- bind_rows(
    summarise_by(df_fine, "ret_strict", "agecl", 0.5) %>%
      mutate(measure = "Retirement accounts only"),
    summarise_by(df_fine, "ret_expansive", "agecl", 0.5) %>%
      mutate(measure = "All financial assets")
  ) %>%
    mutate(measure = factor(measure,
                            levels = c("Retirement accounts only",
                                       "All financial assets")))
  
  text_labels <- range_by_age %>%
    mutate(label = make_dollar_labels(value))
  
  file_path <- paste0(out_path, "/06_strict_vs_expansive.jpeg")
  
  plot <- ggplot(range_by_age, aes(x = agecl, y = value, fill = measure)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(data = text_labels,
              aes(x = agecl, y = value, label = label, group = measure),
              position = position_dodge(width = 0.9),
              col = chart_standard_color, vjust = -0.5,
              size = label_size_small) +
    scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.14))) +
    two_group_fill() +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("A Floor and a Ceiling\n",
                   "Median Resources by Age, ", data_year)) +
    labs(x = "Age", y = "Median Balance",
         caption = make_caption(str_wrap(paste0("Note: The expansive measure adds non-retirement financial assets. Neither includes home equity, pension value or Social Security. All figures in ",
                                                dollar_year, " dollars."),
                                         width = caption_wrap)))
  
  save_chart(plot, file_path)
}

# ##################################################################### #
# SECTION 7: Has it gotten better or worse?
# ##################################################################### #

balance_over_time <- df %>%
  add_agecl_new() %>%
  group_by(year, agecl) %>%
  summarise(median_all = wtd_stat(retqliq, wgt, 0.5),
            pct_with   = wtd_share(has_ret, wgt),
            .groups = "drop")

file_path <- paste0(out_path, "/07_median_balance_over_time_by_age.jpeg")

plot <- ggplot(balance_over_time, aes(x = year, y = median_all)) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  facet_wrap(vars(agecl), scales = "free_y", axes = "all") +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(breaks = seq(year_min, year_max, 9)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Retirement Savings Over Time\n",
                 "Median Balance, All Households")) +
  labs(x = "Year", y = "Retirement Account Balance",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

file_path <- paste0(out_path, "/07_participation_over_time_by_age.jpeg")

plot <- ggplot(balance_over_time, aes(x = year, y = pct_with)) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  facet_wrap(vars(agecl), axes = "all") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(year_min, year_max, 9)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Who Has an Account, Over Time\n",
                 "Share With Any Balance")) +
  labs(x = "Year", y = "Share of Households",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

# ---- Prior vs. latest, median balance by age ----
balance_two_year <- balance_over_time %>%
  filter(year %in% c(prior_year, data_year)) %>%
  mutate(period = factor(as.character(year),
                         levels = c(as.character(prior_year),
                                    as.character(data_year))))

text_labels <- balance_two_year %>%
  mutate(label = make_dollar_labels(median_all))

file_path <- paste0(out_path, "/07_balance_", prior_year, "_vs_",
                    data_year, ".jpeg")

plot <- ggplot(balance_two_year, aes(x = agecl, y = median_all,
                                     fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(data = text_labels,
            aes(x = agecl, y = median_all, label = label, group = period),
            position = position_dodge(width = 0.9),
            col = chart_standard_color, vjust = -0.5,
            size = label_size_small) +
  scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.14))) +
  period_fill_scale() +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Median Retirement Balances by Age\n",
                 prior_year, " vs. ", data_year)) +
  labs(x = "Age", y = "Median Balance",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

export_to_excel(df = balance_over_time,
                outfile = paste0(out_path, "/07_balance_over_time.xlsx"),
                sheetname = "retirement",
                new_file = 1,
                fancy_formatting = 0)

# ##################################################################### #
# SECTION 8: Sanity checks
# ##################################################################### #

print(paste0("Output folder: ", out_path))
print(paste0("Households surveyed in ", data_year, ": ",
             formatC(n_hh, format = "d", big.mark = ",")))

print(paste0("Share of households with ANY retirement account: ",
             make_pct_labels(wtd_share(df_year$has_ret, df_year$wgt), 1)))

# If your build carries the components, confirm they add to RETQLIQ. A
# mismatch means the build is summing something different than expected.
component_cols <- intersect(c("irakh", "thrift", "futpen", "currpen"),
                            names(df_year))

if(length(component_cols) == 4){
  check <- df_year %>%
    mutate(component_sum = irakh + thrift + futpen + currpen,
           diff = abs(component_sum - retqliq)) %>%
    summarise(max_diff = max(diff, na.rm = TRUE),
              n_mismatch = sum(diff > 1, na.rm = TRUE))
  print(paste0("RETQLIQ vs. IRAKH+THRIFT+FUTPEN+CURRPEN - max diff: ",
               formatC(check$max_diff, format = "f", digits = 2),
               ", rows off by >$1: ", check$n_mismatch))
} else {
  print(paste0("Component check skipped (need irakh/thrift/futpen/currpen; ",
               "found: ",
               ifelse(length(component_cols) == 0, "none",
                      paste(component_cols, collapse = ", ")), ")"))
}

if(!is.na(db_flag)){
  print(paste0("DB pension coverage in ", data_year, ": ",
               make_pct_labels(wtd_share(df_year$has_db, df_year$wgt), 1),
               " (using ", db_flag, ")"))
  print(paste0("DB coverage in ", year_min, ": ",
               make_pct_labels(db_over_time %>%
                                 filter(year == year_min) %>%
                                 pull(share), 1)))
} else {
  print("No DB flag in the stack - Section 5 was skipped.")
}

print(paste0("Households excluded from the multiple charts (income below ",
             format_as_dollar(income_floor), "): ",
             make_pct_labels(wtd_share(df_fine$income < income_floor,
                                       df_fine$wgt), 1)))

# ############################  End  ################################## #