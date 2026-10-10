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
library(tidyverse)

########################## Parameters ################################# #

data_year   <- 2025
prior_year  <- 2022
dollar_year <- 2025

# Baseline year for the long comparison in the ladder census
baseline_year <- 1989

# ---------------------------------------------------------------------- #
# ON THE WEIGHTS
#
# wgt in 0003_scf_stack.Rds is a POPULATION weight: summing it across all
# five implicates gives the number of U.S. households the survey represents
# (93.0M in 1989 rising to 131.3M in 2022, which matches the Fed's published
# figures). So sum(wgt) above a threshold IS a household count. No divisor,
# no external Census merge.
#
# Do NOT divide by n_distinct(imp_id). In this build imp_id = y1, and per
# the SCF codebook Y1 is a unique RECORD id (case id x 10 + implicate), so
# n_distinct(imp_id) returns the row count, not 5. If you ever need the
# implicate number itself it is y1 - 10 * yy1.
#
# Shares are scaling-invariant either way, which is why the percentile and
# wealth-level charts were right even before this was sorted out.
# ---------------------------------------------------------------------- #

# Wealth Ladder levels, in dollar_year dollars. L4 is open-ended, so the
# levels sum to 100% without needing L5/L6.
ladder_breaks <- c(-Inf, 10^4, 10^5, 10^6, Inf)
ladder_labels <- c("L1 (<$10k)", "L2 ($10k-$100k)",
                   "L3 ($100k-$1M)", "L4 ($1M+)")
ladder_colors <- c("#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# Thresholds whose percentile rank we track over time (the ladder boundaries)
rank_thresholds <- c(10^4, 10^5, 10^6)

# Household-count-over-time charts (the WSJ/Zidar style). Two tiers: the
# millionaire tier, which is on-theme for this post, and the ultra-wealthy
# tier the WSJ used. Both run off the same helper.
count_thresholds_mill  <- c(10^6, 5 * 10^6, 10^7)
count_thresholds_ultra <- c(3 * 10^7, 5 * 10^7, 10^8)

# Two-series charts (prior year vs. latest year)
prior_year_color <- "#B3B3B3"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(data_year))))
}

# ---- Chart layout constants -------------------------------------------- #
# At 15cm wide with the blog's serif title face, ~30 characters per line is
# the most that fits. The first run clipped titles because the wrap was 40.
title_wrap   <- 30
caption_wrap <- 80

# The first run used size 1.8 for in-chart value labels, which is unreadable.
label_size_small <- 2.4   # dense charts (many bars)
label_size       <- 3.0   # normal charts

########################## Output paths ############################### #

folder_name <- "xxxx_scf_millionaire_stats"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack$year)

optional_vars <- intersect(c("homeeq", "fin", "retqliq", "liq", "nfin",
                             "bus", "vehic", "asset", "debt", "income"),
                           names(scf_stack))

message("Optional variables found: ", paste(optional_vars, collapse = ", "))

missing_optional <- setdiff(c("homeeq", "fin", "retqliq", "liq", "bus"),
                            names(scf_stack))
if(length(missing_optional) > 0){
  message("NOT in stack (some sections will be skipped): ",
          paste(missing_optional, collapse = ", "))
}

df <- scf_stack %>%
  select(all_of(unique(c("year", "hh_id", "imp_id", "agecl", "edcl", "age",
                         "networth", "wgt", optional_vars)))) %>%
  arrange(year, hh_id, imp_id)

all_years <- sort(unique(df$year))
year_min  <- min(all_years)
year_max  <- max(all_years)

# Axis breaks for every time-series chart. Built by stepping BACK from
# year_max so the latest wave always gets a tick. year_breaks
# ran 1989/1995/.../2019 and silently dropped 2022, which made the charts
# look like they ended early. Stepping back from 2022 also keeps working
# when data_year becomes 2025.
year_breaks <- sort(seq(year_max, year_min, by = -3))

df_year <- df %>% filter(year == data_year)

# Unweighted households surveyed in data_year - this is what the note string
# reports, same convention as the other SCF scripts.
n_hh <- n_distinct(df_year$hh_id)

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households. All figures are in ",
                               dollar_year, " dollars."),
                        width = caption_wrap)

# Time-series charts span every wave, so their note cannot cite one year's
# sample size.
note_string_ts <- str_wrap(paste0("Note: All figures are adjusted for inflation (",
                                  dollar_year, " dollars)."),
                           width = caption_wrap)

########################## Helper Functions ########################### #

# Weighted share of households meeting a condition. Scaling-invariant, which
# is exactly why every output in this script is a share.
wtd_share <- function(condition, weights){
  as.numeric(wtd.mean(as.numeric(condition), weights = weights))
}

make_pct_labels <- function(values, digits = 1){
  ifelse(is.na(values), "n/a",
         paste0(formatC(100 * values, format = "f", digits = digits), "%"))
}

# Rounded count for an end-of-line label: "395k", "23.6M". Keeps three
# significant figures so the label stays honest rather than rounding 185k
# up to 200k.
make_round_count <- function(values){
  ifelse(abs(values) >= 10^6,
         paste0(formatC(values/10^6, format = "f", digits = 1), "M"),
         paste0(formatC(round(values/10^3), format = "d", big.mark = ","), "k"))
}

# Household counts read better as "23.6M" than "23,600,000".
make_count_labels <- function(values, digits = 1){
  max_abs <- max(abs(values), na.rm = TRUE)
  if(max_abs >= 10^6){
    paste0(formatC(values/10^6, format = "f", digits = digits), "M")
  } else {
    paste0(formatC(values/10^3, format = "f", digits = 0), "k")
  }
}

make_pp_labels <- function(values, digits = 1){
  paste0(ifelse(values > 0, "+", ""),
         formatC(100 * values, format = "f", digits = digits), "pp")
}

# Wraps BOTH lines. The first version only wrapped the main title, so a long
# subtitle ran off the panel.
make_title <- function(main, sub, width = title_wrap){
  paste0(str_wrap(main, width = width), "\n", str_wrap(sub, width = width))
}

# Proper ordinal suffixes - "82th" and "93th" are not words.
ordinal <- function(n){
  n <- round(n)
  suffix <- ifelse(n %% 100 %in% c(11, 12, 13), "th",
                   ifelse(n %% 10 == 1, "st",
                          ifelse(n %% 10 == 2, "nd",
                                 ifelse(n %% 10 == 3, "rd", "th"))))
  paste0(n, suffix)
}

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

period_factor <- function(year_vector){
  factor(as.character(year_vector),
         levels = c(as.character(prior_year), as.character(data_year)))
}

short_dollar <- function(x){
  ifelse(x >= 10^6,
         paste0("$", formatC(x/10^6, format = "f", digits = 0), "M"),
         paste0("$", formatC(x/10^3, format = "f", digits = 0), "k"))
}

# Endpoint labels for a line chart. First label pushes right (hjust 0), last
# pushes left (hjust 1), so neither runs off the panel - the first run
# clipped "82.0th" at the right edge.
endpoint_labels <- function(data, label_vector){
  data %>%
    mutate(label = label_vector) %>%
    filter(year %in% c(min(year), max(year))) %>%
    mutate(hj = ifelse(year == min(year), 0, 1))
}

# ##################################################################### #
# SECTION 1: How many millionaires, under four definitions
# ##################################################################### #
# "Millionaire" is not one thing. The SCF gives net worth; the wealth
# management industry means investable assets. The gap between them is the
# most interesting content in the post.

millionaire_defs <- list(
  `Net worth` = function(d) d$networth >= 10^6
)

if("homeeq" %in% names(df_year)){
  millionaire_defs[["Net worth, excl. home"]] <-
    function(d) (d$networth - d$homeeq) >= 10^6
}

if("fin" %in% names(df_year)){
  millionaire_defs[["Financial assets"]] <-
    function(d) d$fin >= 10^6
}

if(all(c("fin", "retqliq") %in% names(df_year))){
  millionaire_defs[["Financial assets, excl. retirement"]] <-
    function(d) (d$fin - d$retqliq) >= 10^6
}

definition_summary <- tibble(definition = names(millionaire_defs)) %>%
  mutate(share = map_dbl(millionaire_defs,
                         ~ wtd_share(.x(df_year), df_year$wgt)),
         households = map_dbl(millionaire_defs,
                              ~ sum(df_year$wgt[.x(df_year)], na.rm = TRUE)),
         definition = factor(definition, levels = rev(names(millionaire_defs))))

print("U.S. households with $1M, by definition:")
print(definition_summary %>%
        transmute(definition,
                  share = make_pct_labels(share),
                  households = make_count_labels(households)) %>%
        as.data.frame())

text_labels <- definition_summary %>%
  mutate(label = paste0(make_pct_labels(share), "   ",
                        make_count_labels(households)))

file_path <- paste0(out_path, "/01_millionaire_share_by_definition.jpeg")

plot <- ggplot(definition_summary, aes(x = definition, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = definition, y = share, label = label),
            col = chart_standard_color,
            hjust = -0.2,
            size = label_size) +
  coord_flip() +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.40))) +
  of_dollars_and_data_theme +
  ggtitle(make_title("Who Counts as a Millionaire?",
                     paste0("Share of U.S. Households, ", data_year))) +
  labs(x = NULL, y = "Share of Households With $1M+",
       caption = make_caption())

save_chart(plot, file_path)

write_html_table(
  definition_summary %>%
    arrange(desc(share)) %>%
    transmute(Definition = as.character(definition),
              `Share of Households` = make_pct_labels(share),
              `Households` = formatC(households, format = "d",
                                     big.mark = ",")),
  paste0(out_path, "/01_millionaire_by_definition_table.html"))

# ##################################################################### #
# SECTION 2: Where $1 million ranks, over time
# ##################################################################### #
# NOTE: the percentile rank of $1M and the share of households above $1M are
# the same number flipped (rank = 1 - share). Do NOT present them as two
# separate findings - pick one framing for the post.

millionaire_time <- df %>%
  group_by(year) %>%
  summarise(share       = wtd_share(networth >= 10^6, wgt),
            households  = sum(wgt[networth >= 10^6], na.rm = TRUE),
            all_hh      = sum(wgt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pctile_of_1m = 1 - share)

# ---- Chart 2a: the percentile rank of $1M ----
lab_df <- endpoint_labels(millionaire_time,
                          ordinal(100 * millionaire_time$pctile_of_1m))

file_path <- paste0(out_path, "/02_pctile_of_1m_over_time.jpeg")

plot <- ggplot(millionaire_time, aes(x = year, y = pctile_of_1m)) +
  geom_line(col = chart_standard_color, linewidth = 0.9) +
  geom_point(data = lab_df, aes(x = year, y = pctile_of_1m),
             col = chart_standard_color, size = 1.4) +
  geom_text(data = lab_df, aes(x = year, y = pctile_of_1m, label = label),
            col = chart_standard_color,
            hjust = lab_df$hj, vjust = -1.2, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.08, 0.16))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0.08, 0.08))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Where $1 Million Ranks",
                     "Percentile of U.S. Net Worth")) +
  labs(x = "Year", y = "Percentile",
       caption = paste0(source_string, "\n",
                        str_wrap(paste0("Note: All figures in constant ",
                                        dollar_year,
                                        " dollars, so $1M means the same thing in every year. What changes is how exclusive it is."),
                                 width = caption_wrap)))

save_chart(plot, file_path)

# ---- Chart 2b: share of households worth $1M+ ----
# Same information as 2a, flipped. Produced so you can pick whichever
# framing reads better in the post - do not use both.
lab_df <- endpoint_labels(millionaire_time,
                          make_pct_labels(millionaire_time$share, 0))

file_path <- paste0(out_path, "/02_millionaire_share_over_time.jpeg")

plot <- ggplot(millionaire_time, aes(x = year, y = share)) +
  geom_line(col = chart_standard_color, linewidth = 0.9) +
  geom_point(data = lab_df, aes(x = year, y = share),
             col = chart_standard_color, size = 1.4) +
  geom_text(data = lab_df, aes(x = year, y = share, label = label),
            col = chart_standard_color,
            hjust = lab_df$hj, vjust = -1.2, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.08, 0.16))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0.08, 0.08))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Share of Households Worth $1M",
                     "Inflation-Adjusted")) +
  labs(x = "Year", y = "Share of Households",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

# ---- Chart 2c: percentile rank of every ladder boundary ----
rank_over_time <- map_dfr(rank_thresholds, function(th){
  df %>%
    group_by(year) %>%
    summarise(pctile = wtd_share(networth < th, wgt), .groups = "drop") %>%
    mutate(threshold = th)
}) %>%
  mutate(threshold_label = factor(short_dollar(threshold),
                                  levels = short_dollar(sort(rank_thresholds))))

file_path <- paste0(out_path, "/02_pctile_of_thresholds_over_time.jpeg")

plot <- ggplot(rank_over_time, aes(x = year, y = pctile,
                                   col = threshold_label)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.08, 0.10))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_color_manual(values = ladder_colors[2:4]) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Every Wealth Level Got Less Exclusive\n",
                 "Percentile Rank of Each Cutoff")) +
  labs(x = "Year", y = "Percentile",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

# ---- Chart 2d: household COUNTS above each threshold, over time ----
# The WSJ/Owen Zidar chart. No external data needed: wgt is a population
# weight, so sum(wgt) above a threshold is the household count directly.
#
# This is NOT the same story as the share chart. U.S. households grew from
# 93.0M to 131.3M over this period, so the count rises faster than the share
# and can climb even in waves where the share is flat.

make_count_over_time <- function(thresholds, file_suffix, chart_title,
                                 chart_subtitle){
  
  counts <- map_dfr(thresholds, function(th){
    df %>%
      group_by(year) %>%
      summarise(households  = sum(wgt[networth >= th], na.rm = TRUE),
                n_unweighted = n_distinct(hh_id[networth >= th]),
                .groups = "drop") %>%
      mutate(threshold = th)
  }) %>%
    mutate(threshold_label = factor(short_dollar(threshold),
                                    levels = short_dollar(sort(thresholds,
                                                               decreasing = TRUE))))
  
  # How thin is the sample behind each line? At the top thresholds this can
  # fall to a handful of households, and the SCF excludes the Forbes 400 by
  # design, so the top line is a floor rather than an estimate.
  thin <- counts %>%
    group_by(threshold_label) %>%
    summarise(min_n = min(n_unweighted),
              n_latest = n_unweighted[year == data_year],
              .groups = "drop")
  
  print(paste0("Unweighted households behind each line (", file_suffix, "):"))
  print(as.data.frame(thin))
  
  max_val <- max(counts$households)
  
  y_labels <- if(max_val >= 5 * 10^6){
    function(x) paste0(formatC(x/10^6, format = "f", digits = 0), "M")
  } else {
    comma
  }
  
  # Label the final point of each line with its value and threshold. That
  # makes the bottom legend redundant, so it comes off - the labels identify
  # the series and the chart gets the legend row back as plot area.
  end_labels <- counts %>%
    filter(year == max(year)) %>%
    mutate(label = paste0("(", threshold_label, "+)"))
  
  file_path <- paste0(out_path, "/02_household_counts_", file_suffix, ".jpeg")
  
  plot <- ggplot(counts, aes(x = year, y = households,
                             col = threshold_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    geom_text(data = end_labels,
              aes(x = year, y = households, label = label),
              hjust = -0.15, vjust = 0.4, size = label_size_small,
              show.legend = FALSE) +
    scale_y_continuous(label = y_labels,
                       expand = expansion(mult = c(0.06, 0.12))) +
    scale_x_continuous(breaks = year_breaks,
                       expand = expansion(mult = c(0.04, 0.28))) +
    scale_color_manual(values = rev(ladder_colors[2:4])) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ggtitle(paste0(chart_title, "\n", chart_subtitle)) +
    labs(x = "Year", y = "Total U.S. Households",
         caption = make_caption(note_string_ts))
  
  save_chart(plot, file_path)
  
  write_html_table(
    counts %>%
      mutate(display = make_count_labels(households)) %>%
      select(year, threshold_label, display) %>%
      pivot_wider(names_from = threshold_label, values_from = display) %>%
      rename(Year = year),
    paste0(out_path, "/02_household_counts_", file_suffix, "_table.html"))
  
  export_to_excel(df = counts,
                  outfile = paste0(out_path, "/02_household_counts_",
                                   file_suffix, ".xlsx"),
                  sheetname = "counts",
                  new_file = 1,
                  fancy_formatting = 0)
  
  invisible(counts)
}

counts_mill <- make_count_over_time(
  count_thresholds_mill, "millionaire",
  "How Many Households Are Rich?",
  "Number by Net Worth, Inflation-Adjusted")

counts_ultra <- make_count_over_time(
  count_thresholds_ultra, "ultra",
  "The Very Top Keeps Growing",
  "Number by Net Worth, Inflation-Adjusted")

write_html_table(
  millionaire_time %>%
    transmute(Year = year,
              `Share $1M+` = make_pct_labels(share),
              `$1M Percentile` = ordinal(100 * pctile_of_1m)),
  paste0(out_path, "/02_millionaire_over_time_table.html"))

export_to_excel(df = millionaire_time,
                outfile = paste0(out_path, "/02_millionaire_over_time.xlsx"),
                sheetname = "millionaires",
                new_file = 1,
                fancy_formatting = 0)

# ##################################################################### #
# SECTION 3: Wealth Ladder levels over time
# ##################################################################### #
# REPLACES the find_percentile() solver from 0459. That function iterated
# percentile guesses in 0.001 steps until a weighted quantile landed near a
# dollar target - slow, approximate, and it could stall. The share of
# households above a threshold is a weighted mean of an indicator: exact and
# instant.
#
# The 0459 version also built level shares with lag() across an ungrouped
# frame, which only worked because of row ordering from the nested loop.

ladder <- df %>%
  mutate(level = cut(networth,
                     breaks = ladder_breaks,
                     labels = ladder_labels,
                     right = FALSE))

stopifnot(sum(is.na(ladder$level)) == 0)

ladder_by_year <- ladder %>%
  group_by(year, level) %>%
  summarise(wgt_sum = sum(wgt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = wgt_sum / sum(wgt_sum)) %>%
  ungroup() %>%
  rename(households = wgt_sum) %>%
  select(year, level, share, households)

# L4 is open-ended, so shares sum to 1 in every year.
ladder_check <- ladder_by_year %>%
  group_by(year) %>%
  summarise(total_share = sum(share), .groups = "drop")

stopifnot(all(abs(ladder_check$total_share - 1) < 1e-8))

file_path <- paste0(out_path, "/03_wealth_levels_over_time.jpeg")

plot <- ggplot(ladder_by_year, aes(x = year, y = share, fill = level)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = ladder_colors) +
  scale_y_continuous(label = percent, expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = year_breaks) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(make_title("U.S. Wealth Levels Over Time",
                     paste0("Share of Households, ", year_min, "-",
                            year_max))) +
  labs(x = "Year", y = "Percentage of Households",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

export_to_excel(df = ladder_by_year,
                outfile = paste0(out_path, "/03_wealth_levels_by_year.xlsx"),
                sheetname = "scf",
                new_file = 1,
                fancy_formatting = 0)

# ##################################################################### #
# SECTION 4: Ladder census - which rungs grew?
# ##################################################################### #

census_years <- intersect(c(baseline_year, prior_year, data_year), all_years)

ladder_census <- ladder_by_year %>%
  filter(year %in% census_years) %>%
  mutate(display = paste0(make_pct_labels(share), " (",
                          make_count_labels(households), ")")) %>%
  select(level, year, display) %>%
  pivot_wider(names_from = year, values_from = display) %>%
  rename(`Wealth Level` = level)

write_html_table(ladder_census,
                 paste0(out_path, "/04_wealth_level_census_table.html"))

print("Wealth Ladder census (share of households):")
print(as.data.frame(ladder_census))

# ---- Chart: prior vs. latest, share by level ----
ladder_two_year <- ladder_by_year %>%
  filter(year %in% c(prior_year, data_year)) %>%
  mutate(period = period_factor(year))

text_labels <- ladder_two_year %>%
  mutate(label = make_pct_labels(share, 0))

file_path <- paste0(out_path, "/04_wealth_levels_", prior_year, "_vs_",
                    data_year, ".jpeg")

plot <- ggplot(ladder_two_year, aes(x = level, y = share, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(data = text_labels,
            aes(x = level, y = share, label = label, group = period),
            position = position_dodge(width = 0.9),
            col = chart_standard_color,
            vjust = -0.5, size = label_size_small) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  period_fill_scale() +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Households on Each Wealth Level\n", prior_year, " vs. ", data_year)) +
  labs(x = NULL, y = "Share of Households",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

# ---- Chart: change in SHARE by level, in percentage points ----
# Percentage points, not percent change: with shares this small at the top
# rungs, a percent change would exaggerate movement at L4.
ladder_change <- ladder_by_year %>%
  filter(year %in% c(prior_year, data_year)) %>%
  mutate(year_label = ifelse(year == data_year, "latest", "prior")) %>%
  select(level, year_label, share) %>%
  pivot_wider(names_from = year_label, values_from = share) %>%
  mutate(pp_change = latest - prior)

text_labels <- ladder_change %>%
  mutate(label = make_pp_labels(pp_change))

file_path <- paste0(out_path, "/04_wealth_level_share_change.jpeg")

plot <- ggplot(ladder_change, aes(x = level, y = pp_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_hline(yintercept = 0, col = "black", linewidth = 0.3) +
  geom_text(data = text_labels, aes(x = level, y = pp_change, label = label),
            col = chart_standard_color,
            vjust = ifelse(text_labels$pp_change > 0, -0.5, 1.5),
            size = label_size) +
  scale_y_continuous(label = function(x) paste0(100 * x, "pp"),
                     expand = expansion(mult = c(0.16, 0.16))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Which Rungs Got More Crowded?",
                     paste0("Change in Share, ", prior_year, "-", data_year))) +
  labs(x = NULL, y = "Change in Share of Households",
       caption = make_caption(note_string_ts))

save_chart(plot, file_path)

write_html_table(
  ladder_change %>%
    transmute(`Wealth Level` = as.character(level),
              Prior  = make_pct_labels(prior),
              Latest = make_pct_labels(latest),
              `Change` = make_pp_labels(pp_change)) %>%
    rename(!!as.character(prior_year) := Prior,
           !!as.character(data_year)  := Latest),
  paste0(out_path, "/04_wealth_level_share_change_table.html"))

# ##################################################################### #
# SECTION 5: Who are the millionaires?
# ##################################################################### #

df_mill <- df_year %>% filter(networth >= 10^6)

n_mill_unweighted <- n_distinct(df_mill$hh_id)
message("Unweighted millionaire households in sample: ", n_mill_unweighted)

# ---- 5a: age distribution, millionaires vs. all households ----
age_compare <- bind_rows(
  df_year %>%
    group_by(agecl) %>%
    summarise(wgt_sum = sum(wgt), .groups = "drop") %>%
    mutate(share = wgt_sum / sum(wgt_sum), group = "All households"),
  df_mill %>%
    group_by(agecl) %>%
    summarise(wgt_sum = sum(wgt), .groups = "drop") %>%
    mutate(share = wgt_sum / sum(wgt_sum), group = "Millionaires")
) %>%
  mutate(group = factor(group, levels = c("All households", "Millionaires")))

file_path <- paste0(out_path, "/05_millionaire_age_distribution.jpeg")

plot <- ggplot(age_compare, aes(x = agecl, y = share, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = c(prior_year_color, chart_standard_color)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Millionaire Households Skew Old\nAge Distribution, ", data_year)) +
  labs(x = "Age", y = "Share of Group",
       caption = make_caption())

save_chart(plot, file_path)

# ---- 5b: millionaire RATE by age ----
mill_rate_by_age <- df_year %>%
  group_by(agecl) %>%
  summarise(share = wtd_share(networth >= 10^6, wgt), .groups = "drop")

text_labels <- mill_rate_by_age %>%
  mutate(label = make_pct_labels(share, 0))

file_path <- paste0(out_path, "/05_millionaire_rate_by_age.jpeg")

plot <- ggplot(mill_rate_by_age, aes(x = agecl, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = agecl, y = share, label = label),
            col = chart_standard_color, vjust = -0.5, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Your Odds of Being a Millionaire\nShare of Each Age Group, ", data_year)) +
  labs(x = "Age", y = "Share Worth $1M+",
       caption = make_caption())

save_chart(plot, file_path)

write_html_table(
  mill_rate_by_age %>%
    transmute(Age = as.character(agecl),
              `Share Worth $1M+` = make_pct_labels(share)),
  paste0(out_path, "/05_millionaire_rate_by_age_table.html"))

# ---- 5c: what is millionaire wealth actually made of? ----
# Aggregate shares (sum of weighted component / sum of weighted net worth),
# so this reads as "of all millionaire wealth, how much sits in X".
composition_vars <- intersect(c("homeeq", "retqliq", "bus", "vehic", "liq"),
                              names(df_mill))

if(length(composition_vars) > 0){
  
  pretty_component <- c(homeeq = "Home equity", retqliq = "Retirement accounts",
                        bus = "Private business", vehic = "Vehicles",
                        liq = "Cash")
  
  total_nw <- sum(df_mill$wgt * df_mill$networth, na.rm = TRUE)
  
  composition <- tibble(component = composition_vars) %>%
    mutate(share = map_dbl(component,
                           ~ sum(df_mill$wgt * df_mill[[.x]], na.rm = TRUE) /
                             total_nw),
           label_name = pretty_component[component])
  
  composition <- bind_rows(
    composition,
    tibble(component = "other", share = 1 - sum(composition$share),
           label_name = "Everything else")
  ) %>%
    mutate(label_name = factor(label_name, levels = label_name[order(share)]))
  
  text_labels <- composition %>%
    mutate(label = make_pct_labels(share, 0))
  
  file_path <- paste0(out_path, "/05_millionaire_wealth_composition.jpeg")
  
  plot <- ggplot(composition, aes(x = label_name, y = share)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels, aes(x = label_name, y = share, label = label),
              col = chart_standard_color, hjust = -0.2, size = label_size) +
    coord_flip() +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.18))) +
    of_dollars_and_data_theme +
    ggtitle(paste0("What Millionaire Wealth Is Made Of\nShare of Total Net Worth, ", data_year)) +
    labs(x = NULL, y = "Share of Millionaire Net Worth",
         caption = make_caption())
  
  save_chart(plot, file_path)
  
  write_html_table(
    composition %>%
      arrange(desc(share)) %>%
      transmute(Component = as.character(label_name),
                `Share of Net Worth` = make_pct_labels(share)),
    paste0(out_path, "/05_millionaire_composition_table.html"))
}

# ---- 5d: how much is locked up? ----
if(all(c("homeeq", "retqliq") %in% names(df_mill))){
  
  liquidity <- df_mill %>%
    mutate(illiquid_share = (homeeq + retqliq) / networth) %>%
    group_by(agecl) %>%
    summarise(median_illiquid = as.numeric(wtd.quantile(illiquid_share,
                                                        weights = wgt,
                                                        probs = 0.5)),
              .groups = "drop")
  
  text_labels <- liquidity %>%
    mutate(label = make_pct_labels(median_illiquid, 0))
  
  file_path <- paste0(out_path, "/05_millionaire_illiquid_share.jpeg")
  
  plot <- ggplot(liquidity, aes(x = agecl, y = median_illiquid)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = agecl, y = median_illiquid, label = label),
              col = chart_standard_color, vjust = -0.5, size = label_size) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Most Millionaire Wealth Is Locked Up\n","Median Share in Home + Retirement, ", data_year)) +
    labs(x = "Age", y = "Share of Net Worth",
         caption = make_caption())
  
  save_chart(plot, file_path)
}

# ##################################################################### #
# SECTION 6: Sanity checks
# ##################################################################### #

print(paste0("Output folder: ", out_path))
print(paste0("Households surveyed in ", data_year, ": ",
             formatC(n_hh, format = "d", big.mark = ",")))

print("U.S. households represented by the weights, by year:")
print(millionaire_time %>%
        transmute(year, households = make_count_labels(all_hh)) %>%
        as.data.frame())
print("  (should read ~93M in 1989 rising to ~131M in 2022)")

mill_row <- definition_summary %>% filter(definition == "Net worth")
print(paste0("Households worth $1M+ in ", data_year, ": ",
             make_count_labels(mill_row$households),
             " (", make_pct_labels(mill_row$share), ")"))

print(paste0("$1M sits at the ",
             ordinal(100 * (millionaire_time %>%
                              filter(year == data_year) %>%
                              pull(pctile_of_1m))),
             " percentile in ", data_year, " vs. the ",
             ordinal(100 * (millionaire_time %>%
                              filter(year == year_min) %>%
                              pull(pctile_of_1m))),
             " in ", year_min))

print(paste0("Unweighted millionaire households in sample: ",
             n_mill_unweighted))
print("  (if this is under ~200, treat the Section 5 sub-cuts with care)")

print("Wealth level shares sum to 1 in every year:")
print(as.data.frame(ladder_check))

# ############################  End  ################################## #