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
library(tidyverse)

# ##################################################################### #
# HOW MANY AMERICANS ARE MILLIONAIRES?
#
#   Section 1: How many millionaires, under four definitions
#   Section 2: Where $1M ranks over time, and household counts
#   Section 3: Wealth Ladder levels over time
#   Section 4: Ladder census - which levels grew?
#   Section 5: Who are the millionaires?
#   Section 6: New charts (06_ onward) - candidates for review
#   Section 7: Key facts for review (log only, no charts)
#   Section 8: Sanity checks
#
# Everything printed to the console also goes to a log file in out_path.
# HTML tables are switched off for now. Add them back per chart as needed.
# ##################################################################### #

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

# Household-count-over-time chart (the WSJ/Zidar style), millionaire tier.
count_thresholds_mill  <- c(10^6, 5 * 10^6, 10^7)

# Net worth bands WITHIN millionaires (used in the band composition chart
# and the log)
mill_band_breaks <- c(10^6, 2 * 10^6, 5 * 10^6, 10^7, Inf)
mill_band_labels <- c("$1M-$2M", "$2M-$5M", "$5M-$10M", "$10M+")
mill_band_short  <- c("$1-2M", "$2-5M", "$5-10M", "$10M+")   # chart axis only

# SCF age classes, in age order
age_levels <- c("<35", "35-44", "45-54", "55-64", "65-74", "75+")

# Extra labeled points on the $1M share line chart (pre-crisis peak and
# post-crisis low). Set to NULL to show endpoints only.
share_turn_years <- c(2007, 2013)

# Line colors for the four millionaire definitions over time
def_colors <- c("#08306b", "#2171b5", "#6baed6", "#969696")

# Two-series charts (prior year vs. latest year)
prior_year_color <- "#B3B3B3"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(data_year))))
}

# ---- Composition buckets ----------------------------------------------- #
# These buckets add up to net worth EXACTLY:
#   - Home equity is net of primary-home loans (homeeq = houses - mrthel)
#   - Every other asset bucket is gross
#   - "Other assets" = asset minus houses minus every listed gross bucket
#   - "Debt (ex home loans)" = -(debt - (houses - homeeq))
# Algebra: homeeq + (asset - houses) - debt + (houses - homeeq) = networth
comp_parts <- list(
  `Home equity`         = c("homeeq"),
  `Retirement accounts` = c("retqliq"),
  `Stocks & funds`      = c("stocks", "nmmf"),
  `Private business`    = c("bus"),
  `Other real estate`   = c("oresre", "nnresre"),
  `Cash`                = c("liq"),
  `CDs & bonds`         = c("cds", "bond", "savbnd"),
  `Annuities & trusts`  = c("othma"),
  `Life insurance`      = c("cashli"),
  `Vehicles`            = c("vehic")
)
comp_other_label <- "Other assets"
comp_debt_label  <- "Debt (ex home loans)"
comp_levels      <- c(names(comp_parts), comp_other_label, comp_debt_label)

# Panels on the band composition chart. Everything else is summed into one
# net panel.
band_panels      <- c("Home equity", "Retirement accounts", "Stocks & funds",
                      "Private business", "Other real estate")
band_other_label <- "Everything else (net)"

# ---- Chart layout constants -------------------------------------------- #
# At 15cm wide with the blog's serif title face, ~30 characters per line is
# the most that fits. The first run clipped titles because the wrap was 40.
title_wrap   <- 30
caption_wrap <- 80

# Charts with plot.caption.position = "plot" (01, 06 and 08) start the caption
# at the left edge of the image, not the panel, so the note can run wider
# before it wraps.
caption_wrap_wide <- 110

# The first run used size 1.8 for in-chart value labels, which is unreadable.
label_size_small <- 2.4   # dense charts (many bars)
label_size       <- 3.0   # normal charts

########################## Output paths ############################### #

folder_name <- "0527_scf_millionaire_stats"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Console log ################################ #
# Everything printed to the console also goes to a text file in out_path.
# If a previous run died mid-script its sink is still open - close it first.
# Needs R 4.0+.

while(sink.number() > 0) sink()
globalCallingHandlers(NULL)

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
    ", baseline_year = ", baseline_year, ", dollar_year = ", dollar_year,
    "\n\n", sep = "")

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack$year)

comp_inputs <- setdiff(unlist(comp_parts), "homeeq")

optional_vars <- intersect(unique(c("homeeq", "fin", "retqliq", "liq", "nfin",
                                    "bus", "vehic", "asset", "debt", "income",
                                    "houses", comp_inputs)),
                           names(scf_stack))

message("Optional variables found: ", paste(optional_vars, collapse = ", "))

missing_optional <- setdiff(c("homeeq", "fin", "retqliq", "liq", "bus",
                              "houses", "income", "asset", "debt"),
                            names(scf_stack))
if(length(missing_optional) > 0){
  message("NOT in stack (some sections will be skipped): ",
          paste(missing_optional, collapse = ", "))
}

df <- scf_stack %>%
  select(all_of(unique(c("year", "hh_id", "imp_id", "agecl", "edcl", "age",
                         "networth", "wgt", optional_vars)))) %>%
  arrange(year, hh_id, imp_id)

# Composition inputs that are missing count as 0, so the bucket math still
# runs. Whatever they held lands in "Other assets".
missing_comp <- setdiff(comp_inputs, names(df))
if(length(missing_comp) > 0){
  message("Composition inputs not in stack (treated as 0, absorbed by 'Other assets'): ",
          paste(missing_comp, collapse = ", "))
  for(v in missing_comp){ df[[v]] <- 0 }
}

can_do_composition <- all(c("homeeq", "houses", "asset", "debt") %in% names(df))

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

note_text <- paste0("Note:  Calculations based on weighted data from ",
                    formatC(n_hh, digits = 0, format = "f", big.mark = ","),
                    " U.S. households. All figures are in ",
                    dollar_year, " dollars.")

note_string      <- str_wrap(note_text, width = caption_wrap)
note_string_wide <- str_wrap(note_text, width = caption_wrap_wide)

# Definition line added to the notes on the definition charts (01, 06, 07)
liquid_def_text <- paste0("Liquid net worth is non-retirement financial assets ",
                          "minus all debts other than mortgage debt.")

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

# Caption with an extra wrapped line under the standard note
make_caption_extra <- function(extra, note = note_string, width = caption_wrap){
  make_caption(paste0(note, "\n", str_wrap(extra, width = width)))
}

# Every chart file gets "_<data_year>" before its extension so WordPress can
# tell the 2022 and 2025 versions apart (same helper as 0524).
add_year_suffix <- function(file_path){
  sub("(\\.[A-Za-z0-9]+)$", paste0("_", data_year, "\\1"), file_path)
}

save_chart <- function(plot, file_path){
  ggsave(add_year_suffix(file_path), plot, width = 15, height = 12, units = "cm")
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

# Hand-placed line breaks for the definition names on bar-chart axes. The
# automatic wrap split the "(ex ...)" qualifier.
def_axis_label <- function(x){
  recode(x,
         "Net worth (ex home)" = "Net worth\n(ex home)",
         "Net worth (ex home & retirement)" = "Net worth\n(ex home &\nretirement)")
}

# Puts SCF age classes in age order. Leaves a factor alone.
order_age <- function(x){
  if(is.character(x)){
    factor(x, levels = c(intersect(age_levels, unique(x)),
                         setdiff(unique(x), age_levels)))
  } else {
    x
  }
}

# "1 in 10.2" - easier to say in prose than "9.8%"
one_in <- function(share){
  ifelse(is.na(share) | share <= 0, "n/a",
         paste0("1 in ", formatC(1 / share, format = "f", digits = 1)))
}

pct_chg_label <- function(new_value, old_value){
  ifelse(old_value > 0,
         paste0(ifelse(new_value >= old_value, "+", ""),
                formatC(100 * (new_value / old_value - 1),
                        format = "f", digits = 1), "%"),
         "n/a")
}

# Adds one column per composition bucket to a household-level frame
add_components <- function(d){
  d <- as.data.frame(d)
  for(p in names(comp_parts)){
    d[[p]] <- rowSums(as.matrix(d[, comp_parts[[p]], drop = FALSE]))
  }
  gross_listed <- d$houses +
    rowSums(as.matrix(d[, setdiff(names(comp_parts), "Home equity"), drop = FALSE]))
  d[[comp_other_label]] <- d$asset - gross_listed
  d[[comp_debt_label]]  <- -(d$debt - (d$houses - d$homeeq))
  d
}

# Aggregate composition: weighted sum of each bucket / weighted sum of net
# worth. Reads as "of all the net worth in this group, how much sits in X".
make_composition <- function(d){
  d <- add_components(d)
  total_nw <- sum(d$wgt * d$networth, na.rm = TRUE)
  tibble(component = comp_levels,
         share = map_dbl(comp_levels,
                         ~ sum(d$wgt * d[[.x]], na.rm = TRUE) / total_nw))
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
  millionaire_defs[["Net worth (ex home)"]] <-
    function(d) (d$networth - d$homeeq) >= 10^6
}

if(all(c("homeeq", "retqliq") %in% names(df_year))){
  millionaire_defs[["Net worth (ex home & retirement)"]] <-
    function(d) (d$networth - d$homeeq - d$retqliq) >= 10^6
}

# Liquid net worth: non-retirement financial assets minus all debts EXCEPT
# home loans. The home is already excluded (bar 2 removes home equity, which
# is net of the mortgage), so subtracting the mortgage here would count it
# twice. Exception: if the home is underwater, the shortfall is still a debt
# and is subtracted (same treatment as the SEC accredited-investor test).
#   home loans       = houses - homeeq
#   non-home debt    = debt - (houses - homeeq)
#   underwater gap   = max(0, -homeeq)
#   liquid assets    = asset - nfin - retqliq
if(all(c("asset", "nfin", "retqliq", "debt", "houses", "homeeq") %in% names(df_year))){
  millionaire_defs[["Liquid net worth"]] <-
    function(d) (d$asset - d$nfin - d$retqliq -
                   (d$debt - (d$houses - d$homeeq)) -
                   pmax(0, -d$homeeq)) >= 10^6
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
  mutate(label = paste0(make_pct_labels(share), " (",
                        make_count_labels(households), ")"))

file_path <- paste0(out_path, "/millionaire_share_by_definition.jpeg")

# Fix for clipping: wrap the long category labels so the panel gets more
# width, anchor the caption to the whole plot, and pad the right margin so
# the last axis tick is not cut off.
plot <- ggplot(definition_summary, aes(x = definition, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = definition, y = share, label = label),
            col = chart_standard_color,
            hjust = -0.2,
            size = label_size) +
  coord_flip() +
  scale_x_discrete(labels = def_axis_label) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.40))) +
  of_dollars_and_data_theme +
  theme(plot.caption.position = "plot",
        plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5)) +
  # paste0, not make_title: make_title wraps at 30 characters, which would
  # split this subtitle onto a second line.
  ggtitle(paste0("Who Counts as a Millionaire?\n",
                 "Share and Number of U.S. Households, ", data_year)) +
  labs(x = NULL, y = "Share of Households With $1M+",
       caption = make_caption_extra(liquid_def_text, note = note_string_wide,
                                    width = caption_wrap_wide))

save_chart(plot, file_path)

# ##################################################################### #
# SECTION 2: Where $1 million ranks, over time
# ##################################################################### #
# The percentile rank of $1M is the share flipped (rank = 1 - share). Only
# the share chart is produced; the rank is printed in the log (7a).

millionaire_time <- df %>%
  group_by(year) %>%
  summarise(share       = wtd_share(networth >= 10^6, wgt),
            households  = sum(wgt[networth >= 10^6], na.rm = TRUE),
            all_hh      = sum(wgt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pctile_of_1m = 1 - share)

# ---- Chart 2b: share of households worth $1M+ ----
# Endpoints plus the turning points in share_turn_years. A turning point
# that is a local high gets its label above the line; a low gets it below.
lab_df <- endpoint_labels(millionaire_time,
                          make_pct_labels(millionaire_time$share, 0))

turn_df <- millionaire_time %>%
  arrange(year) %>%
  mutate(prev = lag(share), nxt = lead(share)) %>%
  filter(year %in% share_turn_years) %>%
  mutate(label = make_pct_labels(share, 0),
         vj    = ifelse(share >= prev & share >= nxt, -1.2, 1.9))

file_path <- paste0(out_path, "/millionaire_share_over_time.jpeg")

plot <- ggplot(millionaire_time, aes(x = year, y = share)) +
  geom_line(col = chart_standard_color, linewidth = 0.9) +
  geom_point(data = lab_df, aes(x = year, y = share),
             col = chart_standard_color, size = 1.4) +
  geom_text(data = lab_df, aes(x = year, y = share, label = label),
            col = chart_standard_color,
            hjust = lab_df$hj, vjust = -1.2, size = label_size) +
  geom_point(data = turn_df, aes(x = year, y = share),
             col = chart_standard_color, size = 1.4) +
  geom_text(data = turn_df, aes(x = year, y = share, label = label),
            col = chart_standard_color,
            vjust = turn_df$vj, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.10, 0.16))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0.08, 0.08))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Share of Households Worth $1M+",
                     "Inflation-Adjusted")) +
  labs(x = "Year", y = "Share of Households",
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
    function(x) ifelse(x == 0, "0",
                       paste0(formatC(x/10^6, format = "f", digits = 0), "M"))
  } else {
    comma
  }
  
  # Label the final point of each line with its value and threshold. That
  # makes the bottom legend redundant, so it comes off - the labels identify
  # the series and the chart gets the legend row back as plot area.
  end_labels <- counts %>%
    filter(year == max(year)) %>%
    mutate(label = paste0(make_round_count(households),
                          " (", threshold_label, "+)"))
  
  file_path <- paste0(out_path, "/household_counts_", file_suffix, ".jpeg")
  
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
                       expand = expansion(mult = c(0.04, 0.24))) +
    scale_color_manual(values = rev(ladder_colors[2:4])) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ggtitle(paste0(chart_title, "\n", chart_subtitle)) +
    labs(x = "Year", y = "Total U.S. Households",
         caption = make_caption(note_string_ts))
  
  save_chart(plot, file_path)
  
  # Every value behind the chart, every year (replaces the old .xlsx export)
  print(paste0("Households at each threshold, every survey year (", file_suffix, "):"))
  print(counts %>%
          mutate(display = make_round_count(households)) %>%
          select(year, threshold_label, display) %>%
          pivot_wider(names_from = threshold_label, values_from = display) %>%
          rename(Year = year) %>%
          as.data.frame())
  
  invisible(counts)
}

counts_mill <- make_count_over_time(
  count_thresholds_mill, "millionaire",
  "How Many Millionaire Households Are There?",
  "Number by Net Worth, Inflation-Adjusted")


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

file_path <- paste0(out_path, "/wealth_levels_over_time.jpeg")

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

# Every value behind the chart, every year (replaces the old .xlsx export)
print("Wealth Ladder share (households) by level, every survey year:")
print(ladder_by_year %>%
        mutate(display = paste0(make_pct_labels(share), " (",
                                make_round_count(households), ")")) %>%
        select(year, level, display) %>%
        pivot_wider(names_from = level, values_from = display) %>%
        rename(Year = year) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 4: Ladder census - which levels grew?
# ##################################################################### #

census_years <- intersect(c(baseline_year, prior_year, data_year), all_years)

ladder_census <- ladder_by_year %>%
  filter(year %in% census_years) %>%
  mutate(display = paste0(make_pct_labels(share), " (",
                          make_count_labels(households), ")")) %>%
  select(level, year, display) %>%
  pivot_wider(names_from = year, values_from = display) %>%
  rename(`Wealth Level` = level)

print("Wealth Ladder census (share of households):")
print(as.data.frame(ladder_census))

# ##################################################################### #
# SECTION 5: Who are the millionaires?
# ##################################################################### #

df_mill <- df_year %>% filter(networth >= 10^6)

n_mill_unweighted <- n_distinct(df_mill$hh_id)
message("Unweighted millionaire households in sample: ", n_mill_unweighted)

# ---- 5a: age distribution, millionaires vs. all households (log only) ----
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

# ---- 5b: millionaire RATE by age ----
mill_rate_by_age <- df_year %>%
  group_by(agecl) %>%
  summarise(share = wtd_share(networth >= 10^6, wgt), .groups = "drop")

text_labels <- mill_rate_by_age %>%
  mutate(label = make_pct_labels(share, 0))

file_path <- paste0(out_path, "/millionaire_rate_by_age.jpeg")

plot <- ggplot(mill_rate_by_age, aes(x = agecl, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = agecl, y = share, label = label),
            col = chart_standard_color, vjust = -0.5, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Your Odds of Being a Millionaire\nShare of Each Age Group, ", data_year)) +
  labs(x = "Age", y = "Share Worth $1M+",
       caption = make_caption())

save_chart(plot, file_path)

# ---- 5c: what is millionaire wealth made of? (log only) ----
# Every bucket in comp_parts, adding up to net worth exactly (see
# "Composition buckets" in Parameters). Aggregate share, so the richest
# millionaires carry the most weight - chart 08 splits it by net worth band.
if(can_do_composition){
  
  composition <- make_composition(df_mill)
  
  message("Composition shares sum to ", round(sum(composition$share), 4),
          " (should be 1)")
}

# ---- 5d: how much is in the home and retirement accounts? (log only) ----
if(all(c("homeeq", "retqliq") %in% names(df_mill))){
  
  liquidity <- df_mill %>%
    mutate(illiquid_share = (homeeq + retqliq) / networth) %>%
    group_by(agecl) %>%
    summarise(median_illiquid = as.numeric(wtd.quantile(illiquid_share,
                                                        weights = wgt,
                                                        probs = 0.5)),
              .groups = "drop")
}

# ##################################################################### #
# SECTION 6: New charts (06_ onward) - candidates for review
# ##################################################################### #

nw_mill_flag <- df_year$networth >= 10^6

# ---- 06: how many millionaires pass each stricter test ----
funnel <- tibble(definition = names(millionaire_defs),
                 share = unname(map_dbl(millionaire_defs,
                                        ~ wtd_share(.x(df_year)[nw_mill_flag],
                                                    df_year$wgt[nw_mill_flag])))) %>%
  mutate(definition = factor(definition, levels = rev(names(millionaire_defs))),
         label = make_pct_labels(share, 0))

file_path <- paste0(out_path, "/millionaire_hh_definition_shares.jpeg")

plot <- ggplot(funnel, aes(x = definition, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label), col = chart_standard_color,
            hjust = -0.2, size = label_size) +
  coord_flip() +
  scale_x_discrete(labels = def_axis_label) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     limits = c(0, 1.1), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0))) +
  of_dollars_and_data_theme +
  theme(plot.caption.position = "plot",
        plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5)) +
  ggtitle(paste0("How Many Millionaires Fit Each Definition?\n",
                 "Households Worth $1M+, ", data_year)) +
  labs(x = NULL, y = "Share of Households With $1M+ on This Measure",
       caption = make_caption_extra(liquid_def_text, note = note_string_wide,
                                    width = caption_wrap_wide))

save_chart(plot, file_path)

# ---- 07: all four definitions over time ----
def_time <- map_dfr(all_years, function(yr){
  d <- df %>% filter(year == yr)
  tibble(year = yr,
         definition = names(millionaire_defs),
         share = unname(map_dbl(millionaire_defs, ~ wtd_share(.x(d), d$wgt))),
         households = unname(map_dbl(millionaire_defs,
                                     ~ sum(d$wgt[.x(d)], na.rm = TRUE))))
}) %>%
  mutate(definition = factor(definition, levels = names(millionaire_defs)))

end_df <- def_time %>%
  filter(year == year_max) %>%
  mutate(label = make_pct_labels(share, 0))

file_path <- paste0(out_path, "/millionaire_definitions_over_time.jpeg")

plot <- ggplot(def_time, aes(x = year, y = share, col = definition)) +
  geom_line(linewidth = 0.9) +
  geom_text(data = end_df, aes(label = label),
            hjust = -0.3, size = label_size_small, show.legend = FALSE) +
  scale_color_manual(values = setNames(def_colors[seq_along(millionaire_defs)],
                                       names(millionaire_defs))) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = year_breaks,
                     expand = expansion(mult = c(0.03, 0.10))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.spacing.y = unit(0, "pt"),
        legend.key.height = unit(0.35, "cm"),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  ggtitle(paste0("Millionaires Under Four Definitions\n",
                 "Share of U.S. Households")) +
  labs(x = "Year", y = "Share of Households With $1M+",
       caption = make_caption_extra(liquid_def_text, note = note_string_ts))

save_chart(plot, file_path)

# ---- 08: composition by net worth band ----
# Same buckets as 5c, computed within each band. Five main buckets get their
# own panel; the rest (including the negative debt bucket) sum into one net
# panel.
if(can_do_composition){
  
  df_mill_band <- df_mill %>%
    mutate(band = cut(networth, breaks = mill_band_breaks,
                      labels = mill_band_labels, right = FALSE))
  
  band_comp <- df_mill_band %>%
    group_by(band) %>%
    group_modify(~ make_composition(.x)) %>%
    ungroup()
  
  band_plot <- band_comp %>%
    mutate(panel = ifelse(component %in% band_panels, component,
                          band_other_label)) %>%
    group_by(band, panel) %>%
    summarise(share = sum(share), .groups = "drop") %>%
    mutate(panel = factor(panel, levels = c(band_panels, band_other_label)),
           band  = factor(as.character(band), levels = mill_band_labels,
                          labels = mill_band_short),
           label = make_pct_labels(share, 0),
           vj    = ifelse(share >= 0, -0.4, 1.3))
  
  file_path <- paste0(out_path, "/millionaire_composition_by_band.jpeg")
  
  plot <- ggplot(band_plot, aes(x = band, y = share)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(aes(label = label, vjust = vj),
              col = chart_standard_color, size = label_size_small) +
    geom_hline(yintercept = 0, col = "black", linewidth = 0.3) +
    facet_wrap(vars(panel), nrow = 2) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.08, 0.15))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(size = 7),
          plot.caption.position = "plot") +
    ggtitle(paste0("What Millionaires Own, by Net Worth\n",
                   "Share of Each Group's Net Worth, ", data_year)) +
    labs(x = "Net Worth", y = "Share of Net Worth",
         caption = make_caption_extra(
           "Home equity is net of home loans. 'Everything else' includes all other debt.",
           note = note_string_wide))
  
  save_chart(plot, file_path)
}

# ---- 09: how millionaires split by net worth band ----
# Share of all $1M+ households in each band, so the bars sum to 100%. Reads
# as "X% of millionaires have $1M-$2M".
mill_band_share <- df_mill %>%
  mutate(band = cut(networth, breaks = mill_band_breaks,
                    labels = mill_band_labels, right = FALSE)) %>%
  group_by(band) %>%
  summarise(households = sum(wgt), .groups = "drop") %>%
  mutate(share = households / sum(households),
         label = paste0(make_pct_labels(share, 0), "\n(",
                        make_round_count(households), ")"))

stopifnot(abs(sum(mill_band_share$share) - 1) < 1e-8)

file_path <- paste0(out_path, "/millionaire_share_by_band.jpeg")

plot <- ggplot(mill_band_share, aes(x = band, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label), col = chart_standard_color,
            vjust = -0.3, size = label_size, lineheight = 0.9) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("How Rich Are Millionaires?\n",
                 "Share of $1M+ Households by Net Worth, ", data_year)) +
  labs(x = "Net Worth", y = "Share of Millionaire Households",
       caption = make_caption())

save_chart(plot, file_path)

# ##################################################################### #
# SECTION 7: Key facts for review (log only - no charts)
# ##################################################################### #
# Two kinds of output:
#   - Results the sections above compute but never print
#   - Cuts that could become talking points or charts
# No education cuts, no standard errors.

cat("\n==================== KEY FACTS FOR REVIEW ====================\n\n")

key_years <- intersect(c(baseline_year, prior_year, data_year), all_years)

# ---- 7a: $1M+ households in every survey year ----
cat("---- 7a: Households worth $1M+ (net worth), every survey year ----\n")
print(millionaire_time %>%
        transmute(Year = year,
                  `Share $1M+` = make_pct_labels(share),
                  `1 in X` = one_in(share),
                  `$1M+ households` = make_round_count(households),
                  `All households` = make_round_count(all_hh),
                  `$1M percentile` = make_pct_labels(pctile_of_1m)) %>%
        as.data.frame())

# ---- 7b: all four definitions, key years ----
def_by_year <- def_time %>% filter(year %in% key_years)

cat("\n---- 7b: $1M+ under each definition, share (households) ----\n")
print(def_by_year %>%
        mutate(display = paste0(make_pct_labels(share), " (",
                                make_round_count(households), ")")) %>%
        select(definition, year, display) %>%
        pivot_wider(names_from = year, values_from = display) %>%
        rename(Definition = definition) %>%
        as.data.frame())

cat("\nChange in households, ", prior_year, "-", data_year, " and ",
    baseline_year, "-", data_year, ":\n", sep = "")
print(def_by_year %>%
        group_by(definition) %>%
        summarise(chg_recent = pct_chg_label(households[year == data_year],
                                             households[year == prior_year]),
                  chg_long   = pct_chg_label(households[year == data_year],
                                             households[year == baseline_year]),
                  .groups = "drop") %>%
        rename(Definition = definition,
               !!paste0(prior_year, "-", data_year) := chg_recent,
               !!paste0(baseline_year, "-", data_year) := chg_long) %>%
        as.data.frame())

# ---- 7c: overlap between definitions (same data as chart 06) ----
cat("\n---- 7c: Of net-worth millionaires in ", data_year,
    ", share who ALSO clear $1M on each definition ----\n", sep = "")
print(funnel %>%
        arrange(desc(share)) %>%
        transmute(Definition = as.character(definition),
                  Share = make_pct_labels(share),
                  `Do NOT clear it` = make_pct_labels(1 - share)) %>%
        as.data.frame())

# ---- 7d: growth in the $1M+ count, split into two parts ----
# Count = share x total households. The midpoint split is exact:
#   change in count = (change in share x avg households)
#                   + (change in households x avg share)
decompose_growth <- function(y0, y1){
  a <- millionaire_time %>% filter(year == y0)
  b <- millionaire_time %>% filter(year == y1)
  total      <- b$households - a$households
  from_pop   <- (b$all_hh - a$all_hh) * (a$share + b$share) / 2
  from_share <- (b$share - a$share) * (a$all_hh + b$all_hh) / 2
  tibble(Period = paste0(y0, "-", y1),
         Start = make_round_count(a$households),
         End = make_round_count(b$households),
         Change = make_round_count(total),
         `From more households` = make_round_count(from_pop),
         `From higher share` = make_round_count(from_share),
         `Pct from share` = make_pct_labels(from_share / total))
}

cat("\n---- 7d: Why the $1M+ count changed (more households vs. higher share) ----\n")
print(bind_rows(decompose_growth(prior_year, data_year),
                decompose_growth(baseline_year, data_year)) %>%
        as.data.frame())

# ---- 7e: the $1M / $5M / $10M tiers in the key years ----
tier_tbl <- map_dfr(count_thresholds_mill, function(th){
  df %>%
    filter(year %in% key_years) %>%
    group_by(year) %>%
    summarise(share = wtd_share(networth >= th, wgt),
              households = sum(wgt[networth >= th], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(threshold = th)
}) %>%
  mutate(Tier = factor(paste0(short_dollar(threshold), "+"),
                       levels = paste0(short_dollar(sort(count_thresholds_mill)), "+")))

cat("\n---- 7e: Share (households) and 1-in-X at each tier ----\n")
print(tier_tbl %>%
        mutate(display = paste0(make_pct_labels(share, 2), " (",
                                make_round_count(households), "; ",
                                one_in(share), ")")) %>%
        select(Tier, year, display) %>%
        pivot_wider(names_from = year, values_from = display) %>%
        as.data.frame())

cat("\nChange in households by tier:\n")
print(tier_tbl %>%
        group_by(Tier) %>%
        summarise(chg_recent = pct_chg_label(households[year == data_year],
                                             households[year == prior_year]),
                  chg_long   = pct_chg_label(households[year == data_year],
                                             households[year == baseline_year]),
                  .groups = "drop") %>%
        rename(!!paste0(prior_year, "-", data_year) := chg_recent,
               !!paste0(baseline_year, "-", data_year) := chg_long) %>%
        as.data.frame())

# ---- 7f: the full net worth distribution ----
dist_probs <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 0.999)

dist_tbl <- df %>%
  filter(year %in% key_years) %>%
  group_by(year) %>%
  group_modify(~ tibble(
    stat  = c(paste0("P", 100 * dist_probs), "Mean"),
    value = c(as.numeric(wtd.quantile(.x$networth, weights = .x$wgt,
                                      probs = dist_probs)),
              as.numeric(wtd.mean(.x$networth, weights = .x$wgt))))) %>%
  ungroup()

cat("\n---- 7f: Net worth by percentile (for context on where $1M sits) ----\n")
print(dist_tbl %>%
        mutate(value = format_as_dollar(value)) %>%
        pivot_wider(names_from = year, values_from = value) %>%
        rename(Statistic = stat) %>%
        as.data.frame())

cat("\nMean / median ratio:\n")
print(dist_tbl %>%
        group_by(year) %>%
        summarise(`Mean / median` = formatC(value[stat == "Mean"] /
                                              value[stat == "P50"],
                                            format = "f", digits = 1),
                  .groups = "drop") %>%
        as.data.frame())

# ---- 7g: almost-millionaires, and the spread above $1M ----
near_bands <- tibble(lo = c(5 * 10^5, 7.5 * 10^5),
                     band = c("$500k-$1M", "$750k-$1M"))

cat("\n---- 7g: Households close to $1M ----\n")
print(map_dfr(seq_len(nrow(near_bands)), function(i){
  df %>%
    filter(year %in% c(prior_year, data_year)) %>%
    group_by(year) %>%
    summarise(share = wtd_share(networth >= near_bands$lo[i] &
                                  networth < 10^6, wgt),
              households = sum(wgt[networth >= near_bands$lo[i] &
                                     networth < 10^6], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Band = near_bands$band[i])
}) %>%
  mutate(display = paste0(make_pct_labels(share), " (",
                          make_round_count(households), ")")) %>%
  select(Band, year, display) %>%
  pivot_wider(names_from = year, values_from = display) %>%
  as.data.frame())

cat("\nHow millionaires split by net worth (share of $1M+ households):\n")
print(df %>%
        filter(year %in% c(prior_year, data_year), networth >= 10^6) %>%
        mutate(band = cut(networth, breaks = mill_band_breaks,
                          labels = mill_band_labels, right = FALSE)) %>%
        group_by(year, band) %>%
        summarise(w = sum(wgt), .groups = "drop") %>%
        group_by(year) %>%
        mutate(share = make_pct_labels(w / sum(w))) %>%
        ungroup() %>%
        select(Band = band, year, share) %>%
        pivot_wider(names_from = year, values_from = share) %>%
        as.data.frame())

cat("\nMedian net worth of $1M+ households:\n")
print(df %>%
        filter(year %in% c(prior_year, data_year), networth >= 10^6) %>%
        group_by(year) %>%
        summarise(`Median net worth` = format_as_dollar(as.numeric(
          wtd.quantile(networth, weights = wgt, probs = 0.5))),
          .groups = "drop") %>%
        as.data.frame())

# ---- 7h: age ----
age_rate <- df %>%
  filter(year %in% c(prior_year, data_year)) %>%
  group_by(year, agecl) %>%
  summarise(share  = wtd_share(networth >= 10^6, wgt),
            n_mill = n_distinct(hh_id[networth >= 10^6]),
            .groups = "drop") %>%
  mutate(year_label = ifelse(year == data_year, "latest", "prior")) %>%
  select(-year) %>%
  pivot_wider(names_from = year_label, values_from = c(share, n_mill)) %>%
  mutate(agecl = order_age(agecl)) %>%
  arrange(agecl)

cat("\n---- 7h: Share worth $1M+ by age (2022 vs. 2025) ----\n")
print(age_rate %>%
        transmute(Age = agecl,
                  p = make_pct_labels(share_prior),
                  l = make_pct_labels(share_latest),
                  Change = make_pp_labels(share_latest - share_prior),
                  `1 in X` = one_in(share_latest),
                  `Unweighted $1M+ (latest)` = n_mill_latest) %>%
        rename(!!as.character(prior_year) := p,
               !!as.character(data_year)  := l) %>%
        as.data.frame())

cat("\nAge mix, millionaires vs. all households (", data_year, "):\n", sep = "")
print(age_compare %>%
        mutate(agecl = order_age(agecl),
               share = make_pct_labels(share)) %>%
        select(Age = agecl, group, share) %>%
        pivot_wider(names_from = group, values_from = share) %>%
        arrange(Age) %>%
        as.data.frame())

cat("\nMedian age of household head (", data_year, "):\n", sep = "")
print(tibble(Group = c("All households", "Millionaires"),
             `Median age` = c(as.numeric(wtd.quantile(df_year$age,
                                                      weights = df_year$wgt,
                                                      probs = 0.5)),
                              as.numeric(wtd.quantile(df_mill$age,
                                                      weights = df_mill$wgt,
                                                      probs = 0.5)))) %>%
        as.data.frame())

# ---- 7i: homeownership ----
if("houses" %in% names(df_year)){
  own_df <- df_year %>% mutate(owner = houses > 0)
  
  cat("\n---- 7i: Homeownership and millionaires (", data_year, ") ----\n", sep = "")
  print(tibble(
    Measure = c("Own a home - all households",
                "Own a home - $1M+ households",
                "$1M+ rate - homeowners",
                "$1M+ rate - non-homeowners",
                "Non-homeowners as share of $1M+ households"),
    Value = c(wtd_share(own_df$owner, own_df$wgt),
              wtd_share(own_df$owner[nw_mill_flag], own_df$wgt[nw_mill_flag]),
              wtd_share(own_df$networth[own_df$owner] >= 10^6,
                        own_df$wgt[own_df$owner]),
              wtd_share(own_df$networth[!own_df$owner] >= 10^6,
                        own_df$wgt[!own_df$owner]),
              1 - wtd_share(own_df$owner[nw_mill_flag],
                            own_df$wgt[nw_mill_flag]))) %>%
      mutate(Value = make_pct_labels(Value)) %>%
      as.data.frame())
}

# ---- 7j: income ----
# SCF income is for the calendar year BEFORE the survey year.
if("income" %in% names(df_year)){
  med_inc <- function(d){
    format_as_dollar(as.numeric(wtd.quantile(d$income, weights = d$wgt,
                                             probs = 0.5)))
  }
  
  cat("\n---- 7j: Income and millionaires (", data_year,
      " survey, prior-year income) ----\n", sep = "")
  print(tibble(
    Measure = c("Median income - all households",
                "Median income - $1M+ households",
                "$1M+ households with income under $100k",
                "$1M+ households with income under $250k"),
    Value = c(med_inc(df_year),
              med_inc(df_mill),
              make_pct_labels(wtd_share(df_mill$income < 10^5, df_mill$wgt)),
              make_pct_labels(wtd_share(df_mill$income < 2.5 * 10^5,
                                        df_mill$wgt)))) %>%
      as.data.frame())
  
  # Splits the under-$100k millionaires by age, to see how much of that
  # group is retirees.
  cat("\nMillionaire income by age (", data_year, "):\n", sep = "")
  print(df_mill %>%
          group_by(agecl) %>%
          summarise(`Median income` = format_as_dollar(as.numeric(
            wtd.quantile(income, weights = wgt, probs = 0.5))),
            `Income under $100k` = make_pct_labels(wtd_share(income < 10^5, wgt)),
            `Unweighted $1M+` = n_distinct(hh_id),
            .groups = "drop") %>%
          mutate(agecl = order_age(agecl)) %>%
          arrange(agecl) %>%
          rename(Age = agecl) %>%
          as.data.frame())
  
  cat("\nShare of under-$100k-income millionaires who are 65+: ",
      make_pct_labels(wtd_share(df_mill$age[df_mill$income < 10^5] >= 65,
                                df_mill$wgt[df_mill$income < 10^5])),
      "\n", sep = "")
}

# ---- 7k: composition (same data as chart 08) ----
if(exists("composition")){
  cat("\n---- 7k: What millionaire wealth is made of (", data_year, ") ----\n", sep = "")
  print(composition %>%
          arrange(desc(share)) %>%
          transmute(Component = component,
                    `Share of net worth` = make_pct_labels(share)) %>%
          as.data.frame())
}

if(exists("band_comp")){
  cat("\nComposition by net worth band (share of each band's net worth):\n")
  print(band_comp %>%
          mutate(component = factor(component, levels = comp_levels),
                 share = make_pct_labels(share)) %>%
          pivot_wider(names_from = band, values_from = share) %>%
          arrange(component) %>%
          rename(Component = component) %>%
          as.data.frame())
  
  cat("\nUnweighted households in each band:\n")
  print(df_mill_band %>%
          group_by(band) %>%
          summarise(`Unweighted households` = n_distinct(hh_id),
                    .groups = "drop") %>%
          as.data.frame())
}

if(exists("liquidity")){
  cat("\nMedian share of millionaire net worth in home + retirement, by age:\n")
  print(liquidity %>%
          mutate(agecl = order_age(agecl)) %>%
          arrange(agecl) %>%
          transmute(Age = agecl,
                    `Median share` = make_pct_labels(median_illiquid)) %>%
          as.data.frame())
  
  cat("All ages: ",
      make_pct_labels(as.numeric(wtd.quantile(
        (df_mill$homeeq + df_mill$retqliq) / df_mill$networth,
        weights = df_mill$wgt, probs = 0.5))), "\n", sep = "")
}

# ---- 7l: Wealth Ladder change in percentage points ----
cat("\n---- 7l: Wealth Ladder share change, ", prior_year, "-", data_year,
    " and ", baseline_year, "-", data_year, " ----\n", sep = "")
print(ladder_by_year %>%
        filter(year %in% key_years) %>%
        group_by(level) %>%
        summarise(chg_recent = make_pp_labels(share[year == data_year] -
                                                share[year == prior_year]),
                  chg_long   = make_pp_labels(share[year == data_year] -
                                                share[year == baseline_year]),
                  .groups = "drop") %>%
        rename(`Wealth Level` = level,
               !!paste0(prior_year, "-", data_year) := chg_recent,
               !!paste0(baseline_year, "-", data_year) := chg_long) %>%
        as.data.frame())

cat("\n================== END KEY FACTS FOR REVIEW ==================\n\n")

# ##################################################################### #
# SECTION 8: Sanity checks
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

########################## Close the log ############################## #

cat("\nRun finished: ", format(Sys.time()), "\n", sep = "")

globalCallingHandlers(NULL)
sink()
close(log_con)

print(paste0("Log saved to: ", log_file))

# ############################  End  ################################## #