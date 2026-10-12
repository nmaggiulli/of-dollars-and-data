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
# WHO ACTUALLY GOT RICHER SINCE 2022?
#
#   Section 1: Median net worth by age, 2022 vs 2025 (checks the WSJ result)
#              + overall median and 90th percentile by age (text only)
#   Section 2: Change by percentile, 10th-90th
#   Section 3: Under 35 vs. 75+ - long view, who owns what, what drove the
#              change, typical balance sheet, and a check on whether the
#              groups themselves changed
#   Section 4: Homeowners vs. non-homeowners (table only)
#   Section 5: Income ends (text only)
#   Section 7: Robustness checks
#   Section 8: Every figure cited in the post, in post order, with the
#              post's number in brackets for line-by-line checking
#   Section 6: Sanity checks
#
# Nothing above the 90th percentile of net worth is shown.
# ##################################################################### #

########################## Parameters ################################# #

latest_year   <- 2025
prior_year    <- 2022
baseline_year <- 2019   # one survey further back, for the "zoom out"

# The two ends of the age story
young_old  <- c("<35", "75+")
age_labels <- c(`<35` = "Under 35", `75+` = "75+")

# Percentiles in the change-by-percentile table. Capped at the 90th.
change_probs <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)

# In the change-by-percentile charts, a percentile whose PRIOR value is
# below this is left off the chart (it stays in the table). A move from
# $500 to $200 is "-60%" but means nothing.
min_base_dollars <- 5000

# Labels under this (in dollars) are hidden on the "what drove the change"
# chart so it isn't cluttered with "$0k"
min_label_dollars <- 1000

# Colors
prior_year_color    <- "#B3B3B3"
baseline_year_color <- "#DEDEDE"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(latest_year))))
}

three_year_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(baseline_year_color, prior_year_color,
                                        chart_standard_color),
                                      c(as.character(baseline_year),
                                        as.character(prior_year),
                                        as.character(latest_year))))
}

label_size <- 2.4

########################## Output paths ############################### #

folder_name <- "0524_scf_who_got_richer"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", latest_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Console log ################################ #
# Everything printed to the console also goes to a text file in out_path.
# If a previous run died mid-script its sink is still open - close it first.
# Needs R 4.0+.

while(sink.number() > 0) sink()
globalCallingHandlers(NULL)

log_file <- paste0(out_path, "/", basename(folder_name), "_", latest_year, "_log.txt")
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
    "latest_year = ", latest_year, ", prior_year = ", prior_year,
    ", baseline_year = ", baseline_year, "\n\n", sep = "")

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# Asset buckets for the "what drove the change" breakdown. They add up to
# SCF `asset`; whatever is left over becomes the "Other" bucket.
mix_parts <- list(
  `Primary home`        = c("houses"),
  `Other real estate`   = c("oresre", "nnresre"),
  `Business`            = c("bus"),
  `Stocks & funds`      = c("stocks", "nmmf"),
  `Retirement accounts` = c("retqliq"),
  `Cash & bonds`        = c("liq", "cds", "bond", "savbnd"),
  `Vehicles`            = c("vehic")
)
other_label <- "Other (annuities, trusts, etc.)"
mix_levels  <- c(names(mix_parts), other_label, "Debt")

keep_vars <- unique(c("year", "hh_id", "imp_id", "agecl", "age", "wgt",
                      "networth", "income", "asset", "debt", "fin",
                      "homeeq", unlist(mix_parts)))

missing_vars <- setdiff(keep_vars, names(scf_stack))
if(length(missing_vars) > 0){
  message("Not in stack (treated as 0 where used): ",
          paste(missing_vars, collapse = ", "))
}

df <- scf_stack %>%
  select(any_of(keep_vars)) %>%
  arrange(year, hh_id, imp_id)

for(v in missing_vars){ df[[v]] <- 0 }

year_min <- min(df$year)
year_max <- max(df$year)

stopifnot(latest_year %in% df$year,
          prior_year %in% df$year,
          baseline_year %in% df$year)

df_two   <- df %>% filter(year %in% c(prior_year, latest_year))
df_three <- df %>% filter(year %in% c(baseline_year, prior_year, latest_year))

source_string <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"
note_string   <- paste0("Note: All figures are adjusted for inflation (",
                        latest_year, " dollars).")

make_caption <- function(extra = NULL){
  paste(c(source_string, note_string,
          if(!is.null(extra)) str_wrap(extra, width = 80)), collapse = "\n")
}

########################## Helper Functions ########################### #

# Every output file gets "_<latest_year>" before its extension so
# WordPress can tell the 2022 and 2025 versions apart.
add_year_suffix <- function(file_path){
  sub("(\\.[A-Za-z0-9]+)$", paste0("_", latest_year, "\\1"), file_path)
}

save_chart <- function(plot, file_name){
  ggsave(add_year_suffix(paste0(out_path, "/", file_name)), plot,
         width = 15, height = 12, units = "cm")
}

# Writes a WordPress-ready table: id="myTable" so the post CSS applies,
# no xtable comment, and all on one line so WordPress doesn't add <p>/<br> gaps
write_html_table <- function(table_out, file_name){
  html <- capture.output(print(xtable(table_out),
                               include.rownames = FALSE,
                               type = "html",
                               comment = FALSE,
                               html.table.attributes = 'id="myTable"'))
  writeLines(paste(trimws(html), collapse = ""),
             add_year_suffix(paste0(out_path, "/", file_name)))
}

wtd_stat <- function(x, w, quantile_prob){
  if(quantile_prob == 0){
    as.numeric(wtd.mean(x, weights = w))
  } else {
    as.numeric(wtd.quantile(x, weights = w, probs = quantile_prob))
  }
}

wtd_share <- function(condition, weights){
  as.numeric(wtd.mean(as.numeric(condition), weights = weights))
}

safe_pct_change <- function(new_value, old_value){
  ifelse(old_value > 0, (new_value / old_value) - 1, NA_real_)
}

make_pct_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(ifelse(values > 0, "+", ""),
                formatC(100 * values, format = "f", digits = digits), "%"))
}

make_share_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(formatC(100 * values, format = "f", digits = digits), "%"))
}

# "$80k", "-$9k", "$1.2M"
short_dollar <- function(values){
  sign_prefix <- ifelse(values < 0, "-$", "$")
  ifelse(abs(values) >= 10^6,
         paste0(sign_prefix, formatC(abs(values)/10^6, format = "f", digits = 1), "M"),
         paste0(sign_prefix, formatC(abs(values)/10^3, format = "f", digits = 0), "k"))
}

period_factor <- function(year_vector, years = c(prior_year, latest_year)){
  factor(as.character(year_vector), levels = as.character(years))
}

# Two-year comparison of one net worth statistic by group
compare_two_years <- function(data, group_var, quantile_prob){
  data %>%
    group_by(year, group = .data[[group_var]]) %>%
    summarise(value = wtd_stat(networth, wgt, quantile_prob), .groups = "drop") %>%
    mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
    select(group, year_label, value) %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    mutate(dollar_change = latest - prior,
           pct_change    = safe_pct_change(latest, prior))
}

change_table <- function(tbl, first_col){
  out <- tbl %>%
    transmute(Group      = as.character(group),
              Prior      = format_as_dollar(prior),
              Latest     = format_as_dollar(latest),
              `$ Change` = format_as_dollar(dollar_change),
              `% Change` = make_pct_labels(pct_change))
  names(out) <- c(first_col, as.character(prior_year),
                  as.character(latest_year), "$ Change", "% Change")
  out
}

# ##################################################################### #
# SECTION 1: Median net worth by age, 2022 vs. 2025
# ##################################################################### #

overall_median <- df_two %>%
  group_by(year) %>%
  summarise(median = wtd_stat(networth, wgt, 0.5), .groups = "drop")

med_prior  <- overall_median$median[overall_median$year == prior_year]
med_latest <- overall_median$median[overall_median$year == latest_year]

print(paste0("Overall median net worth: ", format_as_dollar(med_prior), " -> ",
             format_as_dollar(med_latest), " (",
             make_pct_labels(safe_pct_change(med_latest, med_prior), 1), ")"))

median_by_age <- compare_two_years(df_two, "agecl", 0.5)

print("Median net worth by age:")
print(change_table(median_by_age, "Age") %>% as.data.frame())

write_html_table(change_table(median_by_age, "Age"),
                 "01_median_networth_by_age_table.html")

# ---- 1a / 1b: levels side by side, and percent change ----
# Same pair of charts for the median and the 90th percentile.
make_age_charts <- function(tbl, stat_name, file_prefix){
  
  to_plot <- tbl %>%
    select(group, prior, latest) %>%
    pivot_longer(c(prior, latest), names_to = "period", values_to = "value") %>%
    mutate(period = period_factor(ifelse(period == "prior", prior_year, latest_year)),
           label  = short_dollar(value))
  
  plot <- ggplot(to_plot, aes(x = group, y = value, fill = period)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = label, group = period),
              position = position_dodge(width = 0.9), vjust = -0.5,
              col = chart_standard_color, size = label_size) +
    scale_y_continuous(label = dollar, expand = expansion(mult = c(0, 0.08))) +
    period_fill_scale() +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0(stat_name, " Net Worth by Age\n",
                   prior_year, " vs. ", latest_year)) +
    labs(x = "Age", y = paste0(stat_name, " Net Worth"),
         caption = make_caption())
  
  save_chart(plot, paste0(file_prefix, "_by_age.jpeg"))
  
  to_plot <- tbl %>%
    mutate(label = make_pct_labels(pct_change),
           vj    = ifelse(pct_change > 0, -0.5, 1.5))
  
  plot <- ggplot(to_plot, aes(x = group, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(aes(label = label, vjust = vj),
              col = chart_standard_color, size = label_size) +
    geom_hline(yintercept = 0, col = "black") +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.10, 0.10))) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Change in Net Worth by Age\n",
                   stat_name, ", ", prior_year, "-", latest_year)) +
    labs(x = "Age", y = paste0("Change in Real ", stat_name, " Net Worth"),
         caption = make_caption())
  
  save_chart(plot, paste0(file_prefix, "_pct_change_by_age.jpeg"))
}

make_age_charts(median_by_age, "Median", "01_median_networth")

# ---- 1c: 90th percentile by age ----
# The 90th WITHIN each age group, not the top 10% overall.
p90_by_age <- compare_two_years(df_two, "agecl", 0.9)

print("90th percentile net worth by age:")
print(change_table(p90_by_age, "Age") %>% as.data.frame())

write_html_table(change_table(p90_by_age, "Age"),
                 "01_p90_networth_by_age_table.html")

make_age_charts(p90_by_age, "90th Percentile", "01_p90_networth")

# ##################################################################### #
# SECTION 2: Change by percentile, 10th-90th
# ##################################################################### #

prob_label <- function(prob){
  ifelse(prob == 0.5, "50th (Median)",
         paste0(formatC(100 * prob, format = "f", digits = 0), "th"))
}

# One function for all three versions: everyone, under 35, and 75+.
# Percentiles are computed WITHIN the group (e.g. the 90th percentile of
# households under 35), not across all households.
make_pctile_change <- function(data, subtitle, file_tag, table_label){
  
  pctiles <- data %>%
    group_by(year) %>%
    group_modify(~ tibble(prob = change_probs,
                          value = as.numeric(wtd.quantile(.x$networth,
                                                          weights = .x$wgt,
                                                          probs = change_probs)))) %>%
    ungroup() %>%
    mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
    select(prob, year_label, value) %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    mutate(dollar_change = latest - prior,
           pct_change    = safe_pct_change(latest, prior),
           group         = factor(prob_label(prob),
                                  levels = prob_label(sort(change_probs))))
  
  print(paste0("Net worth by percentile, ", table_label, ":"))
  print(change_table(pctiles, "Percentile") %>% as.data.frame())
  
  write_html_table(change_table(pctiles, "Percentile"),
                   paste0("02_networth_change_by_percentile", file_tag, "_table.html"))
  
  to_plot <- pctiles %>%
    filter(prior >= min_base_dollars, !is.na(pct_change)) %>%
    mutate(label = make_pct_labels(pct_change),
           vj    = ifelse(pct_change > 0, -0.5, 1.5))
  
  dropped <- setdiff(as.character(pctiles$group), as.character(to_plot$group))
  if(length(dropped) > 0){
    message("Left off the ", table_label, " chart (prior value under ",
            format_as_dollar(min_base_dollars), "): ", paste(dropped, collapse = ", "))
  }
  
  plot <- ggplot(to_plot, aes(x = group, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(aes(label = label, vjust = vj),
              col = chart_standard_color, size = label_size) +
    geom_hline(yintercept = 0, col = "black") +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.10, 0.10))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Net Worth Change by Percentile\n", subtitle)) +
    labs(x = "Percentile", y = "Change in Real Net Worth",
         caption = make_caption(
           if(length(dropped) > 0) paste0("Percentiles near or below $0 are ",
                                          "not shown, since a percent change ",
                                          "isn't meaningful there.") else NULL))
  
  save_chart(plot, paste0("02_networth_pct_change_by_percentile", file_tag, ".jpeg"))
  
  pctiles
}

pctiles <- make_pctile_change(df_two,
                              paste0(prior_year, "-", latest_year),
                              "", "all households")

pctiles_young <- make_pctile_change(df_two %>% filter(agecl == "<35"),
                                    paste0("Under 35, ", prior_year, "-", latest_year),
                                    "_under_35", "under 35")

pctiles_old <- make_pctile_change(df_two %>% filter(agecl == "75+"),
                                  paste0("75+, ", prior_year, "-", latest_year),
                                  "_75_plus", "75+")

# ##################################################################### #
# SECTION 3: Under 35 vs. 75+
# ##################################################################### #

df_yo <- df %>%
  filter(agecl %in% young_old) %>%
  mutate(age_group = factor(age_labels[as.character(agecl)],
                            levels = unname(age_labels)))

# ---- 3a: the long view ----
yo_time <- df_yo %>%
  group_by(year, age_group) %>%
  summarise(median = wtd_stat(networth, wgt, 0.5), .groups = "drop")

print("Median net worth, under 35 vs. 75+ (recent surveys):")
print(yo_time %>%
        filter(year >= baseline_year) %>%
        mutate(median = format_as_dollar(median)) %>%
        pivot_wider(names_from = age_group, values_from = median) %>%
        as.data.frame())

print(paste0("Change ", baseline_year, "-", latest_year, ":"))
print(yo_time %>%
        group_by(age_group) %>%
        summarise(change = make_pct_labels(safe_pct_change(
          median[year == latest_year], median[year == baseline_year])),
          .groups = "drop") %>%
        as.data.frame())

# Place each label where the line isn't: above a peak, below a dip,
# upper-left of a point on a rising stretch, upper-right on a falling one.
# The last point is pulled left so it isn't cut off at the panel edge.
point_labels <- yo_time %>%
  arrange(age_group, year) %>%
  group_by(age_group) %>%
  mutate(prev = lag(median), nxt = lead(median)) %>%
  ungroup() %>%
  filter(year %in% c(baseline_year, prior_year, latest_year)) %>%
  mutate(label = short_dollar(median),
         spot  = case_when(is.na(nxt) & median >= prev        ~ "top",
                           is.na(nxt)                          ~ "bottom",
                           median >= prev & median >= nxt      ~ "top",
                           median <= prev & median <= nxt      ~ "bottom",
                           median > prev                       ~ "rising",
                           TRUE                                ~ "falling"),
         vj = case_when(spot == "top"    ~ -0.9,
                        spot == "bottom" ~  1.9,
                        TRUE             ~ -0.4),
         hj = case_when(is.na(nxt)        ~  0.8,
                        spot == "rising"  ~  1.15,
                        spot == "falling" ~ -0.15,
                        TRUE              ~  0.5))

plot <- ggplot(yo_time, aes(x = year, y = median)) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  geom_point(data = point_labels, col = chart_standard_color, size = 1.2) +
  geom_text(data = point_labels, aes(label = label, vjust = vj, hjust = hj),
            col = chart_standard_color, size = label_size) +
  facet_wrap(vars(age_group), scales = "free_y", axes = "all") +
  scale_y_continuous(label = dollar, expand = expansion(mult = c(0.12, 0.18))) +
  scale_x_continuous(breaks = sort(seq(year_max, year_min, by = -9)),
                     expand = expansion(mult = c(0.04, 0.08))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Median Net Worth Over Time\n",
                 "Under 35 vs. 75+")) +
  labs(x = "Year", y = "Median Net Worth",
       caption = make_caption("Each panel has its own scale."))

save_chart(plot, "03_median_networth_over_time_young_old.jpeg")

# ---- 3b: who owns what ----
own_vars <- c(houses = "Owns a home", stocks = "Stocks (direct)",
              retqliq = "Retirement accounts", vehic = "Vehicles")

yo_own <- df_yo %>%
  filter(year %in% c(baseline_year, prior_year, latest_year)) %>%
  select(year, age_group, wgt, all_of(names(own_vars))) %>%
  pivot_longer(all_of(names(own_vars)), names_to = "asset", values_to = "value") %>%
  group_by(year, age_group, asset) %>%
  summarise(share = wtd_share(value > 0, wgt), .groups = "drop") %>%
  mutate(asset  = factor(own_vars[asset], levels = unname(own_vars)),
         period = period_factor(year, c(baseline_year, prior_year, latest_year)))

yo_own_table <- yo_own %>%
  mutate(share = make_share_labels(share, 1)) %>%
  select(Asset = asset, Age = age_group, year, share) %>%
  pivot_wider(names_from = year, values_from = share) %>%
  arrange(Asset, Age)

print("Share owning each asset, under 35 vs. 75+:")
print(yo_own_table %>% as.data.frame())

write_html_table(yo_own_table, "03_who_owns_what_young_old_table.html")

plot <- ggplot(yo_own, aes(x = age_group, y = share, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(vars(asset), axes = "all") +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  three_year_fill_scale() +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Who Owns What\n",
                 "Under 35 vs. 75+")) +
  # Ownership rates aren't dollar figures, so skip the inflation note
  labs(x = NULL, y = "Share of Households",
       caption = source_string)

save_chart(plot, "03_ownership_rates_under_35_vs_75_plus.jpeg")

# ---- 3c: what drove the change ----
# Medians of the pieces don't add up to the median net worth, so this uses
# AVERAGES within the middle 50% of each age group (25th-75th percentile of
# net worth, within age and year). The pieces add up, and the richest
# households don't dominate.
df_mix <- df_two %>%
  group_by(year, agecl) %>%
  mutate(nw_p25 = wtd_stat(networth, wgt, 0.25),
         nw_p75 = wtd_stat(networth, wgt, 0.75)) %>%
  ungroup() %>%
  filter(networth >= nw_p25, networth <= nw_p75)

for(part in names(mix_parts)){
  df_mix[[part]] <- rowSums(as.matrix(df_mix[, mix_parts[[part]], drop = FALSE]))
}
df_mix[[other_label]] <- df_mix$asset - rowSums(as.matrix(df_mix[, names(mix_parts)]))
df_mix[["Debt"]]      <- -df_mix$debt

mix_change <- df_mix %>%
  select(year, agecl, wgt, all_of(mix_levels), networth) %>%
  pivot_longer(all_of(c(mix_levels, "networth")),
               names_to = "component", values_to = "value") %>%
  group_by(year, agecl, component) %>%
  summarise(mean_value = weighted.mean(value, wgt), .groups = "drop") %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(-year) %>%
  pivot_wider(names_from = year_label, values_from = mean_value) %>%
  mutate(change = latest - prior)

mix_table <- mix_change %>%
  mutate(component = factor(ifelse(component == "networth", "Net worth", component),
                            levels = c(mix_levels, "Net worth")),
         change = format_as_dollar(change)) %>%
  select(component, agecl, change) %>%
  pivot_wider(names_from = agecl, values_from = change) %>%
  arrange(component) %>%
  rename(Component = component)

print(paste0("What drove the change: average per household, middle 50% of each ",
             "age group, ", prior_year, "-", latest_year,
             " (debt negative; the Net worth row should land near the median change):"))
print(mix_table %>% as.data.frame())

write_html_table(mix_table, "03_what_drove_change_by_age_table.html")

to_plot <- mix_change %>%
  filter(agecl %in% young_old, component != "networth") %>%
  mutate(age_group = factor(age_labels[as.character(agecl)],
                            levels = unname(age_labels)),
         component = factor(component, levels = rev(mix_levels)),
         label = ifelse(abs(change) < min_label_dollars, "", short_dollar(change)),
         hj    = ifelse(change >= 0, -0.1, 1.1))

plot <- ggplot(to_plot, aes(x = component, y = change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label, hjust = hj),
            col = chart_standard_color, size = label_size) +
  geom_hline(yintercept = 0, col = "black") +
  coord_flip() +
  # Category labels only on the left facet, so the long labels aren't drawn twice
  facet_wrap(vars(age_group), axes = "all", axis.labels = "margins") +
  scale_y_continuous(label = label_dollar(scale = 1e-3, suffix = "k"),
                     breaks = breaks_pretty(n = 3),
                     expand = expansion(mult = c(0.30, 0.30))) +
  of_dollars_and_data_theme +
  theme(plot.caption.position = "plot",
        plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5)) +
  ggtitle(paste0("What Drove the Change\n",
                 "Avg. per Household, ", prior_year, "-", latest_year)) +
  labs(x = NULL, y = "Change in Real Value",
       caption = make_caption(paste0(
         "Middle 50% of each age group by net worth.\n",
         "Debt is negative, so less debt shows as a gain.")))

save_chart(plot, "03_what_drove_change_young_old.jpeg")

# ---- 3d: typical balance sheet (medians, zeros included) ----
balance_vars <- c(networth = "Net worth", income = "Income",
                  liq = "Bank accounts", fin = "Financial assets",
                  retqliq = "Retirement accounts", homeeq = "Home equity",
                  debt = "Total debt")

yo_balance <- df_yo %>%
  filter(year %in% c(prior_year, latest_year)) %>%
  select(year, age_group, wgt, all_of(names(balance_vars))) %>%
  pivot_longer(all_of(names(balance_vars)), names_to = "measure", values_to = "value") %>%
  group_by(age_group, measure, year) %>%
  summarise(median = wtd_stat(value, wgt, 0.5), .groups = "drop") %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(-year) %>%
  pivot_wider(names_from = year_label, values_from = median) %>%
  mutate(pct_change = safe_pct_change(latest, prior),
         measure = factor(balance_vars[measure], levels = unname(balance_vars))) %>%
  arrange(age_group, measure)

yo_balance_table <- yo_balance %>%
  transmute(Age = as.character(age_group),
            Measure = as.character(measure),
            Prior = format_as_dollar(prior),
            Latest = format_as_dollar(latest),
            `% Change` = make_pct_labels(pct_change))
names(yo_balance_table)[3:4] <- c(as.character(prior_year), as.character(latest_year))

print("Typical balance sheet (medians, zeros included):")
print(yo_balance_table %>% as.data.frame())

write_html_table(yo_balance_table, "03_typical_balance_sheet_young_old_table.html")

# ---- 3e: did the groups themselves change? (log only) ----
age_mix <- df_two %>%
  group_by(year) %>%
  mutate(total_wgt = sum(wgt)) %>%
  group_by(year, agecl) %>%
  summarise(share_of_households = sum(wgt) / first(total_wgt),
            households = sum(wgt),
            median_age = wtd_stat(age, wgt, 0.5),
            unweighted = n_distinct(hh_id),
            .groups = "drop")

print("Age mix of households (composition check, not for a chart):")
print(age_mix %>%
        mutate(share_of_households = make_share_labels(share_of_households, 1),
               households = paste0(formatC(households/10^6, format = "f",
                                           digits = 1), "M")) %>%
        arrange(agecl, year) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 4: Homeowners vs. non-homeowners
# ##################################################################### #
# Whoever owned a home in each survey - not the same households over time.

summarise_three_years <- function(data, label){
  stats <- data %>%
    group_by(year) %>%
    mutate(total_wgt = sum(wgt)) %>%
    group_by(year, group) %>%
    summarise(p25 = wtd_stat(networth, wgt, 0.25),
              p50 = wtd_stat(networth, wgt, 0.50),
              p75 = wtd_stat(networth, wgt, 0.75),
              share_of_households = sum(wgt) / first(total_wgt),
              n_unw = n_distinct(hh_id),
              .groups = "drop")
  
  changes <- stats %>%
    select(group, year, p25, p50, p75) %>%
    pivot_longer(c(p25, p50, p75), names_to = "stat", values_to = "value") %>%
    pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
    mutate(chg_early = safe_pct_change(.data[[paste0("y", prior_year)]],
                                       .data[[paste0("y", baseline_year)]]),
           chg_late  = safe_pct_change(.data[[paste0("y", latest_year)]],
                                       .data[[paste0("y", prior_year)]]),
           chg_full  = safe_pct_change(.data[[paste0("y", latest_year)]],
                                       .data[[paste0("y", baseline_year)]]))
  
  median_table <- changes %>%
    filter(stat == "p50") %>%
    left_join(stats %>% group_by(group) %>%
                summarise(min_n = min(n_unw), .groups = "drop"), by = "group") %>%
    transmute(Group = as.character(group),
              b = format_as_dollar(.data[[paste0("y", baseline_year)]]),
              p = format_as_dollar(.data[[paste0("y", prior_year)]]),
              l = format_as_dollar(.data[[paste0("y", latest_year)]]),
              early = make_pct_labels(chg_early),
              late  = make_pct_labels(chg_late),
              full  = make_pct_labels(chg_full),
              min_n = formatC(min_n, format = "d", big.mark = ","))
  
  names(median_table) <- c(label, as.character(baseline_year),
                           as.character(prior_year), as.character(latest_year),
                           paste0(baseline_year, "-", prior_year),
                           paste0(prior_year, "-", latest_year),
                           paste0(baseline_year, "-", latest_year),
                           "Min. respondents")
  
  print(paste0("==== Median net worth by ", tolower(label), " ===="))
  print(median_table %>% as.data.frame())
  
  print(paste0("Change ", prior_year, "-", latest_year,
               " at the 25th / 50th / 75th percentile:"))
  print(changes %>%
          select(group, stat, chg_late) %>%
          mutate(chg_late = make_pct_labels(chg_late)) %>%
          pivot_wider(names_from = stat, values_from = chg_late) %>%
          as.data.frame())
  
  print("Share of households in each group:")
  print(stats %>%
          select(group, year, share_of_households) %>%
          mutate(share_of_households = make_share_labels(share_of_households, 1)) %>%
          pivot_wider(names_from = year, values_from = share_of_households) %>%
          as.data.frame())
  
  list(changes = changes, median_table = median_table)
}

df_tenure <- df_three %>%
  mutate(group = factor(ifelse(houses > 0, "Homeowners", "Non-homeowners"),
                        levels = c("Non-homeowners", "Homeowners")))

tenure_out <- summarise_three_years(df_tenure, "Homeownership")

write_html_table(tenure_out$median_table %>% select(-`Min. respondents`),
                 "04_networth_by_homeownership_table.html")

# ##################################################################### #
# SECTION 5: Income ends (text only)
# ##################################################################### #
# Only the bottom 20% and top 10% of earners hold up across the
# distribution; the middle groups are noise, so no chart.

df_income <- df_three %>%
  group_by(year) %>%
  arrange(income, .by_group = TRUE) %>%
  mutate(cum_wgt_inc = cumsum(wgt) / sum(wgt)) %>%
  ungroup() %>%
  mutate(group = factor(case_when(cum_wgt_inc <= 0.20 ~ "Bottom 20%",
                                  cum_wgt_inc <= 0.40 ~ "20th-40th",
                                  cum_wgt_inc <= 0.60 ~ "40th-60th",
                                  cum_wgt_inc <= 0.80 ~ "60th-80th",
                                  cum_wgt_inc <= 0.90 ~ "80th-90th",
                                  TRUE                ~ "Top 10%"),
                        levels = c("Bottom 20%", "20th-40th", "40th-60th",
                                   "60th-80th", "80th-90th", "Top 10%")))

income_out <- summarise_three_years(df_income, "Income Group")

# ##################################################################### #
# SECTION 7: Robustness checks
# ##################################################################### #
# Tests the post's main claims:
#   7a  Is it housing or age? Change in median net worth by age AND
#       homeownership. Note: who owns vs. rents shifts over time, so these
#       group medians partly reflect who ended up in each group.
#   7b  Is it just boomers aging in? Follow the same birth cohort: e.g.
#       households 72+ in 2022 vs. 75+ in 2025 (roughly the same people,
#       three years older). Compare that to the same-age change in the post.
#   7c  How much do home values explain? Median home value and home equity
#       among OWNERS, by age.
#   7d  Imputation noise: the median in each of the five implicates.
#   7e  Who counts as a young household? Young adults living with parents
#       aren't their own SCF household, so a shrinking count of young
#       households would mean the group is more selected.
#   7f  65+ owners combined, so the 74/75 boundary can't shuffle wealth.
#   7h  Under-35 renters' median net worth over every survey year.

robust_ages <- c("<35", "35-44", "45-54", "55-64", "65-74", "75+")

# ---- 7a: age x homeownership ----
df_age_tenure <- df_three %>%
  mutate(tenure = factor(ifelse(houses > 0, "Homeowners", "Non-homeowners"),
                         levels = c("Non-homeowners", "Homeowners")))

age_tenure <- df_age_tenure %>%
  group_by(year, agecl, tenure) %>%
  summarise(median = wtd_stat(networth, wgt, 0.5),
            n_unw  = n_distinct(hh_id),
            .groups = "drop") %>%
  group_by(agecl, tenure) %>%
  summarise(m_base   = median[year == baseline_year],
            m_prior  = median[year == prior_year],
            m_latest = median[year == latest_year],
            min_n    = min(n_unw),
            .groups = "drop") %>%
  mutate(chg_early = safe_pct_change(m_prior, m_base),
         chg_late  = safe_pct_change(m_latest, m_prior),
         agecl     = factor(agecl, levels = robust_ages))

print("==== 7a: Median net worth by age AND homeownership ====")
print(age_tenure %>%
        arrange(agecl, tenure) %>%
        transmute(Age = agecl, Tenure = tenure,
                  b = format_as_dollar(m_base),
                  p = format_as_dollar(m_prior),
                  l = format_as_dollar(m_latest),
                  `2019-2022` = make_pct_labels(chg_early),
                  `2022-2025` = make_pct_labels(chg_late),
                  `Min. respondents` = min_n) %>%
        rename(!!as.character(baseline_year) := b,
               !!as.character(prior_year) := p,
               !!as.character(latest_year) := l) %>%
        as.data.frame())

# ---- 7b: same birth cohort vs. same age ----
# 2025 age band [lo, hi] is compared with the 2022 band [lo-3, hi-3].
gap <- latest_year - prior_year
cohort_bands <- tibble(agecl = robust_ages,
                       lo = c(18, 35, 45, 55, 65, 75),
                       hi = c(34, 44, 54, 64, 74, Inf))

cohort_rows <- lapply(seq_len(nrow(cohort_bands)), function(i){
  b <- cohort_bands[i, ]
  latest_vals <- df_two %>% filter(year == latest_year, age >= b$lo, age <= b$hi)
  prior_same  <- df_two %>% filter(year == prior_year,  age >= b$lo, age <= b$hi)
  prior_coh   <- df_two %>% filter(year == prior_year,
                                   age >= b$lo - gap, age <= b$hi - gap)
  m_latest <- wtd_stat(latest_vals$networth, latest_vals$wgt, 0.5)
  m_same   <- wtd_stat(prior_same$networth,  prior_same$wgt, 0.5)
  m_coh    <- wtd_stat(prior_coh$networth,   prior_coh$wgt, 0.5)
  tibble(agecl = b$agecl,
         cohort_2022_ages = paste0(b$lo - gap, ifelse(is.infinite(b$hi), "+",
                                                      paste0("-", b$hi - gap))),
         same_age_prior = m_same, same_cohort_prior = m_coh, latest = m_latest,
         chg_same_age = safe_pct_change(m_latest, m_same),
         chg_cohort   = safe_pct_change(m_latest, m_coh))
})
cohort_tbl <- bind_rows(cohort_rows) %>%
  mutate(agecl = factor(agecl, levels = robust_ages))

print("==== 7b: Same age vs. same birth cohort (median net worth) ====")
print(cohort_tbl %>%
        transmute(`2025 age` = agecl,
                  `Same cohort in 2022 (ages)` = cohort_2022_ages,
                  `2022 same age` = format_as_dollar(same_age_prior),
                  `2022 same cohort` = format_as_dollar(same_cohort_prior),
                  `2025` = format_as_dollar(latest),
                  `Chg vs same age` = make_pct_labels(chg_same_age),
                  `Chg vs same cohort` = make_pct_labels(chg_cohort)) %>%
        as.data.frame())

# Like-for-like 75+ gain, for the post
old_cohort <- cohort_tbl %>% filter(agecl == "75+")
print(paste0("75+ like-for-like: households ", old_cohort$cohort_2022_ages, " in ",
             prior_year, " (", format_as_dollar(old_cohort$same_cohort_prior),
             ") vs. 75+ in ", latest_year, " (", format_as_dollar(old_cohort$latest),
             "): ", make_pct_labels(old_cohort$chg_cohort),
             " vs. ", make_pct_labels(old_cohort$chg_same_age), " same-age"))

# ---- 7h: young renters' median net worth over time ----
# Answers "did young renters actually get poorer?" across every survey year.
# Caveat: who rents changes over time (would-be buyers who stay renters
# raise the renter median), so read this alongside the homeownership rate.
young_renters <- df %>%
  filter(agecl == "<35") %>%
  group_by(year) %>%
  summarise(median    = wtd_stat(networth[houses <= 0], wgt[houses <= 0], 0.5),
            own_rate  = wtd_share(houses > 0, wgt),
            n_renters = n_distinct(hh_id[houses <= 0]),
            .groups = "drop")

print("==== 7h: Under-35 non-homeowners, median net worth by survey year ====")
print(young_renters %>%
        transmute(year,
                  `Renter median` = format_as_dollar(median),
                  `Under-35 homeownership` = make_share_labels(own_rate, 1),
                  `Renter respondents` = n_renters) %>%
        as.data.frame())

renter_labels <- young_renters %>%
  filter(year %in% c(baseline_year, prior_year, latest_year)) %>%
  mutate(label = format_as_dollar(round(median, -2)))

plot <- ggplot(young_renters, aes(x = year, y = median)) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  geom_point(data = renter_labels, col = chart_standard_color, size = 1.2) +
  geom_text(data = renter_labels, aes(label = label),
            vjust = -0.9, col = chart_standard_color, size = label_size) +
  scale_y_continuous(label = dollar, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_x_continuous(breaks = sort(seq(year_max, year_min, by = -6))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Median Net Worth of Young Renters\n",
                 "Under 35, Non-Homeowners")) +
  labs(x = "Year", y = "Median Net Worth",
       caption = make_caption())

save_chart(plot, "07_median_networth_under_35_renters_over_time.jpeg")

# ---- 7c: home values among owners ----
print("==== 7c: Among homeowners - median home value and home equity ====")
print(df_two %>%
        filter(houses > 0) %>%
        group_by(agecl, year) %>%
        summarise(home_value  = wtd_stat(houses, wgt, 0.5),
                  home_equity = wtd_stat(homeeq, wgt, 0.5),
                  networth    = wtd_stat(networth, wgt, 0.5),
                  n_unw       = n_distinct(hh_id),
                  .groups = "drop") %>%
        group_by(agecl) %>%
        summarise(`Home value chg`  = make_pct_labels(safe_pct_change(
          home_value[year == latest_year], home_value[year == prior_year])),
          `Home equity chg` = make_pct_labels(safe_pct_change(
            home_equity[year == latest_year], home_equity[year == prior_year])),
          `Net worth chg`   = make_pct_labels(safe_pct_change(
            networth[year == latest_year], networth[year == prior_year])),
          `2025 home value` = format_as_dollar(home_value[year == latest_year]),
          `Min. respondents` = min(n_unw),
          .groups = "drop") %>%
        mutate(agecl = factor(agecl, levels = robust_ages)) %>%
        arrange(agecl) %>%
        as.data.frame())

# ---- 7d: implicate spread ----
# In the SCF, the last digit of the record id is the implicate (1-5).
df_imp <- df_two %>%
  filter(agecl %in% young_old) %>%
  mutate(implicate = imp_id %% 10)

if(all(df_imp$implicate %in% 1:5)){
  print("==== 7d: Median net worth in each implicate (imputation noise only) ====")
  print(df_imp %>%
          group_by(agecl, year, implicate) %>%
          summarise(median = wtd_stat(networth, wgt, 0.5), .groups = "drop") %>%
          group_by(agecl, year) %>%
          summarise(low  = format_as_dollar(min(median)),
                    high = format_as_dollar(max(median)),
                    .groups = "drop") %>%
          as.data.frame())
} else {
  message("7d skipped: imp_id doesn't end in 1-5, so implicates can't be identified.")
}

# ---- 7e: how many young households are there? ----
print("==== 7e: Under-35 households over time (households and share of all) ====")
print(df %>%
        filter(year >= 2010) %>%
        group_by(year) %>%
        mutate(total_wgt = sum(wgt)) %>%
        filter(agecl == "<35") %>%
        summarise(households = paste0(formatC(sum(wgt)/10^6, format = "f", digits = 1), "M"),
                  share      = make_share_labels(sum(wgt) / first(total_wgt), 1),
                  .groups = "drop") %>%
        as.data.frame())

# ---- 7f: 65+ homeowners combined ----
# Removes the 74/75 boundary: households crossing from 65-74 into 75+ stay
# in the same group.
print("==== 7f: 65+ combined, by homeownership ====")
print(df_age_tenure %>%
        filter(age >= 65) %>%
        group_by(year, tenure) %>%
        summarise(median = wtd_stat(networth, wgt, 0.5),
                  n_unw  = n_distinct(hh_id), .groups = "drop") %>%
        group_by(tenure) %>%
        summarise(b = format_as_dollar(median[year == baseline_year]),
                  p = format_as_dollar(median[year == prior_year]),
                  l = format_as_dollar(median[year == latest_year]),
                  `2022-2025` = make_pct_labels(safe_pct_change(
                    median[year == latest_year], median[year == prior_year])),
                  `Min. respondents` = min(n_unw), .groups = "drop") %>%
        rename(!!as.character(baseline_year) := b,
               !!as.character(prior_year) := p,
               !!as.character(latest_year) := l) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 8: Figures cited in the post
# ##################################################################### #
# One line per figure, in the order the post uses them. The number the post
# states is in [brackets] at the end of each line so the log can be checked
# line by line. Figures that don't come from the SCF (stock returns, gift
# tax exclusion, etc.) are not computed here.

cat("\n==================== FIGURES CITED IN THE POST ====================\n\n")

post_line <- function(...){ cat(..., "\n", sep = "") }

age_row    <- function(tbl, a){ tbl[as.character(tbl$group) == a, ] }
pctile_row <- function(tbl, p){ tbl[abs(tbl$prob - p) < 1e-9, ] }

list_moves <- function(tbl, direction){
  moved <- !is.na(tbl$pct_change) &
    (if(direction == "up") tbl$pct_change > 0 else tbl$pct_change < 0)
  out <- as.character(tbl$group[moved])
  if(length(out) == 0) "none" else paste(out, collapse = ", ")
}

list_na <- function(tbl){
  out <- as.character(tbl$group[is.na(tbl$pct_change)])
  if(length(out) == 0) "none" else paste(out, collapse = ", ")
}

# ---- Intro: headline and table ----
y_med <- age_row(median_by_age, "<35")
o_med <- age_row(median_by_age, "75+")

post_line("--- Intro ---")
post_line("75+ median net worth: ", format_as_dollar(o_med$prior), " -> ",
          format_as_dollar(o_med$latest), " = ",
          make_pct_labels(o_med$pct_change, 1), "  [post: +38%]")
post_line("Under-35 median net worth: ", format_as_dollar(y_med$prior), " -> ",
          format_as_dollar(y_med$latest), " = ",
          make_pct_labels(y_med$pct_change, 1), "  [post: -23%]")

mid_med <- median_by_age %>% filter(!(as.character(group) %in% young_old))
mid_big <- which.max(abs(mid_med$pct_change))
post_line("Largest change among the other age groups: ",
          as.character(mid_med$group[mid_big]), " ",
          make_pct_labels(mid_med$pct_change[mid_big], 1),
          "  [post: no more than 6% in either direction]")

n_resp <- n_distinct(df$hh_id[df$year == latest_year])
post_line("SCF respondents (households interviewed) in ", latest_year, ": ",
          formatC(n_resp, format = "d", big.mark = ","),
          "  [post: only a few thousand households]")
post_line("Age table in the post: see 'Median net worth by age' in Section 1 above")

# ---- Are the rich getting richer? ----
all_p20 <- pctile_row(pctiles, 0.2)
all_top <- pctiles %>% filter(prob >= 0.7)

post_line("\n--- Are the Rich Getting Richer? ---")
post_line("All households, 20th percentile change: ",
          make_pct_labels(all_p20$pct_change, 1), "  [post: -11%]")
post_line("All households, smallest change at the 70th percentile or above: ",
          make_pct_labels(min(all_top$pct_change, na.rm = TRUE), 1),
          "  [post: +11% or higher]")
post_line("All households, percentiles that rose: ", list_moves(pctiles, "up"))
post_line("All households, percentiles that fell: ", list_moves(pctiles, "down"))

post_line("Under 35, percentiles that rose: ", list_moves(pctiles_young, "up"),
          "  [post: only the 80th]")
post_line("Under 35, percentiles that fell: ", list_moves(pctiles_young, "down"))
post_line("Under 35, no meaningful % change (2022 value at or below $0): ",
          list_na(pctiles_young))

post_line("75+, percentiles that rose: ", list_moves(pctiles_old, "up"),
          "  [post: everywhere but the 10th]")
post_line("75+, percentiles that fell: ", list_moves(pctiles_old, "down"))
post_line("75+, no meaningful % change (2022 value at or below $0): ",
          list_na(pctiles_old))

post_line("75+ same birth cohort (ages ", old_cohort$cohort_2022_ages, " in ",
          prior_year, "): ", format_as_dollar(old_cohort$same_cohort_prior), " -> ",
          format_as_dollar(old_cohort$latest), " = ",
          make_pct_labels(old_cohort$chg_cohort, 1),
          "  [post: 72+ in 2022, $418,720 -> $504,600, +21%]")

y_p90 <- pctile_row(pctiles_young, 0.9)
o_p20 <- pctile_row(pctiles_old, 0.2)
o_p30 <- pctile_row(pctiles_old, 0.3)
o_p40 <- pctile_row(pctiles_old, 0.4)

post_line("Under-35 90th percentile net worth, ", latest_year, ": ",
          format_as_dollar(y_p90$latest), "  [post: $351,000]")
post_line("75+ 40th percentile net worth, ", latest_year, ": ",
          format_as_dollar(o_p40$latest), "  [post: similar to $351,000]")
post_line("Under-35 90th percentile change: ",
          make_pct_labels(y_p90$pct_change, 1), "  [post: -14%]")
post_line("75+ 40th percentile change: ",
          make_pct_labels(o_p40$pct_change, 1), "  [post: +38%]")
post_line("75+ 20th / 30th percentile change: ",
          make_pct_labels(o_p20$pct_change, 1), " / ",
          make_pct_labels(o_p30$pct_change, 1), "  [post: +26% / +45%]")

# ---- What drove the change in net worth? ----
mix_moves <- function(a, direction){
  x <- mix_change %>% filter(as.character(agecl) == a, component != "networth")
  x <- if(direction == "up") x %>% filter(change > 0) %>% arrange(desc(change)) else
    x %>% filter(change < 0) %>% arrange(change)
  if(nrow(x) == 0) return("none")
  paste0(x$component, " (", short_dollar(x$change), ")", collapse = ", ")
}

post_line("\n--- What Drove the Change in Net Worth? ---")
post_line("(Middle 50% of each age group, average per household. Debt is shown as ",
          "negative, so a fall in 'Debt' means more debt.)")
post_line("75+ components that rose: ", mix_moves("75+", "up"),
          "  [post: primary home, retirement accounts, other assets]")
post_line("75+ components that fell: ", mix_moves("75+", "down"))
post_line("Under-35 components that fell: ", mix_moves("<35", "down"),
          "  [post: vehicles, retirement accounts, primary home]")
post_line("Under-35 components that rose: ", mix_moves("<35", "up"))

own_share <- function(asset_label, grp, yr){
  yo_own$share[as.character(yo_own$asset) == asset_label &
                 as.character(yo_own$age_group) == grp & yo_own$year == yr]
}

for(yr in c(baseline_year, prior_year, latest_year)){
  post_line("Homeownership, ", yr, ": under 35 ",
            make_share_labels(own_share("Owns a home", "Under 35", yr), 1),
            " vs. 75+ ", make_share_labels(own_share("Owns a home", "75+", yr), 1),
            "  [post: under 35 far less likely in every year shown]")
}
for(a in c("Stocks (direct)", "Retirement accounts", "Vehicles")){
  post_line(a, " ownership, ", latest_year, ": under 35 ",
            make_share_labels(own_share(a, "Under 35", latest_year), 1),
            " vs. 75+ ", make_share_labels(own_share(a, "75+", latest_year), 1),
            "  [post: similar across the two groups]")
}
post_line("Under-35 homeownership: ",
          make_share_labels(own_share("Owns a home", "Under 35", prior_year), 1),
          " -> ",
          make_share_labels(own_share("Owns a home", "Under 35", latest_year), 1),
          "  [post: 38.5% -> 35.6%]")
post_line("Under-35 share NOT owning a home, ", latest_year, ": ",
          make_share_labels(1 - own_share("Owns a home", "Under 35", latest_year), 1),
          "  [post: around 64%]")

y_tenure <- age_tenure %>% filter(as.character(agecl) == "<35")
y_owner  <- y_tenure$m_latest[y_tenure$tenure == "Homeowners"]
y_renter <- y_tenure$m_latest[y_tenure$tenure == "Non-homeowners"]
post_line("Under-35 median net worth, ", latest_year, ": homeowners ",
          format_as_dollar(y_owner), " vs. non-homeowners ",
          format_as_dollar(y_renter), " = ",
          formatC(y_owner / y_renter, format = "f", digits = 1), "x",
          "  [post: $184,000 vs. $12,000, roughly 15x]")

renters_ranked <- young_renters %>% arrange(desc(median))
post_line("Under-35 non-homeowner median, ", latest_year, ": ",
          format_as_dollar(young_renters$median[young_renters$year == latest_year]),
          "  [post: $12,000]")
post_line("Highest under-35 non-homeowner median in the data: ",
          renters_ranked$year[1], " (", format_as_dollar(renters_ranked$median[1]),
          "); runner-up ", renters_ranked$year[2], " (",
          format_as_dollar(renters_ranked$median[2]), "); data start in ", year_min,
          "  [post: 2025 is the highest since 1989]")

mix_shares <- df_mix %>%
  filter(year == prior_year, as.character(agecl) %in% young_old) %>%
  group_by(agecl) %>%
  summarise(total_assets = weighted.mean(asset, wgt),
            fin_share = weighted.mean(`Stocks & funds` + `Retirement accounts`, wgt) /
              total_assets,
            veh_share = weighted.mean(Vehicles, wgt) / total_assets,
            .groups = "drop")
ms <- function(a, col){ mix_shares[[col]][as.character(mix_shares$agecl) == a] }

post_line("Stocks, funds & retirement accounts as share of total assets, ", prior_year,
          " (middle 50%, ratio of averages): under 35 ",
          make_share_labels(ms("<35", "fin_share"), 1), " vs. 75+ ",
          make_share_labels(ms("75+", "fin_share"), 1),
          "  [post: about 9% vs. 16%]")
post_line("Vehicles as share of total assets, ", prior_year,
          " (middle 50%, ratio of averages): under 35 ",
          make_share_labels(ms("<35", "veh_share"), 1), " vs. 75+ ",
          make_share_labels(ms("75+", "veh_share"), 1),
          "  [post: about 20% vs. under 6%]")

veh_owners <- df_two %>%
  filter(as.character(agecl) %in% young_old, vehic > 0) %>%
  group_by(agecl, year) %>%
  summarise(med = wtd_stat(vehic, wgt, 0.5),
            n_unw = n_distinct(hh_id), .groups = "drop") %>%
  group_by(agecl) %>%
  summarise(prior = med[year == prior_year],
            latest = med[year == latest_year],
            chg = safe_pct_change(latest, prior),
            min_n = min(n_unw), .groups = "drop")
vo <- function(a){ veh_owners[as.character(veh_owners$agecl) == a, ] }

for(a in young_old){
  post_line("Median total vehicle value among ", age_labels[[a]],
            " households with a vehicle: ", format_as_dollar(vo(a)$prior), " -> ",
            format_as_dollar(vo(a)$latest), " = ",
            make_pct_labels(vo(a)$chg, 1), " (min. respondents ", vo(a)$min_n, ")",
            ifelse(a == "<35", "  [post: -21%]", "  [post: -1%]"))
}

# ---- A step back (not a collapse) ----
yo_val <- function(grp, yr){
  yo_time$median[as.character(yo_time$age_group) == grp & yo_time$year == yr]
}

post_line("\n--- A Step Back (Not a Collapse) ---")
post_line("Under-35 median: ", baseline_year, " ",
          format_as_dollar(yo_val("Under 35", baseline_year)), " -> ", prior_year, " ",
          format_as_dollar(yo_val("Under 35", prior_year)), " (",
          make_pct_labels(safe_pct_change(yo_val("Under 35", prior_year),
                                          yo_val("Under 35", baseline_year)), 1),
          ") -> ", latest_year, " ",
          format_as_dollar(yo_val("Under 35", latest_year)),
          "  [post: ~$18,000 -> nearly $43,000 (+140%) -> $33,000]")
post_line("75+ median change ", baseline_year, "-", prior_year, ": ",
          make_pct_labels(safe_pct_change(yo_val("75+", prior_year),
                                          yo_val("75+", baseline_year)), 1),
          "  [post: +13%]")
post_line("75+ median change ", prior_year, "-", latest_year, ": ",
          make_pct_labels(safe_pct_change(yo_val("75+", latest_year),
                                          yo_val("75+", prior_year)), 1),
          "  [post: +38%]")

y_steps <- yo_time %>%
  filter(age_group == "Under 35") %>%
  arrange(year) %>%
  mutate(from_year = lag(year),
         dollar_chg = median - lag(median),
         pct_chg = safe_pct_change(median, lag(median))) %>%
  filter(!is.na(from_year))

post_line("Under-35 median, every survey-to-survey change:")
print(y_steps %>%
        transmute(Period = paste0(from_year, "-", year),
                  `$ Change` = format_as_dollar(dollar_chg),
                  `% Change` = make_pct_labels(pct_chg, 1)) %>%
        as.data.frame())
big_pct <- y_steps[which.max(y_steps$pct_chg), ]
big_dol <- y_steps[which.max(y_steps$dollar_chg), ]
post_line("Largest under-35 increase by %: ", big_pct$from_year, "-", big_pct$year,
          " (", make_pct_labels(big_pct$pct_chg, 1), "); by $: ",
          big_dol$from_year, "-", big_dol$year, " (",
          format_as_dollar(big_dol$dollar_chg), ")",
          "  [post: 2019-2022 was the largest on record]")

cat("\n================== END FIGURES CITED IN THE POST ==================\n\n")

# ##################################################################### #
# SECTION 6: Sanity checks
# ##################################################################### #

print(paste0("Output folder: ", out_path))
print("Records (all five implicates), respondents, and households represented:")
print(df %>%
        filter(year %in% c(baseline_year, prior_year, latest_year)) %>%
        group_by(year) %>%
        summarise(records = n(),
                  respondents = n_distinct(hh_id),
                  households = paste0(formatC(sum(wgt)/10^6, format = "f",
                                              digits = 1), "M"),
                  .groups = "drop") %>%
        as.data.frame())

########################## Close the log ############################## #

cat("\nRun finished: ", format(Sys.time()), "\n", sep = "")

globalCallingHandlers(NULL)
sink()
close(log_con)

print(paste0("Log saved to: ", log_file))

# ############################  End  ################################## #