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
dollar_year <- 2025

# Percentiles used for the within-cohort distribution charts
dist_probs  <- seq(0.1, 0.9, by = 0.1)

########################## Output paths ############################### #

folder_name <- "0525_scf_net_worth_by_age_homeeq"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)
dist_path   <- paste0(out_path, "/age_distribution")

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)
dir.create(file.path(paste0(dist_path)), showWarnings = FALSE)

# Everything printed below also goes to this log file (and still to the console)
log_file <- paste0(out_path, "/0525_log_", data_year, ".txt")
sink(log_file, split = TRUE)

########################## Start Program Here ######################### #

# Keep every survey year for the over-time section of the post.
# ASSUMPTION: 0003_scf_stack.Rds stores all years in dollar_year dollars
# (as the Fed's historical summary extract does). If it doesn't, the
# over-time charts are NOT inflation-adjusted - check the 2019 print-out
# at the bottom against the Fed's published 2019 median in 2025 dollars.
scf_all <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

scf_stack <- scf_all %>%
  filter(year == data_year)

stopifnot(nrow(scf_stack) > 0)

df <- scf_stack %>%
  select(hh_id, imp_id, age,
         networth, homeeq, wgt,
         agecl) %>%
  # Net worth excluding home equity (homeeq = primary residence value minus
  # the debt secured by it). Answers "what does this look like without my home?"
  mutate(nw_ex_home = networth - homeeq) %>%
  arrange(hh_id, imp_id)

n_hh <- length(unique(df$hh_id))

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households. All figures are in ",
                               dollar_year, " dollars."),
                        width = 85)

excel_path <- paste0(out_path, "/all_var_summaries.xlsx")

########################## Helper Functions ########################### #

# Dollar labels for charts. The $k / $M decision is made once per vector
# based on the max, and negatives are handled properly - the original
# version rendered a -$1,500 bar as "$-2k" because the minus sign came out
# of formatC instead of the prefix. That matters here: the 25th percentile
# under-35 cells are negative and the post talks about them directly.
make_dollar_labels <- function(values){
  max_abs <- max(abs(values), na.rm = TRUE)
  sign_prefix <- ifelse(values < 0, "-$", "$")
  
  out <- if(max_abs >= 10^6){
    paste0(sign_prefix, formatC(abs(values)/10^6, big.mark = ",",
                                format = "f", digits = 2), "M")
  } else {
    paste0(sign_prefix, formatC(abs(values)/10^3, big.mark = ",",
                                format = "f", digits = 0), "k")
  }
  
  ifelse(round(values, 0) == 0, "$0", out)
}

quantile_prob_string <- function(quantile_prob){
  if(quantile_prob == 0){
    "avg"
  } else {
    str_pad(100 * quantile_prob, side = "left", width = 3, pad = "0")
  }
}

# Weighted stat for one variable at one prob (prob = 0 -> mean)
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

save_chart <- function(plot, file_path){
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

write_html_table <- function(table_out, file_path){
  # id="myTable" hooks into the blog's #myTable CSS
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        html.table.attributes = 'id="myTable"',
        comment = FALSE,
        file = file_path)
}

# Turns "35-44" or "<35" into something safe for a file name
file_safe <- function(x){
  x %>%
    str_replace_all("<", "under_") %>%
    str_replace_all(">=|\\+", "_plus") %>%
    str_replace_all("[^0-9A-Za-z]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# ##################################################################### #
# MAIN: charts and tables for one variable at one percentile
# ##################################################################### #

create_percentile_chart <- function(var, var_title, quantile_prob){
  
  qps <- quantile_prob_string(quantile_prob)
  
  # ---- Overall figure, printed to console for the write-up ----
  overall_value <- wtd_stat(df[[var]], df$wgt, quantile_prob)
  
  print(paste0("Overall ", var_title, " is: ", format_as_dollar(overall_value)))
  
  # ---- By age ----
  to_plot <- summarise_by(df, var, "agecl", quantile_prob)
  
  export_to_excel(to_plot %>%
                    mutate(value = format_as_dollar(value)),
                  excel_path,
                  paste0("age_", var, "_", qps),
                  create_new_file,
                  0)
  
  if(create_new_file == 1){
    assign("create_new_file", 0, envir = .GlobalEnv)
  }
  
  text_labels <- to_plot %>%
    mutate(label = make_dollar_labels(value),
           vj    = ifelse(value > 0, -0.2, 1.2))
  
  file_path <- paste0(out_path, "/", var, "_", qps, "_age_", data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = agecl, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = 3) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0(str_wrap(var_title, width = 38), "\nby Age")) +
    labs(x = "Age", y = paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
}

create_new_file <- 1

create_percentile_chart("networth", "25th Percentile Net Worth", 0.25)
create_percentile_chart("networth", "Median Net Worth", 0.5)
create_percentile_chart("networth", "75th Percentile Net Worth", 0.75)
create_percentile_chart("networth", "Average Net Worth", 0)
create_percentile_chart("networth", "90th Percentile Net Worth", 0.9)
create_percentile_chart("networth", "93rd Percentile Net Worth", 0.93)
create_percentile_chart("networth", "95th Percentile Net Worth", 0.95)
create_percentile_chart("networth", "96th Percentile Net Worth", 0.96)
create_percentile_chart("networth", "97th Percentile Net Worth", 0.97)
create_percentile_chart("networth", "98th Percentile Net Worth", 0.98)
create_percentile_chart("networth", "99th Percentile Net Worth", 0.99)

# ##################################################################### #
# Combined 25th / 50th / 75th / 90th percentile by age
# Post: "same y-axis across each sub-chart" -> fixed scales on purpose.
# ##################################################################### #

comb_probs <- c(0.25, 0.5, 0.75, 0.9)

comb_percentiles <- function(var){
  map_dfr(comb_probs, function(p){
    summarise_by(df, var, "agecl", p) %>%
      mutate(pct = p)
  }) %>%
    mutate(pct_label = factor(paste0(100 * pct, "th Percentile"),
                              levels = paste0(100 * comb_probs, "th Percentile")))
}

comb_df    <- comb_percentiles("networth")
comb_df_ex <- comb_percentiles("nw_ex_home")

comb_y_limits <- range(c(0, comb_df$value))

plot_pct_comb <- function(to_plot, title, y_title, file_name){
  
  labels <- to_plot %>%
    group_by(pct_label) %>%
    mutate(label = make_dollar_labels(value)) %>%
    ungroup() %>%
    mutate(vj = ifelse(value >= 0, -0.2, 1.2))
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    facet_wrap(vars(pct_label), axes = "all") +
    geom_text(data = labels,
              aes(x = agecl, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = 1.5) +
    scale_y_continuous(label = dollar, limits = comb_y_limits) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(title) +
    labs(x = "Age", y = y_title,
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, paste0(out_path, "/", file_name, "_", data_year, ".jpeg"))
}

plot_pct_comb(comb_df,
              "Net Worth Percentiles by Age",
              "Net Worth",
              "networth_pct_25_50_75_90_age_comb")

# HTML table of the same data: rows = age group, columns = percentile
pct_table <- comb_df %>%
  mutate(agecl   = as.character(agecl),
         display = format_as_dollar(value),
         col     = recode(as.character(pct_label),
                          "25th Percentile" = "25th Percentile",
                          "50th Percentile" = "Median",
                          "75th Percentile" = "75th Percentile",
                          "90th Percentile" = "90th Percentile")) %>%
  select(`Age Range` = agecl, col, display) %>%
  pivot_wider(names_from = col, values_from = display)

# "All Ages" row: same percentiles, computed on every household
all_ages_pct <- tibble(
  `Age Range`         = "All Ages",
  `25th Percentile`   = format_as_dollar(wtd_stat(df$networth, df$wgt, 0.25)),
  Median              = format_as_dollar(wtd_stat(df$networth, df$wgt, 0.5)),
  `75th Percentile`   = format_as_dollar(wtd_stat(df$networth, df$wgt, 0.75)),
  `90th Percentile`   = format_as_dollar(wtd_stat(df$networth, df$wgt, 0.9))
)

pct_table <- bind_rows(pct_table, all_ages_pct)

write_html_table(pct_table,
                 paste0(out_path, "/networth_pct_25_50_75_90_age_",
                        data_year, "_table.html"))

# ---- With vs. without home equity, side by side ----
nw_types <- c("Total net worth", "Net worth (ex home equity)")
nw_fills <- setNames(c("#a9bfd6", chart_standard_color), nw_types)

comb_both <- bind_rows(comb_df    %>% mutate(type = nw_types[1]),
                       comb_df_ex %>% mutate(type = nw_types[2])) %>%
  mutate(type = factor(type, levels = nw_types))

dodge <- position_dodge(width = 0.8)

with_vs_ex_caption <- function(extra_note = ""){
  paste0(source_string, "\n",
         str_wrap(paste0("Note: ", extra_note, "All figures are in ",
                         dollar_year, " dollars."), width = 85))
}

# (a) Median only - single panel, the first chart readers see in the section
median_both <- comb_both %>%
  filter(pct == 0.5)

median_both_labels <- median_both %>%
  mutate(label = make_dollar_labels(value),
         vj    = ifelse(value >= 0, -0.3, 1.3))

plot <- ggplot(median_both, aes(x = agecl, y = value, fill = type)) +
  geom_col(position = dodge, width = 0.8) +
  geom_text(data = median_both_labels,
            aes(x = agecl, y = value, label = label, vjust = vj, group = type),
            position = dodge,
            col = "black",
            size = 2.6) +
  scale_fill_manual(values = nw_fills) +
  scale_y_continuous(label = dollar,
                     expand = expansion(mult = c(0, 0.1))) +
  of_dollars_and_data_theme +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title    = element_blank()) +
  ggtitle("Median Net Worth by Age\nWith and Without Home Equity") +
  labs(x = "Age", y = "Median Net Worth",
       caption = with_vs_ex_caption())

save_chart(plot, paste0(out_path, "/nw_vs_nw_ex_home_median_age_",
                        data_year, ".jpeg"))

# (b) 25th / 50th / 75th / 90th percentiles
# Each panel gets its own y-axis (free_y): the comparison that matters is
# within a panel (light bar vs. dark bar), and a shared axis flattens the
# 25th and 50th percentile panels to nothing.
comb_both_labels <- comb_both %>%
  group_by(pct_label) %>%
  mutate(label = make_dollar_labels(value)) %>%
  ungroup() %>%
  mutate(vj = ifelse(value >= 0, -0.3, 1.3))

plot <- ggplot(comb_both, aes(x = agecl, y = value, fill = type)) +
  geom_col(position = dodge, width = 0.8) +
  geom_text(data = comb_both_labels,
            aes(x = agecl, y = value, label = label, vjust = vj, group = type),
            position = dodge,
            col = "black",
            size = 1.3) +
  facet_wrap(vars(pct_label), scales = "free_y", axes = "all") +
  scale_fill_manual(values = nw_fills) +
  scale_y_continuous(label = dollar,
                     expand = expansion(mult = c(0, 0.1))) +
  of_dollars_and_data_theme +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title    = element_blank()) +
  ggtitle("Net Worth Percentiles by Age\nWith and Without Home Equity") +
  labs(x = "Age", y = "Net Worth",
       caption = with_vs_ex_caption("Each panel has its own y-axis. "))

# Renamed from nw_with_vs_ex_home_pct_age_comb (already uploaded under the
# old legend label)
save_chart(plot, paste0(out_path, "/nw_vs_nw_ex_home_pct_age_comb_",
                        data_year, ".jpeg"))

# ##################################################################### #
# Prior survey years (only used for the 2022 vs. 2025 comparisons in the text)
# ##################################################################### #

df_ts <- scf_all %>%
  filter(year <= data_year) %>%
  select(year, age, agecl, wgt, networth)

# ##################################################################### #
# NEW: Distribution of a variable WITHIN each age cohort
# For each agecl, plot the 10th, 20th, ..., 90th weighted percentiles.
# Outputs (in out_path/age_distribution/):
#   - one chart per age cohort
#   - one faceted chart with every cohort side by side
#   (the 10th-90th table itself is printed in the log appendix)
#   - one sheet in all_var_summaries.xlsx
# ##################################################################### #

# comb_exclude: age groups left out of the faceted chart only (to keep it
# readable). They still get their own single-cohort charts and table columns.
create_age_distribution_charts <- function(var, var_title, probs = dist_probs,
                                           comb_exclude = c("65-74", "75+")){
  
  pct_levels <- paste0(100 * probs, "th")
  
  dist_df <- df %>%
    group_by(agecl) %>%
    reframe(pct   = probs,
            value = as.numeric(wtd.quantile(.data[[var]],
                                            weights = wgt,
                                            probs   = probs))) %>%
    mutate(pct_label = factor(paste0(100 * pct, "th"), levels = pct_levels))
  
  assign(paste0("age_dist_", var), dist_df, envir = .GlobalEnv)
  
  # Keep cohorts in their natural order if agecl is a factor
  cohorts <- if(is.factor(df$agecl)){
    levels(droplevels(df$agecl))
  } else {
    sort(unique(df$agecl))
  }
  
  # ---- Excel sheet ----
  export_to_excel(dist_df %>%
                    transmute(agecl,
                              percentile = pct_label,
                              value = format_as_dollar(value)),
                  excel_path,
                  paste0("age_dist_", var),
                  create_new_file,
                  0)
  
  if(create_new_file == 1){
    assign("create_new_file", 0, envir = .GlobalEnv)
  }
  
  # ##### 1. One chart per age cohort #####
  for(cohort in cohorts){
    
    to_plot <- dist_df %>%
      filter(as.character(agecl) == as.character(cohort))
    
    text_labels <- to_plot %>%
      mutate(label = make_dollar_labels(value),
             vj    = ifelse(value >= 0, -0.2, 1.2))
    
    file_path <- paste0(dist_path, "/", var, "_dist_age_",
                        file_safe(cohort), "_", data_year, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = pct_label, y = value)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
      geom_text(data = text_labels,
                aes(x = pct_label, y = value, label = label, vjust = vj),
                col = chart_standard_color,
                size = 3) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      ggtitle(paste0(str_wrap(paste0(var_title, " Distribution"), width = 38),
                     "\nAge ", cohort)) +
      labs(x = "Percentile", y = var_title,
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
  }
  
  # ##### 2. Cohorts faceted together (minus comb_exclude) #####
  comb_dist <- dist_df %>%
    filter(!(as.character(agecl) %in% comb_exclude))
  
  if(is.factor(comb_dist$agecl)) comb_dist$agecl <- droplevels(comb_dist$agecl)
  
  # Each bar formatted on its own ($7k, $350k, $1.2M) so small bars don't
  # show as "$0.01M" in panels that reach into the millions
  text_labels <- comb_dist %>%
    mutate(label = map_chr(value, make_dollar_labels),
           vj    = ifelse(value >= 0, -0.3, 1.3))
  
  file_path <- paste0(dist_path, "/", var, "_dist_age_comb_",
                      data_year, ".jpeg")
  
  plot <- ggplot(comb_dist, aes(x = pct_label, y = value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    facet_wrap(vars(agecl), scales = "free_y", axes = "all", ncol = 2) +
    geom_text(data = text_labels,
              aes(x = pct_label, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = 1.8) +
    scale_y_continuous(label = dollar,
                       expand = expansion(mult = c(0.12, 0.1))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
    ggtitle(paste0(str_wrap(paste0(var_title, " Distribution"), width = 38),
                   "\nby Age")) +
    labs(x = "Percentile", y = var_title,
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  print(paste0("Wrote ", length(cohorts), " cohort distribution charts for ",
               var, " to: ", dist_path))
}

create_age_distribution_charts("networth", "Net Worth")

# ##################################################################### #
# Fine-grained age table (average and median side by side)
# ##################################################################### #

# Covers EVERY household so each row rolls up into a bar on the age charts
# (e.g. 75-79 + 80+ = the 75+ bar). The old version stopped at age 80, which
# dropped the oldest households and made the table look inconsistent with
# the 75+ chart.
detail_levels <- c("Under 25", "25-29", "30-34", "35-39", "40-44", "45-49",
                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

age_detail_table <- df %>%
  mutate(agecl_new = case_when(age < 25 ~ "Under 25",
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
                               age < 80 ~ "75-79",
                               TRUE     ~ "80+"),
         agecl_new = factor(agecl_new, levels = detail_levels)) %>%
  group_by(agecl_new) %>%
  summarise(
    avg    = format_as_dollar(as.numeric(wtd.mean(networth, weights = wgt))),
    pct_50 = format_as_dollar(as.numeric(wtd.quantile(networth, weights = wgt,
                                                      probs = 0.5))),
    .groups = "drop"
  ) %>%
  mutate(agecl_new = as.character(agecl_new)) %>%
  select(`Age Range` = agecl_new,
         `Average Net Worth` = avg,
         `Median Net Worth`  = pct_50)

# "All Ages" row: average and median across every household
age_detail_table <- bind_rows(
  age_detail_table,
  tibble(`Age Range`         = "All Ages",
         `Average Net Worth` = format_as_dollar(wtd_stat(df$networth, df$wgt, 0)),
         `Median Net Worth`  = format_as_dollar(wtd_stat(df$networth, df$wgt, 0.5)))
)

write_html_table(age_detail_table,
                 paste0(out_path, "/networth_by_age_detail_",
                        data_year, "_table.html"))

# ##################################################################### #
# Sanity checks
# ##################################################################### #
# For the 2022 run, median net worth should print as $192,700 and average
# as $1,059,470 - the figures published in post 369.
# The 50th percentile row of the new distribution table should match the
# median-by-age chart from create_percentile_chart("networth", ..., 0.5).

print(paste0("Output folder: ", out_path))
print(paste0("Unweighted households (single implicate): ",
             formatC(n_hh, digits = 0, format = "f", big.mark = ",")))
print(paste0("Median net worth ", data_year, ": ",
             format_as_dollar(wtd_stat(df$networth, df$wgt, 0.5))))
print(paste0("Average net worth ", data_year, ": ",
             format_as_dollar(wtd_stat(df$networth, df$wgt, 0))))

dist_check <- age_dist_networth %>%
  filter(pct == 0.5) %>%
  select(agecl, dist_median = value) %>%
  inner_join(summarise_by(df, "networth", "agecl", 0.5), by = "agecl")

stopifnot(isTRUE(all.equal(dist_check$dist_median, dist_check$value)))
print("Distribution 50th percentile matches median-by-age: OK")

# ##################################################################### #
# NUMBERS FOR THE POST - printed in the order they appear in the text
# ##################################################################### #

compare_year <- 2022   # "since 2022"

fd <- format_as_dollar

yr_stat <- function(yr, prob, data = df_ts, var = "networth"){
  d <- data %>% filter(year == yr)
  wtd_stat(d[[var]], d$wgt, prob)
}

pct_chg <- function(new, old){
  paste0(ifelse(new >= old, "+", ""), round(100 * (new / old - 1)), "%")
}

n_item <- 0
say <- function(label, value){
  n_item <<- n_item + 1
  cat(sprintf("%2d. %s: %s\n", n_item, label, value))
}

print_df <- function(x){
  print(as.data.frame(x), row.names = FALSE)
  cat("\n")
}

avg_now   <- yr_stat(data_year, 0)
avg_cmp   <- yr_stat(compare_year, 0)
med_now   <- yr_stat(data_year, 0.5)
med_cmp   <- yr_stat(compare_year, 0.5)

by_age <- summarise_by(df, "networth", "agecl", 0.25) %>% rename(p25 = value) %>%
  left_join(summarise_by(df, "networth", "agecl", 0.5)  %>% rename(median = value), by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0.75) %>% rename(p75 = value),    by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0.9)  %>% rename(p90 = value),    by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0)    %>% rename(average = value), by = "agecl")

# Display-only version with an "All Ages" row. The say() checks below use
# by_age (age groups only) so the All Ages row can't skew min/max/counts.
by_age_all <- bind_rows(
  by_age %>% mutate(agecl = as.character(agecl)),
  tibble(agecl   = "All Ages",
         p25     = wtd_stat(df$networth, df$wgt, 0.25),
         median  = med_now,
         p75     = wtd_stat(df$networth, df$wgt, 0.75),
         p90     = wtd_stat(df$networth, df$wgt, 0.9),
         average = avg_now)
)

cat("\n================ NUMBERS FOR THE POST (in order) ================\n")
cat("All figures in", dollar_year, "dollars\n")

# ---- Average Net Worth ----
cat("\n-- What is the Average Net Worth? --\n")
say(paste0("Average NW ", data_year), fd(avg_now))
say(paste0("Change in average NW since ", compare_year),
    paste0(fd(avg_now - avg_cmp), " (", pct_chg(avg_now, avg_cmp), ")"))
say(paste0("Average NW ", compare_year), fd(avg_cmp))
say(paste0("Median NW ", data_year, " (all ages)"), fd(med_now))
say("Median / average",
    paste0(round(med_now / avg_now, 2), " (about 1/", round(avg_now / med_now, 1), ")"))
say(paste0("Change in median NW since ", compare_year, " ('about $50,000')"),
    paste0(fd(med_now - med_cmp), " (", pct_chg(med_now, med_cmp), ")"))
say(paste0("Median NW ", compare_year, " (all ages)"), fd(med_cmp))

# ---- Net Worth by Age ----
cat("\n-- What is the Average Net Worth by Age? --\n")
say("Average NW by age: lowest",
    paste0(fd(min(by_age$average)), " (", by_age$agecl[which.min(by_age$average)], ")"))
say("Average NW by age: highest",
    paste0(fd(max(by_age$average)), " (", by_age$agecl[which.max(by_age$average)], ")"))
say("Median NW by age: peak age group ('rising to age 65-74')",
    paste0(by_age$agecl[which.max(by_age$median)], " (", fd(max(by_age$median)), ")"))

cat("\nFine-grained table:\n")
print_df(age_detail_table)

# Exclude the All Ages row from the peak checks
age_detail_ages <- age_detail_table %>% filter(`Age Range` != "All Ages")

say("Fine table: peak AVERAGE age range ('wealth peaks in 65-69')",
    age_detail_ages$`Age Range`[which.max(parse_number(age_detail_ages$`Average Net Worth`))])
say("Fine table: peak MEDIAN age range",
    age_detail_ages$`Age Range`[which.max(parse_number(age_detail_ages$`Median Net Worth`))])

# ---- Worse or Better Than the Median ----
cat("\n-- Doing Worse or Better Than the Median? --\n")
cat("Percentiles by age (25th / median / 75th / 90th / average):\n")
print_df(by_age_all %>% mutate(across(-agecl, fd)))

say(paste0("25th percentile positive for every age group in ", data_year),
    paste0(all(by_age$p25 > 0), " (lowest: ", fd(min(by_age$p25)), ", ",
           by_age$agecl[which.min(by_age$p25)], ")"))
say("75th percentile closer to median or average? (count of age groups closer to average)",
    paste0(sum(abs(by_age$p75 - by_age$average) < abs(by_age$p75 - by_age$median)),
           " of ", nrow(by_age)))

# ---- Net worth excluding home equity ----
cat("\n-- What Does This Look Like Without Home Equity? --\n")

med_ex_home_now <- wtd_stat(df$nw_ex_home, df$wgt, 0.5)

say(paste0("Median NW ex home equity ", data_year, " (all ages)"),
    fd(med_ex_home_now))
say(paste0("Median NW ", data_year, " (all ages): with vs. ex home equity"),
    paste0(fd(med_now), " vs. ", fd(med_ex_home_now),
           " (ex home is ", round(100 * med_ex_home_now / med_now), "% of total)"))
say(paste0("Average NW ex home equity ", data_year, " (all ages)"),
    fd(wtd_stat(df$nw_ex_home, df$wgt, 0)))

# Renter example for the text: same $150k, ranked against total net worth
# vs. against net worth excluding home equity (a renter's two are identical)
ex_age <- "35-44"
ex_nw  <- 150000
d_ex   <- df %>% filter(as.character(agecl) == ex_age)
say(paste0("Renter, age ", ex_age, ", ", fd(ex_nw), ": percentile on total NW"),
    paste0(round(100 * sum(d_ex$wgt[d_ex$networth <= ex_nw]) / sum(d_ex$wgt)), "th"))
say(paste0("Renter, age ", ex_age, ", ", fd(ex_nw), ": percentile on NW excl. home equity"),
    paste0(round(100 * sum(d_ex$wgt[d_ex$nw_ex_home <= ex_nw]) / sum(d_ex$wgt)), "th"))

ex_home_table <- comb_df %>%
  select(agecl, pct_label, with_home = value) %>%
  left_join(comb_df_ex %>% select(agecl, pct_label, ex_home = value),
            by = c("agecl", "pct_label")) %>%
  mutate(home_share = paste0(round(100 * (1 - ex_home / with_home)), "%"))

# "All Ages" rows: each percentile with and without home equity, computed on
# every household (re-ranked separately on each measure, same as the age rows)
age_order <- c(if(is.factor(df$agecl)) levels(droplevels(df$agecl))
               else sort(unique(as.character(df$agecl))),
               "All Ages")

all_ages_ex <- map_dfr(comb_probs, function(p){
  tibble(agecl     = "All Ages",
         pct_label = paste0(100 * p, "th Percentile"),
         with_home = wtd_stat(df$networth,   df$wgt, p),
         ex_home   = wtd_stat(df$nw_ex_home, df$wgt, p))
}) %>%
  mutate(home_share = paste0(round(100 * (1 - ex_home / with_home)), "%"))

ex_home_table <- bind_rows(
  ex_home_table %>% mutate(agecl = as.character(agecl),
                           pct_label = as.character(pct_label)),
  all_ages_ex
) %>%
  mutate(agecl     = factor(agecl, levels = age_order),
         pct_label = factor(pct_label, levels = levels(comb_df$pct_label)))

cat("\nPercentiles by age, with vs. without home equity\n")
cat("(home_share = how much lower the percentile is once home equity is removed.\n")
cat(" Households are re-ranked without home equity, so this is not the exact home\n")
cat(" equity share of any one household):\n")
print_df(ex_home_table %>%
           mutate(across(c(with_home, ex_home), fd)) %>%
           arrange(pct_label, agecl))

# ---- Home equity share for the SAME households ----
# Unlike the table above (two separate rankings), this ranks households once
# on total net worth within their age group, takes a band around each
# percentile, and asks: for these households, how much of their net worth
# is home equity? Bands are +/-10 percentiles wide (the 90th uses 80-100)
# so each band holds enough households to be stable.
share_bands <- tribble(
  ~band,              ~lo,  ~hi,
  "~25th (15th-35th)", 0.15, 0.35,
  "~50th (40th-60th)", 0.40, 0.60,
  "~75th (65th-85th)", 0.65, 0.85,
  "~90th (80th-100th)",0.80, 1.00
) %>%
  mutate(band = factor(band, levels = band))

df_ranked <- df %>%
  group_by(agecl) %>%
  arrange(networth, .by_group = TRUE) %>%
  mutate(nw_rank = (cumsum(wgt) - wgt / 2) / sum(wgt)) %>%
  ungroup()

home_share_table <- pmap_dfr(share_bands, function(band, lo, hi){
  df_ranked %>%
    filter(nw_rank >= lo, nw_rank < hi) %>%
    group_by(agecl) %>%
    summarise(
      band            = band,
      median_nw       = wtd_stat(networth, wgt, 0.5),
      median_homeeq   = wtd_stat(homeeq, wgt, 0.5),
      pct_with_homeeq = sum(wgt[homeeq > 0]) / sum(wgt),
      # share only defined when net worth is positive
      median_home_share = wtd_stat(homeeq[networth > 0] / networth[networth > 0],
                                   wgt[networth > 0], 0.5),
      .groups = "drop"
    )
}) %>%
  mutate(band = factor(band, levels = levels(share_bands$band)))

# ---- Chart: home equity vs. everything else, middle vs. top 20% ----
share_chart_bands <- c("~50th (40th-60th)"  = "Middle of Age Group\n(40th-60th Percentile)",
                       "~90th (80th-100th)" = "Top 20% of Age Group\n(80th-100th Percentile)")

share_parts <- c("Home equity", "Everything else")

share_plot_df <- home_share_table %>%
  filter(as.character(band) %in% names(share_chart_bands)) %>%
  mutate(panel = factor(share_chart_bands[as.character(band)],
                        levels = share_chart_bands),
         home  = pmin(pmax(median_home_share, 0), 1)) %>%
  select(panel, agecl, home) %>%
  mutate(other = 1 - home) %>%
  pivot_longer(c(home, other), names_to = "part", values_to = "share") %>%
  mutate(part = factor(ifelse(part == "home", share_parts[1], share_parts[2]),
                       levels = rev(share_parts)))

share_labels <- share_plot_df %>%
  filter(part == share_parts[1]) %>%
  mutate(label  = paste0(round(100 * share), "%"),
         inside = share >= 0.08,
         y      = ifelse(inside, share / 2, share + 0.02))

plot <- ggplot(share_plot_df, aes(x = agecl, y = share, fill = part)) +
  geom_col(width = 0.75) +
  geom_text(data = share_labels %>% filter(inside),
            aes(x = agecl, y = y, label = label),
            inherit.aes = FALSE, col = "white", fontface = "bold", size = 3) +
  geom_text(data = share_labels %>% filter(!inside),
            aes(x = agecl, y = y, label = label),
            inherit.aes = FALSE, col = chart_standard_color,
            fontface = "bold", size = 3, vjust = 0) +
  facet_wrap(vars(panel), axes = "all") +
  scale_fill_manual(values = setNames(c(chart_standard_color, "#a9bfd6"),
                                      share_parts),
                    breaks = share_parts) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  of_dollars_and_data_theme +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title    = element_blank()) +
  ggtitle("How Much of Net Worth Is Home Equity?\nMedian Share by Age") +
  labs(x = "Age", y = "Share of Net Worth",
       caption = paste0(source_string, "\n",
                        str_wrap(paste0("Note: Households ranked by total net worth ",
                                        "within their age group. Bars show the ",
                                        "median share of net worth held in home ",
                                        "equity."), width = 85)))

save_chart(plot, paste0(out_path, "/home_equity_share_by_age_",
                        data_year, ".jpeg"))

cat("Home equity share of net worth for the SAME households\n")
cat("(households ranked once by total net worth within their age group):\n")
print_df(home_share_table %>%
           arrange(band, agecl) %>%
           transmute(band, agecl,
                     median_nw       = fd(median_nw),
                     median_homeeq   = fd(median_homeeq),
                     pct_with_homeeq = paste0(round(100 * pct_with_homeeq), "%"),
                     median_home_share = paste0(round(100 * median_home_share), "%")))

# ---- Appendix: full data behind the distribution charts ----
cat("\n================ APPENDIX: FULL DATA ================\n")

cat("\n")
cat("Net worth distribution by age (10th-90th percentiles):\n")
print_df(age_dist_networth %>%
           transmute(agecl, pct_label, value = fd(value)) %>%
           pivot_wider(names_from = agecl, values_from = value))

cat("\n=================================================================\n")

sink()
print(paste0("Log written to: ", log_file))

# ############################  End  ################################## #