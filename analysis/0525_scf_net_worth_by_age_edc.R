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

folder_name <- "0525_scf_net_worth_by_age_edc"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)
dist_path   <- paste0(out_path, "/age_distribution")
ts_path     <- paste0(out_path, "/over_time")

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)
dir.create(file.path(paste0(dist_path)), showWarnings = FALSE)
dir.create(file.path(paste0(ts_path)), showWarnings = FALSE)

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
         networth, homeeq, fin, wgt,
         agecl, edcl) %>%
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
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
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
  
  # ##### 1. Age x Education grid #####
  to_plot <- summarise_by(df, var, c("edcl", "agecl"), quantile_prob)
  
  assign(paste0("age_edc_", var, "_", qps), to_plot, envir = .GlobalEnv)
  
  export_to_excel(to_plot %>%
                    mutate(value = format_as_dollar(value)),
                  excel_path,
                  paste0("age_edc_", var, "_", qps),
                  create_new_file,
                  0)
  
  if(create_new_file == 1){
    assign("create_new_file", 0, envir = .GlobalEnv)
  }
  
  # Labels are formatted PER FACET because the facets use free_y. A
  # College Degree panel in the millions shouldn't dictate the label
  # format for the No High School panel.
  text_labels <- to_plot %>%
    group_by(edcl) %>%
    mutate(label = make_dollar_labels(value)) %>%
    ungroup() %>%
    mutate(vj = ifelse(value > 0, 0, 1))
  
  file_path <- paste0(out_path, "/", var, "_", qps,
                      "_age_edc_comb_", data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", position = "dodge",
             fill = chart_standard_color) +
    # ggplot2's own facet_wrap repeats axes on every panel. lemon's
    # facet_rep_wrap breaks on current ggplot2 versions.
    facet_wrap(vars(edcl), scales = "free_y", axes = "all") +
    geom_text(data = text_labels,
              aes(x = agecl, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = 1.8) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0(str_wrap(var_title, width = 38),
                   "\nby Age & Education Level")) +
    labs(x = "Age", y = paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ---- Grid table: rows are age, columns are education ----
  grid_table <- to_plot %>%
    mutate(display = format_as_dollar(value)) %>%
    select(agecl, edcl, display) %>%
    pivot_wider(names_from = edcl, values_from = display) %>%
    rename(Age = agecl)
  
  write_html_table(grid_table,
                   paste0(out_path, "/", var, "_", qps,
                          "_age_edc_comb_", data_year, "_table.html"))
  
  # ##### 2. Age only, then Education only #####
  for(g in 1:2){
    if(g == 1){
      group_var    <- "agecl"
      end_filename <- "age"
      x_var        <- "Age"
    } else {
      group_var    <- "edcl"
      end_filename <- "edc"
      x_var        <- "Education Level"
    }
    
    to_plot <- summarise_by(df, var, group_var, quantile_prob)
    
    text_labels <- to_plot %>%
      mutate(label = make_dollar_labels(value),
             vj    = ifelse(value > 0, -0.2, 1.2))
    
    file_path <- paste0(out_path, "/", var, "_", qps, "_",
                        end_filename, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = .data[[group_var]], y = value)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      geom_text(data = text_labels,
                aes(x = .data[[group_var]], y = value, label = label,
                    vjust = vj),
                col = chart_standard_color,
                size = 3) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0(str_wrap(var_title, width = 38), "\nby ", x_var)) +
      labs(x = x_var, y = paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
    
    # ---- Table ----
    table_out <- to_plot %>%
      transmute(Group = as.character(.data[[group_var]]),
                Value = format_as_dollar(value))
    
    names(table_out)[1] <- x_var
    names(table_out)[2] <- var_title
    
    write_html_table(table_out,
                     paste0(out_path, "/", var, "_", qps, "_",
                            end_filename, "_table.html"))
  }
}

create_new_file <- 1

create_percentile_chart("networth", "25th Percentile Net Worth", 0.25)
create_percentile_chart("networth", "Median Net Worth", 0.5)
create_percentile_chart("homeeq", "Median Home Equity", 0.5)
create_percentile_chart("fin", "Median Financial Assets", 0.5)
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

comb_df <- map_dfr(comb_probs, function(p){
  summarise_by(df, "networth", "agecl", p) %>%
    mutate(pct = p)
}) %>%
  mutate(pct_label = factor(paste0(100 * pct, "th Percentile"),
                            levels = paste0(100 * comb_probs, "th Percentile")))

comb_labels <- comb_df %>%
  group_by(pct_label) %>%
  mutate(label = make_dollar_labels(value)) %>%
  ungroup() %>%
  mutate(vj = ifelse(value >= 0, -0.2, 1.2))

plot <- ggplot(comb_df, aes(x = agecl, y = value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  facet_wrap(vars(pct_label), axes = "all") +
  geom_text(data = comb_labels,
            aes(x = agecl, y = value, label = label, vjust = vj),
            col = chart_standard_color,
            size = 1.5) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Net Worth Percentiles by Age") +
  labs(x = "Age", y = "Net Worth",
       caption = paste0(source_string, "\n", note_string))

save_chart(plot, paste0(out_path, "/networth_pct_25_50_75_90_age_comb_",
                        data_year, ".jpeg"))

# ##################################################################### #
# OVER TIME: real median by survey year (1989 - data_year)
# Charts in the "How Has American Net Worth Changed Over Time?" section
# ##################################################################### #

ts_vars <- intersect(c("networth", "fin", "vehic"), names(scf_all))

if(!"vehic" %in% ts_vars){
  warning("Column 'vehic' not found in 0003_scf_stack.Rds - the under-35 ",
          "median vehicle value chart will be skipped. Add vehic to 0003.")
}

df_ts <- scf_all %>%
  filter(year <= data_year) %>%
  select(year, age, agecl, wgt, all_of(ts_vars))

ts_note <- paste0("Note:  All figures are adjusted for inflation (",
                  dollar_year, " dollars).")
ts_source <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"

ts_median <- function(data, var, group_vars = "year"){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(value = as.numeric(wtd.quantile(.data[[var]], weights = wgt,
                                              probs = 0.5)),
              .groups = "drop")
}

ts_line_chart <- function(to_plot, y_title, file_name, title = NULL){
  plot <- ggplot(to_plot, aes(x = year, y = value)) +
    geom_line(col = "black") +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(breaks = sort(unique(to_plot$year))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = y_title,
         caption = paste0(ts_source, "\n", ts_note))
  
  if(!is.null(title)) plot <- plot + ggtitle(title)
  
  save_chart(plot, paste0(ts_path, "/", file_name))
}

# 1. Real median net worth by year (all households)
median_nw_by_year <- ts_median(df_ts, "networth")
ts_line_chart(median_nw_by_year, "Real Median Net Worth",
              paste0("median_networth_by_year_", data_year, ".jpeg"),
              "Real Median Net Worth\nby Year")

# 2. Real median net worth by year, faceted by age (same y-axis)
median_nw_by_year_age <- ts_median(df_ts, "networth", c("year", "agecl"))

plot <- ggplot(median_nw_by_year_age, aes(x = year, y = value)) +
  geom_line(col = "black") +
  facet_wrap(vars(agecl), axes = "all") +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(breaks = seq(1990, data_year, 10)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Real Median Net Worth",
       caption = paste0(ts_source, "\n", ts_note))

save_chart(plot, paste0(ts_path, "/median_networth_by_year_age_",
                        data_year, ".jpeg"))

# 3-5. Under-35 households: net worth, financial assets, vehicle value
df_ts_u35 <- df_ts %>% filter(age < 35)

u35_specs <- tribble(
  ~var,       ~y_title,
  "networth", "Real Median Net Worth",
  "fin",      "Real Median Financial Assets",
  "vehic",    "Real Median Vehicle Value"
) %>%
  filter(var %in% ts_vars)

u35_results <- list()

for(i in seq_len(nrow(u35_specs))){
  v <- u35_specs$var[i]
  to_plot <- ts_median(df_ts_u35, v)
  u35_results[[v]] <- to_plot
  ts_line_chart(to_plot, u35_specs$y_title[i],
                paste0("median_", v, "_by_year_under35_", data_year, ".jpeg"))
}

# Over-time data for the text, plus one Excel sheet
over_time_table <- median_nw_by_year %>%
  rename(all_households = value) %>%
  left_join(median_nw_by_year_age %>%
              mutate(agecl = paste0("age_", agecl)) %>%
              pivot_wider(names_from = agecl, values_from = value),
            by = "year")

for(v in names(u35_results)){
  over_time_table <- over_time_table %>%
    left_join(u35_results[[v]] %>% rename(!!paste0("under35_", v) := value),
              by = "year")
}

export_to_excel(over_time_table %>%
                  mutate(across(-year, format_as_dollar)),
                excel_path,
                "median_over_time",
                create_new_file,
                0)

# ##################################################################### #
# NEW: Distribution of a variable WITHIN each age cohort
# For each agecl, plot the 10th, 20th, ..., 90th weighted percentiles.
# Outputs (in out_path/age_distribution/):
#   - one chart per age cohort
#   - one faceted chart with every cohort side by side
#   - one HTML table (rows = percentile, columns = age cohort)
#   - one sheet in all_var_summaries.xlsx
# ##################################################################### #

create_age_distribution_charts <- function(var, var_title, probs = dist_probs){
  
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
  
  # ##### 2. All cohorts faceted together #####
  # Labels formatted per facet since facets use free_y
  text_labels <- dist_df %>%
    group_by(agecl) %>%
    mutate(label = make_dollar_labels(value)) %>%
    ungroup() %>%
    mutate(vj = ifelse(value >= 0, 0, 1))
  
  file_path <- paste0(dist_path, "/", var, "_dist_age_comb_",
                      data_year, ".jpeg")
  
  plot <- ggplot(dist_df, aes(x = pct_label, y = value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    facet_wrap(vars(agecl), scales = "free_y", axes = "all") +
    geom_text(data = text_labels,
              aes(x = pct_label, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = 1.5) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
    ggtitle(paste0(str_wrap(paste0(var_title, " Distribution"), width = 38),
                   "\nby Age")) +
    labs(x = "Percentile", y = var_title,
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ##### 3. Table: rows = percentile, columns = age cohort #####
  dist_table <- dist_df %>%
    mutate(display = format_as_dollar(value)) %>%
    select(pct_label, agecl, display) %>%
    pivot_wider(names_from = agecl, values_from = display) %>%
    rename(Percentile = pct_label)
  
  write_html_table(dist_table,
                   paste0(dist_path, "/", var, "_dist_age_",
                          data_year, "_table.html"))
  
  print(paste0("Wrote ", length(cohorts), " cohort distribution charts for ",
               var, " to: ", dist_path))
}

create_age_distribution_charts("networth", "Net Worth")

# ##################################################################### #
# Fine-grained age table (average and median side by side)
# This is the 0369-style table the merged post needs alongside the
# education charts above.
# ##################################################################### #

age_detail_table <- df %>%
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
                               TRUE     ~ "75-80")) %>%
  group_by(agecl_new) %>%
  summarise(
    avg    = format_as_dollar(as.numeric(wtd.mean(networth, weights = wgt))),
    pct_50 = format_as_dollar(as.numeric(wtd.quantile(networth, weights = wgt,
                                                      probs = 0.5))),
    .groups = "drop"
  ) %>%
  select(`Age Range` = agecl_new,
         `Average Net Worth` = avg,
         `Median Net Worth`  = pct_50)

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

compare_year <- 2019   # "since 2019"
prior_year   <- 2022   # "over the past three years"
gfc_year     <- 2007   # "pre-GFC"
first_year   <- min(df_ts$year)

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
avg_prior <- yr_stat(prior_year, 0)
med_now   <- yr_stat(data_year, 0.5)
med_cmp   <- yr_stat(compare_year, 0.5)
med_prior <- yr_stat(prior_year, 0.5)
med_first <- yr_stat(first_year, 0.5)
med_gfc   <- yr_stat(gfc_year, 0.5)

by_age <- summarise_by(df, "networth", "agecl", 0.25) %>% rename(p25 = value) %>%
  left_join(summarise_by(df, "networth", "agecl", 0.5)  %>% rename(median = value), by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0.75) %>% rename(p75 = value),    by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0.9)  %>% rename(p90 = value),    by = "agecl") %>%
  left_join(summarise_by(df, "networth", "agecl", 0)    %>% rename(average = value), by = "agecl")

cat("\n================ NUMBERS FOR THE POST (in order) ================\n")
cat("All figures in", dollar_year, "dollars\n")

# ---- What is Net Worth? ----
cat("\n-- What is Net Worth? --\n")
say(paste0("Median NW change ", prior_year, " -> ", data_year,
           " ('grown substantially over the past three years')"),
    paste0(fd(med_now - med_prior), " (", pct_chg(med_now, med_prior), ")"))
say(paste0("Average NW change ", prior_year, " -> ", data_year),
    paste0(fd(avg_now - avg_prior), " (", pct_chg(avg_now, avg_prior), ")"))

# ---- Average Net Worth ----
cat("\n-- What is the Average Net Worth? --\n")
say(paste0("Average NW ", data_year), fd(avg_now))
say(paste0("Change in average NW since ", compare_year), fd(avg_now - avg_cmp))
say(paste0("Average NW ", compare_year), fd(avg_cmp))
say(paste0("Median NW ", data_year), fd(med_now))
say("Median / average",
    paste0(round(med_now / avg_now, 2), " (about 1/", round(avg_now / med_now, 1), ")"))
say(paste0("Change in median NW since ", compare_year), fd(med_now - med_cmp))
say(paste0("Median NW ", compare_year), fd(med_cmp))

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

say("Fine table: peak AVERAGE age range ('wealth peaks in 65-69')",
    age_detail_table$`Age Range`[which.max(parse_number(age_detail_table$`Average Net Worth`))])
say("Fine table: peak MEDIAN age range",
    age_detail_table$`Age Range`[which.max(parse_number(age_detail_table$`Median Net Worth`))])

# ---- Worse or Better Than the Median ----
cat("\n-- Doing Worse or Better Than the Median? --\n")
cat("Percentiles by age (25th / median / 75th / 90th / average):\n")
print_df(by_age %>% mutate(across(-agecl, fd)))

say(paste0("25th percentile positive for every age group in ", data_year),
    paste0(all(by_age$p25 > 0), " (lowest: ", fd(min(by_age$p25)), ", ",
           by_age$agecl[which.min(by_age$p25)], ")"))
say(paste0("Under-35 25th percentile NW ", compare_year, " ('near $0')"),
    fd(yr_stat(compare_year, 0.25, df_ts %>% filter(age < 35))))
say(paste0("Under-35 25th percentile NW ", data_year),
    fd(yr_stat(data_year, 0.25, df_ts %>% filter(age < 35))))
say("75th percentile closer to median or average? (count of age groups closer to average)",
    paste0(sum(abs(by_age$p75 - by_age$average) < abs(by_age$p75 - by_age$median)),
           " of ", nrow(by_age)))

# ---- Over Time ----
cat("\n-- How Has American Net Worth Changed Over Time? --\n")
say(paste0("Median NW ", first_year), fd(med_first))
say(paste0("Median NW ", data_year), fd(med_now))
say(paste0("Multiple ", first_year, " -> ", data_year, " ('more than doubled')"),
    paste0(round(med_now / med_first, 2), "x"))
say(paste0("Median NW change ", compare_year, " -> ", data_year, " ('increased by 37%')"),
    pct_chg(med_now, med_cmp))
say(paste0("Median NW ", gfc_year, " (pre-GFC)"),
    paste0(fd(med_gfc), " | ", data_year, " above pre-GFC: ", med_now > med_gfc))

cat("\nMedian NW by age, ", gfc_year, " vs ", data_year,
    " ('every age group roughly the same or better than 2007'):\n", sep = "")
print_df(median_nw_by_year_age %>%
           filter(year %in% c(gfc_year, data_year)) %>%
           pivot_wider(names_from = year, values_from = value,
                       names_prefix = "y") %>%
           mutate(change = pct_chg(.data[[paste0("y", data_year)]],
                                   .data[[paste0("y", gfc_year)]])) %>%
           mutate(across(starts_with("y"), fd)))

cat("Under-35 households ('exploded in the last three years'):\n")
for(v in names(u35_results)){
  r <- u35_results[[v]]
  v_cmp   <- r$value[r$year == compare_year]
  v_prior <- r$value[r$year == prior_year]
  v_now   <- r$value[r$year == data_year]
  say(paste0("Under-35 median ", v, " ", compare_year, " / ", prior_year, " / ", data_year),
      paste0(fd(v_cmp), " / ", fd(v_prior), " / ", fd(v_now),
             " | ", prior_year, "->", data_year, ": ", pct_chg(v_now, v_prior),
             " | ", compare_year, "->", data_year, ": ", pct_chg(v_now, v_cmp)))
}

cat("\n=================================================================\n")

# ############################  End  ################################## #