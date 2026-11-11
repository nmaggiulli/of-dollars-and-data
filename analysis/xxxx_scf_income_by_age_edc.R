cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(Hmisc)
library(xtable)
library(tidyverse)

# Wide enough that logged tables don't wrap
options(width = 200)

########################## Parameters ################################# #

data_year  <- 2025
prior_year <- 2022

# The SCF asks about income for the PRIOR calendar year, so the 2025
# survey's income figures describe 2024. The log prints this for the post.
income_year       <- data_year - 1
prior_income_year <- prior_year - 1

# Percentiles for the headline "what is rich" numbers
rich_probs <- c(0.90, 0.95, 0.99)

# Round-number thresholds for the "what share of households clear this?" section
income_thresholds <- c(100000, 150000, 250000, 400000, 500000, 1000000)

# Example incomes quoted in the text
intro_example_income <- 100000  # Intro: "$100,000 ... rich at 18, but not at 50"
edc_example_income   <- 250000  # Education: "$250,000 with a college degree may not feel rich"

# Education levels named in the text
college_level <- "College Degree"
hs_level      <- "High School"

# Flag any percentile cell backed by fewer sample households than this
min_support_hh <- 10

# Level order for the SCF age and education classes. The script fails if
# the stack's labels do not match these.
agecl_order <- c("<35", "35-44", "45-54", "55-64", "65-74", "75+")
edcl_order  <- c("No High School", "High School", "Some College", "College Degree")

# ---- Inflation (Section 3 only, and only if the stack is nominal) ----
# CPI-U annual averages. Entered from memory: VERIFY against BLS series
# CUUR0000SA0 before publishing. Defaults are the INCOME years (2021, 2024).
# If the stack stores income in survey-year dollars, use 2022 and 2025.
cpi_prior  <- 270.970
cpi_latest <- 313.689

# Published 2022 headline values (top 10% / 5% / 1%). Used to test whether
# the stack is nominal. Set to NULL if prior_year is not 2022.
published_prior_headline <- c(248610, 390209, 1199812)

# Dollar basis of the stack. NA = detect from the published values above.
# TRUE = stack already in constant dollars. FALSE = stack is nominal.
# First run: the stack's 2022 values were 1.0944x the published ones at
# every threshold, so the stack is in constant (2025) dollars and the CPI
# inputs are not used.
# (The auto check assumes the stack was not rebased to prior_year dollars.)
prior_already_real <- NA

# ---- Chart styling ----
facet_label_size <- 1.8
prior_year_color <- "#B3B3B3"

########################## Output paths ############################### #

folder_name <- "xxxx_scf_income_by_age_edc"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

excel_path <- paste0(out_path, "/all_var_summaries.xlsx")
log_path   <- paste0(out_path, "/post_numbers_", data_year, ".txt")

########################## Load data ################################## #

scf_stack_all <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack_all$year)

df <- scf_stack_all %>%
  filter(year == data_year) %>%
  select(hh_id, imp_id, age, income, wgt, agecl, edcl) %>%
  mutate(agecl = factor(as.character(agecl), levels = agecl_order),
         edcl  = factor(as.character(edcl),  levels = edcl_order)) %>%
  arrange(hh_id, imp_id)

stopifnot(!any(is.na(df$agecl)), !any(is.na(df$edcl)))

n_hh  <- n_distinct(df$hh_id)
n_imp <- n_distinct(df$imp_id)

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households."),
                        width = 85)

########################## Logging helpers ############################ #
# Every number quoted in the post goes through these, so the log file is a
# single checklist to fact-check the draft against.

if(file.exists(log_path)){ file.remove(log_path) }

log_line <- function(...){
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  cat(txt, "\n", sep = "", file = log_path, append = TRUE)
}

log_section <- function(title){
  log_line("")
  log_line(strrep("=", 78))
  log_line(title)
  log_line(strrep("=", 78))
}

log_sub <- function(title){
  log_line("")
  log_line("--- ", title, " ---")
}

log_value <- function(label, value){
  log_line(sprintf("  %-60s %s", label, value))
}

log_table <- function(tbl){
  walk(capture.output(print(as.data.frame(tbl), row.names = FALSE)),
       ~ log_line("  ", .x))
}

log_note <- function(...){
  log_line("  NOTE: ", ...)
}

########################## Formatting helpers ######################### #

# Dollar labels for charts. The $k / $M decision is made once per vector
# off the max, so "$2.0M" never renders as "$2,000k". Negatives get "-$".
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

fmt_dollar <- function(x){ format_as_dollar(x) }

# Short form for prose ("$249k", "$1.2M"). Unit picked per value.
fmt_short <- function(x){ map_chr(x, make_dollar_labels) }

# Blank instead of "$NA" for the first row of a lag()
fmt_dollar_blank <- function(x){
  ifelse(is.na(x), "", fmt_dollar(coalesce(x, 0)))
}

fmt_pct <- function(x, digits = 1){
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

fmt_signed_pct <- function(x, digits = 1){
  paste0(ifelse(x > 0, "+", ""), fmt_pct(x, digits))
}

fmt_count <- function(x, digits = 0){
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

rich_label <- function(p){
  paste0("Top ", formatC(100 * (1 - p), format = "f", digits = 0), "%")
}

quantile_prob_string <- function(quantile_prob){
  if(quantile_prob == 0){
    "avg"
  } else {
    str_pad(100 * quantile_prob, side = "left", width = 3, pad = "0")
  }
}

########################## Stat helpers ############################### #
# Weights are applied across all implicates at once. Scaling every weight
# by the same constant does not change a quantile, mean, or share, so this
# gives the same point estimates whether or not the stack divides wgt by 5.

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

# Weighted share of households at or below a value = its percentile rank
wtd_share_at_or_below <- function(x, w, value){
  as.numeric(wtd.mean(as.numeric(x <= value), weights = w))
}

wtd_share_at_or_above <- function(x, w, value){
  as.numeric(wtd.mean(as.numeric(x >= value), weights = w))
}

# Unweighted household count per cell
cell_counts <- function(data, group_vars){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(n_hh = n_distinct(hh_id), .groups = "drop")
}

# Sample households behind an upper percentile: rows at or above the cell's
# threshold, divided by the number of implicates. This counts actual rows
# instead of using n * (1 - p), because the SCF oversamples wealthy
# households. The naive formula understates support in the upper tail.
support_above <- function(data, group_vars, quantile_prob){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(threshold = wtd_stat(income, wgt, quantile_prob),
              n_hh      = n_distinct(hh_id),
              hh_above  = sum(income >= threshold) / n_distinct(imp_id),
              .groups   = "drop")
}

# Youngest vs. peak value within each education level
age_profile <- function(data, quantile_prob){
  summarise_by(data, "income", c("edcl", "agecl"), quantile_prob) %>%
    arrange(edcl, agecl) %>%
    group_by(edcl) %>%
    summarise(youngest_value = first(value),
              peak_age       = as.character(agecl[which.max(value)]),
              peak_value     = max(value),
              .groups        = "drop") %>%
    mutate(dollar_rise = peak_value - youngest_value,
           pct_rise    = peak_value / youngest_value - 1,
           # "Rises with age" = peak is NOT the youngest group AND the peak
           # is at least 25% above it. A small gap is noise, not a pattern.
           rises_with_age = (peak_age != agecl_order[1]) & (pct_rise > 0.25))
}

########################## Output helpers ############################# #

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
# SECTION A: Charts by age, education, and the two combined
# ##################################################################### #
# Same charts as before. This section draws only; the numbers are logged
# in post order further down.

create_percentile_chart <- function(var, var_title, quantile_prob){
  
  qps <- quantile_prob_string(quantile_prob)
  
  # ##### A1. Age x Education grid #####
  # NOTE: no scales = "free_y" here, deliberately. The shared axis is what
  # makes the College Degree panel visually dwarf the others, which is the
  # whole point of this chart in the post.
  to_plot <- summarise_by(df, var, c("edcl", "agecl"), quantile_prob)
  
  export_to_excel(to_plot %>%
                    mutate(value = format_as_dollar(value)),
                  excel_path,
                  paste0("age_edc_", var, "_", qps),
                  create_new_file,
                  0)
  
  if(create_new_file == 1){
    assign("create_new_file", 0, envir = .GlobalEnv)
  }
  
  text_labels <- to_plot %>%
    mutate(label = make_dollar_labels(value),
           vj    = ifelse(value > 0, 0, 1))
  
  file_path <- paste0(out_path, "/", var, "_", qps,
                      "_age_edc_comb_scf_", data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", position = "dodge",
             fill = chart_standard_color) +
    # ggplot2's own facet_wrap repeats axes on every panel. lemon's
    # facet_rep_wrap breaks on current ggplot2 versions.
    facet_wrap(vars(edcl), axes = "all") +
    geom_text(data = text_labels,
              aes(x = agecl, y = value, label = label, vjust = vj),
              col = chart_standard_color,
              size = facet_label_size) +
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
    arrange(agecl) %>%
    rename(Age = agecl)
  
  write_html_table(grid_table,
                   paste0(out_path, "/", var, "_", qps,
                          "_age_edc_comb_", data_year, "_table.html"))
  
  # ##### A2. Age only, then Education only #####
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
                        end_filename, "_scf_", data_year, ".jpeg")
    
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

create_stack <- function(var_name, var_title){
  create_percentile_chart(var_name, paste0("25th Percentile ", var_title), 0.25)
  create_percentile_chart(var_name, paste0("Median ", var_title), 0.5)
  create_percentile_chart(var_name, paste0("75th Percentile ", var_title), 0.75)
  create_percentile_chart(var_name, paste0("Average ", var_title), 0)
  create_percentile_chart(var_name, paste0("90th Percentile ", var_title), 0.9)
  create_percentile_chart(var_name, paste0("95th Percentile ", var_title), 0.95)
  create_percentile_chart(var_name, paste0("99th Percentile ", var_title), 0.99)
  # REMOVED: the 99.9th percentile. Nationally that is a handful of
  # households in the sample - not an estimate, just a few respondents.
}

create_stack("income", "Income")

# ##################################################################### #
# LOG HEADER
# ##################################################################### #

log_section(paste0("WHAT INCOME MAKES YOU RICH? - NUMBERS FOR THE POST (",
                   data_year, " SCF)"))
log_value("Run time", format(Sys.time(), "%Y-%m-%d %H:%M"))
log_value("Survey year", data_year)
log_value("Income year (SCF asks about the prior calendar year)", income_year)
log_value("Unweighted households (chart note)", fmt_count(n_hh))
log_value("Implicates per household", n_imp)
log_value("Rows in data (households x implicates)", fmt_count(nrow(df)))

# ##################################################################### #
# SECTION 1: What are the Top 10%, Top 5%, and Top 1% of Incomes?
# ##################################################################### #

log_section("1. WHAT ARE THE TOP 10%, TOP 5%, AND TOP 1% OF INCOMES?")

headline <- tibble(prob = rich_probs) %>%
  mutate(label = rich_label(prob),
         value = map_dbl(prob, ~ wtd_stat(df$income, df$wgt, .x)))

log_table(headline %>%
            transmute(Threshold = label,
                      Exact     = fmt_dollar(value),
                      Short     = fmt_short(value)))

log_value("Median household income (context)",
          fmt_dollar(wtd_stat(df$income, df$wgt, 0.5)))
log_value("'Rich could be defined as...' (short form)",
          paste(fmt_short(headline$value), collapse = " / "))

write_html_table(headline %>%
                   transmute(Threshold = label,
                             `Household Income` = format_as_dollar(value)),
                 paste0(out_path, "/income_headline_thresholds_",
                        data_year, "_table.html"))

# ##################################################################### #
# SECTION 2: Share of households above round-number thresholds
# ##################################################################### #
# Fits the "what makes you rich" framing better than percentiles alone -
# readers think in round numbers, not percentiles.

log_section("2. SHARE OF HOUSEHOLDS EARNING AT LEAST...")

threshold_share <- tibble(threshold = income_thresholds) %>%
  mutate(share = map_dbl(threshold,
                         ~ wtd_share_at_or_above(df$income, df$wgt, .x)),
         threshold_label = factor(format_as_dollar(threshold),
                                  levels = format_as_dollar(sort(income_thresholds))))

log_table(threshold_share %>%
            transmute(`Household Income`    = as.character(threshold_label),
                      `Share at or above`   = fmt_pct(share),
                      `Roughly 1 in ...`    = fmt_count(1 / share)))

text_labels <- threshold_share %>%
  mutate(label = fmt_pct(share))

file_path <- paste0(out_path, "/income_share_above_thresholds_",
                    data_year, ".jpeg")

plot <- ggplot(threshold_share, aes(x = threshold_label, y = share)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels,
            aes(x = threshold_label, y = share, label = label),
            col = chart_standard_color,
            vjust = -0.3,
            size = 3) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Share of U.S. Households Earning at Least...\n", data_year)) +
  labs(x = "Household Income", y = "Share of Households",
       caption = paste0(source_string, "\n", note_string))

save_chart(plot, file_path)

write_html_table(threshold_share %>%
                   transmute(`Household Income` = as.character(threshold_label),
                             `Share of Households` = fmt_pct(share)),
                 paste0(out_path, "/income_share_above_thresholds_",
                        data_year, "_table.html"))

# ##################################################################### #
# SECTION 3: Did the bar for "rich" outrun inflation?
# ##################################################################### #

log_section(paste0("3. WHAT CHANGED SINCE ", prior_year))

if(prior_year %in% scf_stack_all$year){
  
  df_prior <- scf_stack_all %>%
    filter(year == prior_year) %>%
    select(hh_id, imp_id, income, wgt)
  
  prior_stack_values <- map_dbl(rich_probs,
                                ~ wtd_stat(df_prior$income, df_prior$wgt, .x))
  
  cpi_factor <- cpi_latest / cpi_prior
  
  # ---- Dollar basis check ----
  # Compares the prior year in the stack to the published 2022 numbers.
  #   Ratio = 1 at every threshold  -> stack is nominal. Inflate the prior
  #                                    year with the CPI inputs.
  #   Same ratio > 1 at every       -> stack already applies one inflation
  #   threshold                        factor to the prior year (constant
  #                                    dollars). Use it as is. The ratio IS
  #                                    the stack's inflation factor.
  #   Anything else                 -> stop and look.
  log_sub("Dollar basis check")
  
  implied_factor <- NA_real_
  
  if(!is.null(published_prior_headline)){
    basis_ratio  <- prior_stack_values / published_prior_headline
    ratio_spread <- max(basis_ratio) / min(basis_ratio) - 1
    
    log_table(tibble(Threshold  = rich_label(rich_probs),
                     Published  = fmt_dollar(published_prior_headline),
                     `In stack` = fmt_dollar(prior_stack_values),
                     Ratio      = formatC(basis_ratio, format = "f", digits = 4)))
    
    if(is.na(prior_already_real)){
      if(all(abs(basis_ratio - 1) < 0.005)){
        prior_already_real <- FALSE
      } else if(ratio_spread < 0.001 & mean(basis_ratio) > 1){
        prior_already_real <- TRUE
      } else {
        stop("Dollar basis unclear. Stack / published ratios are ",
             paste(round(basis_ratio, 4), collapse = ", "),
             ". One common ratio = constant dollars; ratios of 1 = nominal. ",
             "Set prior_already_real by hand.")
      }
    }
    
    if(prior_already_real & ratio_spread < 0.001){
      implied_factor <- mean(basis_ratio)
    }
  }
  
  if(is.na(prior_already_real)){
    stop("Set prior_already_real: no published values to test the dollar basis against.")
  }
  
  if(prior_already_real){
    prior_factor <- 1
    log_value("Stack treated as", "constant dollars (no adjustment applied)")
    if(!is.na(implied_factor)){
      log_value(paste0("Inflation factor built into stack (", prior_year, " -> ", data_year, ")"),
                paste0(formatC(implied_factor, format = "f", digits = 4),
                       " (", fmt_signed_pct(implied_factor - 1), ")"))
    }
    log_note("CPI inputs (cpi_prior, cpi_latest) not used.")
  } else {
    prior_factor <- cpi_factor
    log_value("Stack treated as", "nominal (prior year inflated by CPI factor)")
    log_value("CPI factor (cpi_latest / cpi_prior) - VERIFY CPI inputs",
              formatC(cpi_factor, format = "f", digits = 4))
  }
  
  # Nominal prior value: the stack value itself if nominal, or the stack
  # value with its built-in factor removed if constant dollars.
  prior_nominal_divisor <- case_when(!prior_already_real   ~ 1,
                                     !is.na(implied_factor) ~ implied_factor,
                                     TRUE                   ~ NA_real_)
  
  threshold_compare <- tibble(prob = rich_probs) %>%
    mutate(label          = factor(rich_label(prob), levels = rich_label(rich_probs)),
           prior          = prior_stack_values * prior_factor,
           prior_nominal  = prior_stack_values / prior_nominal_divisor,
           latest         = headline$value,
           pct_change     = latest / prior - 1,
           nominal_change = latest / prior_nominal - 1)
  
  log_sub(paste0("Thresholds, ", prior_year, " vs. ", data_year))
  
  compare_log <- threshold_compare %>%
    transmute(Threshold = as.character(label),
              Prior     = fmt_dollar(prior),
              Latest    = fmt_dollar(latest),
              `Real change` = fmt_signed_pct(pct_change))
  
  if(!is.na(prior_nominal_divisor)){
    compare_log <- compare_log %>%
      mutate(`Prior (nominal)` = fmt_dollar(threshold_compare$prior_nominal),
             `Nominal change`  = fmt_signed_pct(threshold_compare$nominal_change))
  }
  
  names(compare_log)[2] <- paste0(prior_year, " (real)")
  names(compare_log)[3] <- as.character(data_year)
  
  log_table(compare_log)
  
  real_note <- str_wrap(paste0("Note: ", prior_year,
                               " figures adjusted for inflation",
                               ifelse(prior_already_real,
                                      paste0(" to ", data_year, " dollars."),
                                      " with CPI-U.")),
                        width = 85)
  
  # ---- Levels, side by side ----
  to_plot <- threshold_compare %>%
    select(label, prior, latest) %>%
    pivot_longer(cols = c(prior, latest),
                 names_to = "period", values_to = "value") %>%
    mutate(period = factor(ifelse(period == "prior",
                                  as.character(prior_year),
                                  as.character(data_year)),
                           levels = c(as.character(prior_year),
                                      as.character(data_year))))
  
  file_path <- paste0(out_path, "/income_thresholds_", prior_year, "_vs_",
                      data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = label, y = value, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(label = dollar) +
    scale_fill_manual(values = setNames(c(prior_year_color,
                                          chart_standard_color),
                                        c(as.character(prior_year),
                                          as.character(data_year)))) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("What It Takes to Be Rich\n", prior_year, " vs. ",
                   data_year)) +
    labs(x = "Threshold", y = "Household Income",
         caption = paste0(source_string, "\n", real_note))
  
  save_chart(plot, file_path)
  
  # ---- Real percent change ----
  text_labels <- threshold_compare %>%
    mutate(pct_label = fmt_signed_pct(pct_change),
           vj = ifelse(pct_change > 0, -0.3, 1.3))
  
  file_path <- paste0(out_path, "/income_thresholds_pct_change_",
                      prior_year, "_", data_year, ".jpeg")
  
  plot <- ggplot(threshold_compare, aes(x = label, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = label, y = pct_change, label = pct_label, vjust = vj),
              col = chart_standard_color,
              size = 3) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Real Change in the Income Needed to Be Rich\n",
                   prior_year, "-", data_year)) +
    labs(x = "Threshold", y = "Real Change",
         caption = paste0(source_string, "\n", real_note))
  
  save_chart(plot, file_path)
  
  # ---- Table ----
  compare_table <- threshold_compare %>%
    arrange(desc(prob)) %>%
    transmute(Threshold     = as.character(label),
              Prior         = format_as_dollar(prior),
              Latest        = format_as_dollar(latest),
              `Real Change` = fmt_signed_pct(pct_change))
  
  names(compare_table)[2] <- as.character(prior_year)
  names(compare_table)[3] <- as.character(data_year)
  
  write_html_table(compare_table,
                   paste0(out_path, "/income_thresholds_compare_table.html"))
  
} else {
  log_note("prior_year ", prior_year, " not in the stack - section skipped.")
}

# ##################################################################### #
# SECTION 4: How Does Income Change with Age?
# ##################################################################### #

log_section("4. HOW DOES INCOME CHANGE WITH AGE?")

age_rich <- map_dfr(rich_probs,
                    ~ summarise_by(df, "income", "agecl", .x) %>%
                      mutate(prob = .x))

age_counts     <- cell_counts(df, "agecl")
age_support_99 <- support_above(df, "agecl", 0.99)

log_sub("Thresholds by age group (charts: 90th, 95th, 99th by age)")
log_table(age_rich %>%
            mutate(label = rich_label(prob),
                   value = fmt_dollar(value)) %>%
            select(agecl, label, value) %>%
            pivot_wider(names_from = label, values_from = value) %>%
            left_join(age_counts, by = "agecl") %>%
            left_join(age_support_99 %>% select(agecl, hh_above), by = "agecl") %>%
            transmute(Age = agecl,
                      `Top 10%`, `Top 5%`, `Top 1%`,
                      `Sample HH` = fmt_count(n_hh),
                      `Sample HH above Top 1%` = fmt_count(hh_above, 1)))

age_99 <- age_rich %>% filter(prob == 0.99)

log_sub("Top 1% range across age groups (text: 'as little as X or as much as Y')")
log_value("Lowest",
          paste0(fmt_dollar(min(age_99$value)), " (",
                 as.character(age_99$agecl[which.min(age_99$value)]), ")"))
log_value("Highest",
          paste0(fmt_dollar(max(age_99$value)), " (",
                 as.character(age_99$agecl[which.max(age_99$value)]), ")"))

log_sub(paste0("Intro example: percentile rank of ",
               fmt_dollar(intro_example_income), " by age group"))
log_table(df %>%
            group_by(agecl) %>%
            summarise(share = wtd_share_at_or_below(income, wgt, intro_example_income),
                      .groups = "drop") %>%
            transmute(Age = agecl,
                      `Percentile rank` = fmt_pct(share),
                      `Top X%` = fmt_pct(1 - share)))

# ##################################################################### #
# SECTION 5: Fine-grained age table
# ##################################################################### #
# The published 2022 version carried a Top 1% column here. Split across
# twelve bands, each band's 99th percentile rests on a few households. It
# showed 30-34 at $468k jumping to $1,048k at 35-39, and 45-49 top 5%
# coming in BELOW 40-44 - impossible as population facts, ordinary as
# sampling noise. So this table stops at the top 5%. The top 1% lives in
# the 6-group age chart above. The diagnostic below shows the thinness.

log_section("5. AGE TABLE (5-YEAR BANDS)")

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
                                 TRUE     ~ "75-80"))
}

df_fine <- df %>% add_agecl_new()

fine_counts <- cell_counts(df_fine, "agecl_new")

rich_table_by_age <- df_fine %>%
  group_by(agecl_new) %>%
  summarise(
    pct_50 = wtd_stat(income, wgt, 0.50),
    pct_90 = wtd_stat(income, wgt, 0.90),
    pct_95 = wtd_stat(income, wgt, 0.95),
    .groups = "drop"
  ) %>%
  left_join(fine_counts, by = "agecl_new") %>%
  transmute(`Age Range`  = agecl_new,
            `Median`     = format_as_dollar(pct_50),
            `Top 10%`    = format_as_dollar(pct_90),
            `Top 5%`     = format_as_dollar(pct_95),
            `Households` = fmt_count(n_hh))

log_table(rich_table_by_age)
log_value("Households outside ages 20-80 (dropped from this table)",
          fmt_count(n_hh - n_distinct(df_fine$hh_id)))
log_note("The last band '75-80' covers six ages (75 through 80).")

write_html_table(rich_table_by_age,
                 paste0(out_path, "/income_by_agecl_table_rich_",
                        data_year, ".html"))

log_sub("DIAGNOSTIC - Top 1% by 5-year band. Do NOT publish.")
log_table(support_above(df_fine, "agecl_new", 0.99) %>%
            transmute(`Age Range` = agecl_new,
                      `Top 1%`    = fmt_dollar(threshold),
                      `Sample HH` = fmt_count(n_hh),
                      `Sample HH above Top 1%` = fmt_count(hh_above, 1)))

# ##################################################################### #
# SECTION 6: Peak earnings by threshold
# ##################################################################### #
# Uses the 6-group age classes for all three thresholds, so the peaks are
# comparable. (The 2022 post took peaks from the 5-year table, which mixed
# band widths with the charts.)

log_section("6. PEAK EARNINGS BY THRESHOLD (6 AGE GROUPS)")

peaks <- age_rich %>%
  group_by(prob) %>%
  summarise(peak_value = max(value),
            peak_age   = as.character(agecl[which.max(value)]),
            .groups    = "drop") %>%
  arrange(prob) %>%
  mutate(gap_from_prev = peak_value - lag(peak_value))

log_table(peaks %>%
            transmute(Threshold  = rich_label(prob),
                      `Peak`     = fmt_dollar(peak_value),
                      `Short`    = fmt_short(peak_value),
                      `Peak age` = peak_age,
                      `Extra vs. previous threshold` =
                        fmt_dollar_blank(gap_from_prev)))

# ##################################################################### #
# SECTION 7: How Does Income Change with Education Level?
# ##################################################################### #

log_section("7. HOW DOES INCOME CHANGE WITH EDUCATION LEVEL?")

college_df <- df %>% filter(edcl == college_level)

for(p in c(0.90, 0.99)){
  
  edc_tbl <- summarise_by(df, "income", "edcl", p) %>%
    mutate(gap_vs_prev_level = value - lag(value)) %>%
    left_join(support_above(df, "edcl", p) %>% select(edcl, n_hh, hh_above),
              by = "edcl")
  
  log_sub(paste0(rich_label(p), " by education level"))
  log_table(edc_tbl %>%
              transmute(Education = edcl,
                        Value     = fmt_dollar(value),
                        Short     = fmt_short(value),
                        `Gap vs. previous level` =
                          fmt_dollar_blank(gap_vs_prev_level),
                        `Sample HH` = fmt_count(n_hh),
                        `Sample HH above` = fmt_count(hh_above, 1)))
  
  college_value <- edc_tbl$value[edc_tbl$edcl == college_level]
  hs_value      <- edc_tbl$value[edc_tbl$edcl == hs_level]
  
  log_value(paste0(college_level, " minus ", hs_level),
            fmt_dollar(college_value - hs_value))
  log_value(paste0(college_level, " / ", hs_level),
            paste0(formatC(college_value / hs_value, format = "f", digits = 2), "x"))
  
  if(p == 0.90){
    log_sub(paste0("Text example: ", fmt_dollar(edc_example_income),
                   " with a college degree"))
    log_value(paste0("Percentile rank of ", fmt_dollar(edc_example_income),
                     ", all households"),
              fmt_pct(wtd_share_at_or_below(df$income, df$wgt, edc_example_income)))
    log_value(paste0("Percentile rank of ", fmt_dollar(edc_example_income),
                     ", ", college_level, " households"),
              fmt_pct(wtd_share_at_or_below(college_df$income, college_df$wgt,
                                            edc_example_income)))
  }
  
  if(p == 0.99){
    edc_99_spread <- max(edc_tbl$value) - min(edc_tbl$value)
    age_99_spread <- max(age_99$value) - min(age_99$value)
    
    log_sub("Top 1%: education spread vs. age spread (text: 'even greater than')")
    log_value("Spread across education levels (max - min)", fmt_dollar(edc_99_spread))
    log_value("Spread across age groups (max - min)",       fmt_dollar(age_99_spread))
    log_value("Education spread is larger?", as.character(edc_99_spread > age_99_spread))
    log_value("Difference", fmt_dollar(edc_99_spread - age_99_spread))
  }
}

# ##################################################################### #
# SECTION 8: Why Both Age and Education Level Matter
# ##################################################################### #

log_section("8. WHY BOTH AGE AND EDUCATION LEVEL MATTER")

grid_wide <- function(quantile_prob){
  summarise_by(df, "income", c("edcl", "agecl"), quantile_prob) %>%
    mutate(value = fmt_dollar(value)) %>%
    pivot_wider(names_from = edcl, values_from = value) %>%
    arrange(agecl) %>%
    rename(Age = agecl)
}

# ---- 8a. Grids and the "rises with age" check ----
# The 2022 post said income rose with age ONLY for College Degree
# households. This prints the check before you write the section.
for(p in c(0.90, 0.50)){
  
  stat_name <- ifelse(p == 0.5, "Median", rich_label(p))
  
  log_sub(paste0(stat_name, " by age x education (chart grid)"))
  log_table(grid_wide(p))
  
  prof <- age_profile(df, p)
  
  log_sub(paste0(stat_name, ": youngest group vs. peak, within each education level"))
  log_table(prof %>%
              transmute(Education  = edcl,
                        Youngest   = fmt_dollar(youngest_value),
                        `Peak age` = peak_age,
                        Peak       = fmt_dollar(peak_value),
                        `$ rise`   = fmt_dollar(dollar_rise),
                        `% rise`   = fmt_pct(pct_rise, 0),
                        `Rises with age` = rises_with_age))
  
  rising <- as.character(prof$edcl[prof$rises_with_age])
  
  log_value("Education levels where income rises with age",
            ifelse(length(rising) == 0, "(none)", paste(rising, collapse = ", ")))
  log_value(paste0("2022 claim ('only ", college_level, "') still holds?"),
            as.character(identical(rising, college_level)))
}

# ---- 8b. Reference group ----
# Why a top-10% household with a degree may not feel rich: where the
# national top-10% line for its age falls among college households that age.
log_sub(paste0("Reference group: national Top 10% line vs. ", college_level,
               " households of the same age"))

ref_group <- summarise_by(df, "income", "agecl", 0.90) %>%
  rename(all_90 = value) %>%
  left_join(summarise_by(college_df, "income", "agecl", 0.90) %>%
              rename(college_90 = value),
            by = "agecl") %>%
  mutate(pctl_within_college =
           map2_dbl(as.character(agecl), all_90, function(a, v){
             d <- college_df %>% filter(agecl == a)
             wtd_share_at_or_below(d$income, d$wgt, v)
           }))

log_table(ref_group %>%
            transmute(Age = agecl,
                      `Top 10%, all`     = fmt_dollar(all_90),
                      `Top 10%, college` = fmt_dollar(college_90),
                      `College / all`    = paste0(formatC(college_90 / all_90,
                                                          format = "f", digits = 2), "x"),
                      `All-HH line = college percentile` = fmt_pct(pctl_within_college)))

# ---- 8c. Oldest group above the next-oldest ----
# Readers will ask why 75+ can beat 65-74. Flags every case.
oldest_two <- tail(agecl_order, 2)

log_sub(paste0("Check: ", oldest_two[2], " above ", oldest_two[1], "?"))

late_check <- map_dfr(c(0.50, 0.90), function(p){
  bind_rows(summarise_by(df, "income", "agecl", p) %>%
              mutate(edcl = "All households"),
            summarise_by(df, "income", c("edcl", "agecl"), p) %>%
              mutate(edcl = as.character(edcl))) %>%
    filter(agecl %in% oldest_two) %>%
    mutate(agecl = as.character(agecl),
           stat  = ifelse(p == 0.5, "Median", rich_label(p)))
}) %>%
  pivot_wider(names_from = agecl, values_from = value) %>%
  mutate(older_is_higher = .data[[oldest_two[2]]] > .data[[oldest_two[1]]])

log_table(late_check %>%
            transmute(Stat = stat,
                      Group = edcl,
                      !!oldest_two[1] := fmt_dollar(.data[[oldest_two[1]]]),
                      !!oldest_two[2] := fmt_dollar(.data[[oldest_two[2]]]),
                      `Older higher?` = older_is_higher))

# ---- 8d. Thin cells ----
log_sub(paste0("Thin cells: fewer than ", min_support_hh,
               " sample households above the percentile"))

support_all <- bind_rows(
  support_above(df, "agecl", 0.99) %>%
    mutate(chart = "Top 1% by age",
           cell  = as.character(agecl)),
  support_above(df, "edcl", 0.99) %>%
    mutate(chart = "Top 1% by education",
           cell  = as.character(edcl)),
  support_above(df, c("edcl", "agecl"), 0.90) %>%
    mutate(chart = "Top 10% by age x education",
           cell  = paste0(edcl, " / ", agecl)),
  support_above(df, c("edcl", "agecl"), 0.99) %>%
    mutate(chart = "Top 1% by age x education (not in post)",
           cell  = paste0(edcl, " / ", agecl))
) %>%
  select(chart, cell, threshold, n_hh, hh_above)

log_table(support_all %>%
            group_by(chart) %>%
            summarise(`Fewest HH above` = fmt_count(min(hh_above), 1),
                      `In cell`         = cell[which.min(hh_above)],
                      `Cells flagged`   = sum(hh_above < min_support_hh),
                      .groups = "drop") %>%
            rename(Chart = chart))

flagged <- support_all %>% filter(hh_above < min_support_hh)

if(nrow(flagged) > 0){
  log_table(flagged %>%
              transmute(Chart = chart,
                        Cell  = cell,
                        Value = fmt_dollar(threshold),
                        `Sample HH` = fmt_count(n_hh),
                        `HH above`  = fmt_count(hh_above, 1)))
} else {
  log_value("Flagged cells", "(none)")
}

# ##################################################################### #
# SECTION 9: Sanity checks
# ##################################################################### #

log_section("9. SANITY CHECKS")

log_value("Weighted mean income (all households)",
          fmt_dollar(wtd_stat(df$income, df$wgt, 0)))
log_value("Households with income <= 0",
          fmt_count(n_distinct(df$hh_id[df$income <= 0])))
log_value("Output folder", out_path)
log_value("Log file", log_path)
log_note("External numbers are NOT in this log: the BLS dual-income share ",
         "and the '$1M more over a lifetime' college stat. Source those separately.")

# ############################  End  ################################## #