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
# CHANGE THESE WHEN THE 2025 DATA LANDS. Everything downstream - output
# subfolder, titles, source strings, notes, filenames - keys off them.

data_year   <- 2022   # -> 2025
prior_year  <- 2019   # -> 2022  (for the threshold comparison section)
dollar_year <- 2022   # -> 2025  (dollar basis of 0003_scf_stack.Rds)

# NOTE ON INCOME YEAR: the SCF asks about income for the PRIOR calendar
# year, so the 2025 survey's income figures describe 2024. Worth one
# sentence in the post rather than letting a reader catch it.

# Round-number thresholds for the "what share of households clear this?"
# section. These are in dollar_year dollars.
income_thresholds <- c(100000, 150000, 250000, 400000, 500000, 1000000)

# Percentiles for the headline "what is rich" numbers
rich_probs <- c(0.90, 0.95, 0.99)

# Two-series charts (prior year vs. latest year). The prior year is muted
# so the eye lands on the current figures; the latest year uses the same
# navy as every single-series chart on the blog.
prior_year_color  <- "#B3B3B3"

########################## Output paths ############################### #

folder_name <- "xxxx_scf_income_by_age_edc"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack_all <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

stopifnot(data_year %in% scf_stack_all$year)

df <- scf_stack_all %>%
  filter(year == data_year) %>%
  select(hh_id, imp_id, age, income, wgt, agecl, edcl) %>%
  arrange(hh_id, imp_id)

n_hh <- length(unique(df$hh_id))

source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year,
                        " (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note:  Calculations based on weighted data from ",
                               formatC(n_hh, digits = 0, format = "f",
                                       big.mark = ","),
                               " U.S. households."),
                        width = 85)

excel_path <- paste0(out_path, "/all_var_summaries.xlsx")

########################## Helper Functions ########################### #

# Dollar labels for charts. Two fixes over the original:
#   1. The $k / $M decision is made once per vector off the max, so a
#      top-1%-by-education chart renders "$2.0M" instead of "$2,000k".
#   2. Negatives get a "-$" prefix instead of letting formatC emit the
#      minus inside a hardcoded "$" (which produced "$-2k").
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

quantile_prob_string <- function(quantile_prob){
  if(quantile_prob == 0){
    "avg"
  } else {
    str_pad(100 * quantile_prob, side = "left", width = 3, pad = "0")
  }
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

save_chart <- function(plot, file_path){
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

write_html_table <- function(table_out, file_path){
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        file = file_path)
}

# Unweighted household count per cell. Used to flag thin cells - a 99th
# percentile estimated off a few dozen households is a handful of specific
# respondents, not a population figure.
cell_counts <- function(data, group_vars){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(n_hh = n_distinct(hh_id), .groups = "drop")
}

# ##################################################################### #
# SECTION 1: Charts by age, education, and the two combined
# ##################################################################### #

create_percentile_chart <- function(var, var_title, quantile_prob){
  
  qps <- quantile_prob_string(quantile_prob)
  
  overall_value <- wtd_stat(df[[var]], df$wgt, quantile_prob)
  print(paste0("Overall ", var_title, " is: ", format_as_dollar(overall_value)))
  
  # ##### 1a. Age x Education grid #####
  # NOTE: no scales = "free_y" here, deliberately. The shared axis is what
  # makes the College Degree panel visually dwarf the others, which is the
  # whole point of this chart in the post.
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
  
  text_labels <- to_plot %>%
    mutate(label = make_dollar_labels(value))
  
  file_path <- paste0(out_path, "/", var, "_", qps,
                      "_age_edc_comb_scf_", data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", position = "dodge",
             fill = chart_standard_color) +
    facet_rep_wrap(edcl ~ ., repeat.tick.labels = c("left", "bottom")) +
    geom_text(data = text_labels, aes(x = agecl, y = value, label = label),
              col = chart_standard_color,
              size = 1.8,
              vjust = ifelse(text_labels$value > 0, 0, 1)) +
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
  
  # ##### 1b. Age only, then Education only #####
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
      mutate(label = make_dollar_labels(value))
    
    file_path <- paste0(out_path, "/", var, "_", qps, "_",
                        end_filename, "_scf_", data_year, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = .data[[group_var]], y = value)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      geom_text(data = text_labels,
                aes(x = .data[[group_var]], y = value, label = label),
                col = chart_standard_color,
                vjust = ifelse(text_labels$value > 0, -0.2, 1.2),
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
  # REMOVED: the 99.9th percentile. Nationally that is roughly five
  # households in the sample - not an estimate, just a few respondents.
  # It never made the post and it is too tempting to quote once the chart
  # is sitting in the folder.
}

create_stack("income", "Income")

# ##################################################################### #
# SECTION 2: The headline top 10% / 5% / 1% numbers
# ##################################################################### #

headline <- tibble(prob = rich_probs) %>%
  mutate(value = map_dbl(prob, ~ wtd_stat(df$income, df$wgt, .x)),
         label = paste0("Top ", formatC(100 * (1 - prob), format = "f",
                                        digits = 0), "%"))

print("Headline thresholds:")
print(headline %>% transmute(label, value = format_as_dollar(value)))

write_html_table(headline %>%
                   transmute(Threshold = label,
                             `Household Income` = format_as_dollar(value)),
                 paste0(out_path, "/income_headline_thresholds_",
                        data_year, "_table.html"))

# ##################################################################### #
# SECTION 3: Fine-grained age table
# ##################################################################### #
# The published 2022 version carried a Top 1% column here. With ~4,600
# households split across twelve bands, that is ~380 households per band,
# so the 99th percentile of a band is about FOUR households. It showed:
# 30-34 at $468k jumping to $1,048k at 35-39, and 45-49 top 5% coming in
# BELOW 40-44 - impossible as population facts, ordinary as sampling noise.
#
# So the 5-year table now stops at the top 5%. The top 1% lives in the
# 6-bucket agecl chart from Section 1, where each group has ~765
# households. The count column below lets you see the thinness directly.

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
    pct_50 = as.numeric(wtd.quantile(income, weights = wgt, probs = 0.50)),
    pct_90 = as.numeric(wtd.quantile(income, weights = wgt, probs = 0.90)),
    pct_95 = as.numeric(wtd.quantile(income, weights = wgt, probs = 0.95)),
    .groups = "drop"
  ) %>%
  left_join(fine_counts, by = "agecl_new") %>%
  transmute(`Age Range` = agecl_new,
            `Median`    = format_as_dollar(pct_50),
            `Top 10%`   = format_as_dollar(pct_90),
            `Top 5%`    = format_as_dollar(pct_95),
            `Households` = formatC(n_hh, format = "d", big.mark = ","))

write_html_table(rich_table_by_age,
                 paste0(out_path, "/income_by_agecl_table_rich_",
                        data_year, ".html"))

# Diagnostic only, NOT for publication: the top 1% by 5-year band, with
# household counts, so you can see for yourself how thin it is before
# deciding whether to say anything about it.
thin_check <- df_fine %>%
  group_by(agecl_new) %>%
  summarise(pct_99 = as.numeric(wtd.quantile(income, weights = wgt,
                                             probs = 0.99)),
            .groups = "drop") %>%
  left_join(fine_counts, by = "agecl_new") %>%
  mutate(approx_hh_above = round(n_hh * 0.01, 1),
         pct_99 = format_as_dollar(pct_99))

print("DIAGNOSTIC - top 1% by 5-year band is thinly sampled, do not publish:")
print(thin_check)

# ##################################################################### #
# SECTION 4: Share of households above round-number thresholds
# ##################################################################### #
# Fits the "what makes you rich" framing better than percentiles alone -
# readers think in round numbers, not percentiles.

threshold_share <- tibble(threshold = income_thresholds) %>%
  mutate(share = map_dbl(threshold,
                         ~ as.numeric(wtd.mean(as.numeric(df$income >= .x),
                                               weights = df$wgt))),
         threshold_label = factor(format_as_dollar(threshold),
                                  levels = format_as_dollar(sort(income_thresholds))))

text_labels <- threshold_share %>%
  mutate(label = paste0(formatC(100 * share, format = "f", digits = 1), "%"))

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
                             `Share of Households` =
                               paste0(formatC(100 * share, format = "f",
                                              digits = 1), "%")),
                 paste0(out_path, "/income_share_above_thresholds_",
                        data_year, "_table.html"))

# ##################################################################### #
# SECTION 5: Did the bar for "rich" outrun inflation?
# ##################################################################### #
# The obvious reader question on an update post, and nobody else answers
# it cleanly. Does not poach from the net worth decomposition post - that
# one is wealth, this is income.

if(prior_year %in% scf_stack_all$year){
  
  df_two_year <- scf_stack_all %>%
    filter(year %in% c(prior_year, data_year)) %>%
    select(year, hh_id, imp_id, age, income, wgt, agecl, edcl)
  
  threshold_compare <- df_two_year %>%
    group_by(year) %>%
    group_modify(~ tibble(prob = rich_probs,
                          value = as.numeric(wtd.quantile(.x$income,
                                                          weights = .x$wgt,
                                                          probs = rich_probs)))) %>%
    ungroup() %>%
    mutate(year_label = ifelse(year == data_year, "latest", "prior")) %>%
    select(prob, year_label, value) %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    mutate(pct_change = ifelse(prior > 0, (latest / prior) - 1, NA_real_),
           label = factor(paste0("Top ",
                                 formatC(100 * (1 - prob), format = "f",
                                         digits = 0), "%"),
                          levels = paste0("Top ",
                                          formatC(100 * (1 - sort(rich_probs, decreasing = TRUE)),
                                                  format = "f", digits = 0), "%")))
  
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
         caption = paste0(source_string, "\n",
                          str_wrap(paste0("Note: All figures adjusted for inflation (",
                                          dollar_year, " dollars)."), width = 85)))
  
  save_chart(plot, file_path)
  
  # ---- Real percent change ----
  text_labels <- threshold_compare %>%
    mutate(pct_label = paste0(ifelse(pct_change > 0, "+", ""),
                              formatC(100 * pct_change, format = "f",
                                      digits = 1), "%"))
  
  file_path <- paste0(out_path, "/income_thresholds_pct_change_",
                      prior_year, "_", data_year, ".jpeg")
  
  plot <- ggplot(threshold_compare, aes(x = label, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = label, y = pct_change, label = pct_label),
              col = chart_standard_color,
              vjust = ifelse(text_labels$pct_change > 0, -0.3, 1.3),
              size = 3) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Real Change in the Income Needed to Be Rich\n",
                   prior_year, "-", data_year)) +
    labs(x = "Threshold", y = "Real Change",
         caption = paste0(source_string, "\n",
                          str_wrap(paste0("Note: All figures adjusted for inflation (",
                                          dollar_year, " dollars)."), width = 85)))
  
  save_chart(plot, file_path)
  
  # ---- Table ----
  compare_table <- threshold_compare %>%
    arrange(desc(prob)) %>%
    transmute(Threshold = as.character(label),
              Prior     = format_as_dollar(prior),
              Latest    = format_as_dollar(latest),
              `Real Change` = paste0(ifelse(pct_change > 0, "+", ""),
                                     formatC(100 * pct_change, format = "f",
                                             digits = 1), "%"))
  
  names(compare_table)[2] <- as.character(prior_year)
  names(compare_table)[3] <- as.character(data_year)
  
  write_html_table(compare_table,
                   paste0(out_path, "/income_thresholds_compare_table.html"))
  
  print("Threshold comparison (real):")
  print(compare_table)
  
} else {
  message("prior_year ", prior_year, " not in the stack - skipping Section 5.")
}

# ##################################################################### #
# SECTION 6: Does income still rise with age only among college grads?
# ##################################################################### #
# This is the most surprising claim in the original post, and the one most
# likely to have changed. Printed as a check BEFORE you write, not as a
# chart - if it flipped, that is a more interesting post than the update.

age_edc_check <- summarise_by(df, "income", c("edcl", "agecl"), 0.90) %>%
  group_by(edcl) %>%
  arrange(agecl, .by_group = TRUE) %>%
  summarise(
    youngest_agecl = as.character(first(agecl)),
    youngest_value = first(value),
    peak_agecl     = as.character(agecl[which.max(value)]),
    peak_value     = max(value),
    # "Rises with age" = the peak is NOT in the youngest bucket AND the
    # peak is at least 25% above the youngest bucket. A peak one bucket
    # in with a trivial gap is noise, not a pattern.
    rises_with_age = (which.max(value) > 1) &
      (max(value) / first(value) > 1.25),
    .groups = "drop") %>%
  transmute(edcl,
            `Youngest`   = format_as_dollar(youngest_value),
            `Peak Age`   = peak_agecl,
            `Peak Value` = format_as_dollar(peak_value),
            `Peak / Youngest` = paste0(formatC(peak_value / youngest_value,
                                               format = "f", digits = 2), "x"),
            `Rises With Age` = rises_with_age)

print("Does top 10% income rise with age WITHIN each education level?")
print("(In 2022 this was true only for College Degree - check if it still holds.)")
print(as.data.frame(age_edc_check))

# ##################################################################### #
# Sanity checks
# ##################################################################### #
# For the 2022 run these should print $248,610 / $390,209 / $1,199,812 -
# the figures published in post 331.

print(paste0("Output folder: ", out_path))
print(paste0("Unweighted households: ",
             formatC(n_hh, digits = 0, format = "f", big.mark = ",")))
print(paste0("Median household income ", data_year, ": ",
             format_as_dollar(wtd_stat(df$income, df$wgt, 0.5))))

# ############################  End  ################################## #