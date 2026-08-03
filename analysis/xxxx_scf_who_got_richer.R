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
# CHANGE THESE THREE LINES WHEN THE 2025 DATA LANDS.
# Everything downstream (titles, notes, filenames, output folder, tables)
# keys off them.

latest_year <- 2022   # -> 2025
prior_year  <- 2019   # -> 2022

# Percentiles used in the change-by-percentile charts/tables
change_probs <- c(0.10, 0.20, 0.25, 0.30, 0.40, 0.50,
                  0.60, 0.70, 0.75, 0.80, 0.90, 0.95, 0.99)

# Age groups that get their own standalone time-series chart
agecl_focus <- c("<35", "35-44")

# Participation above this in BOTH years means "basically everyone has it",
# so the component is dropped from the participation charts.
universal_cutoff <- 0.98

# A participation shift bigger than this makes median-among-owners
# non-comparable across years (composition effect). Flagged on the chart.
composition_cutoff <- 0.015

########################## Output paths ############################### #
# Charts land in a year-stamped subfolder, so the 2022 dry run and the 2025
# run sit side by side instead of overwriting each other.

folder_name <- "xxxx_scf_who_got_richer"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", latest_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# Component variables for the decomposition. We only keep the ones that
# actually exist in the stack, so you can add variables to the build later
# without touching any of the logic below.
component_vars_all <- c("fin", "nfin", "homeeq", "vehic", "liq", "retqliq",
                        "stocks", "nmmf", "bus", "asset", "debt",
                        "ccbal", "install", "resdbt", "edn_inst", "income")

component_vars <- intersect(component_vars_all, names(scf_stack))

message("Components found in stack: ", paste(component_vars, collapse = ", "))

missing_components <- setdiff(component_vars_all, names(scf_stack))
if(length(missing_components) > 0){
  message("Components NOT in stack (skipped): ",
          paste(missing_components, collapse = ", "))
}

# Split assets from liabilities so we never chart "share of households with
# any holdings" across a mix of the two.
debt_components  <- intersect(c("debt", "ccbal", "install", "resdbt",
                                "edn_inst"), component_vars)
asset_components <- setdiff(component_vars, c(debt_components, "income"))

keep_vars <- unique(c("year", "hh_id", "imp_id", "agecl", "edcl", "age",
                      "networth", "wgt", component_vars))

df <- scf_stack %>%
  select(all_of(intersect(keep_vars, names(scf_stack)))) %>%
  arrange(year, hh_id, imp_id)

year_min <- min(df$year)
year_max <- max(df$year)

stopifnot(latest_year %in% df$year)
stopifnot(prior_year %in% df$year)

source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
note_string   <- paste0("Note: All figures are adjusted for inflation (",
                        latest_year, " dollars).")

########################## Helper Functions ########################### #

# Scale-robust dollar labels. The format decision is made ONCE per vector
# (based on the max), so you never get mixed $k and $M labels side by side.
# On FACETED charts, call this grouped by facet - otherwise a $3M facet
# forces "$0.02M" labels onto a facet whose values are all in the tens of
# thousands.
make_dollar_labels <- function(values){
  max_abs <- max(abs(values), na.rm = TRUE)
  sign_prefix <- ifelse(values < 0, "-$", "$")
  
  if(max_abs >= 10^6){
    paste0(sign_prefix, formatC(abs(values)/10^6, big.mark = ",",
                                format = "f", digits = 2), "M")
  } else if(max_abs >= 10^3){
    paste0(sign_prefix, formatC(abs(values)/10^3, big.mark = ",",
                                format = "f", digits = 0), "k")
  } else {
    paste0(sign_prefix, formatC(abs(values), big.mark = ",",
                                format = "f", digits = 0))
  }
}

make_pct_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(ifelse(values > 0, "+", ""),
                formatC(100 * values, format = "f", digits = digits), "%"))
}

# Wraps a long main title so it doesn't run off a 15cm chart, then appends
# the subtitle line underneath.
make_title <- function(main, sub, width = 38){
  paste0(str_wrap(main, width = width), "\n", sub)
}

# Weighted stat for a single variable at a single prob (prob = 0 -> mean)
wtd_stat <- function(x, w, quantile_prob){
  if(quantile_prob == 0){
    as.numeric(wtd.mean(x, weights = w))
  } else {
    as.numeric(wtd.quantile(x, weights = w, probs = quantile_prob))
  }
}

# Grouped weighted summary returning one row per group
summarise_by <- function(data, var, group_vars, quantile_prob){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(value = wtd_stat(.data[[var]], wgt, quantile_prob),
              .groups = "drop")
}

# Multiple percentiles for one variable, one row per (group x prob).
wtd_pctile_tbl <- function(data, var, probs){
  tibble(
    prob  = probs,
    value = as.numeric(wtd.quantile(data[[var]], weights = data$wgt, probs = probs))
  )
}

summarise_pctiles_by <- function(data, var, group_vars, probs){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~ wtd_pctile_tbl(.x, var, probs)) %>%
    ungroup()
}

prob_label <- function(prob){
  ifelse(prob == 0.5, "50th (Median)",
         paste0(formatC(100 * prob, format = "f", digits = 0), "th"))
}

quantile_prob_string <- function(quantile_prob){
  str_pad(100 * quantile_prob, side = "left", width = 3, pad = "0")
}

stat_title <- function(quantile_prob){
  if(quantile_prob == 0){
    "Average"
  } else if(quantile_prob == 0.5){
    "Median"
  } else {
    paste0(formatC(100 * quantile_prob, format = "f", digits = 0),
           "th Percentile")
  }
}

save_chart <- function(plot, file_path){
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Percent change, guarded against a non-positive base. A 10th-percentile net
# worth of $0 or -$5,000 in the prior year makes percent change meaningless,
# so those come back NA and get labeled "n/a" rather than silently plotted.
safe_pct_change <- function(new_value, old_value){
  ifelse(old_value > 0, (new_value / old_value) - 1, NA_real_)
}

# Weighted within-year wealth group. Note this pools all implicates when
# ranking, which is fine for descriptive cuts but is not a formal MI estimate.
add_wealth_group <- function(data){
  data %>%
    group_by(year) %>%
    arrange(networth, .by_group = TRUE) %>%
    mutate(cum_wgt = cumsum(wgt) / sum(wgt),
           wealth_group = case_when(
             cum_wgt <= 0.25 ~ "Bottom 25%",
             cum_wgt <= 0.50 ~ "25th-50th",
             cum_wgt <= 0.75 ~ "50th-75th",
             cum_wgt <= 0.90 ~ "75th-90th",
             cum_wgt <= 0.99 ~ "90th-99th",
             TRUE            ~ "Top 1%")) %>%
    ungroup() %>%
    mutate(wealth_group = factor(wealth_group,
                                 levels = c("Bottom 25%", "25th-50th",
                                            "50th-75th", "75th-90th",
                                            "90th-99th", "Top 1%")))
}

df_two_year <- df %>% filter(year %in% c(prior_year, latest_year))
df_wealth   <- add_wealth_group(df_two_year)

period_factor <- function(year_vector){
  factor(as.character(year_vector),
         levels = c(as.character(prior_year), as.character(latest_year)))
}

# ##################################################################### #
# SECTION 1: Long time series (adapted from 0369)
# ##################################################################### #

create_time_series_chart <- function(var, var_title, quantile_prob){
  
  qps <- quantile_prob_string(quantile_prob)
  
  # ---- Overall ----
  to_plot <- summarise_by(df, var, "year", quantile_prob)
  
  file_path <- paste0(out_path, "/01_", var, "_", qps, "_by_year.jpeg")
  
  plot <- ggplot(to_plot, aes(x = year, y = value)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(breaks = seq(year_min, year_max, 3),
                       limits = c(year_min, year_max)) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(make_title(var_title, "by Year")) +
    labs(x = "Year", y = paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ---- By age, then by education ----
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
    
    to_plot <- summarise_by(df, var, c("year", group_var), quantile_prob)
    
    file_path <- paste0(out_path, "/01_", var, "_", qps,
                        "_by_year_", end_filename, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      facet_rep_wrap(as.formula(paste0(group_var, " ~ .")),
                     repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(make_title(var_title, paste0("by Year & ", x_var))) +
      labs(x = "Year", y = paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
  }
  
  # ---- Standalone chart for each focus age group ----
  # (In the original this branch silently hardcoded "<35" on the mean path,
  #  so every average chart for 35-44 was actually plotting under-35 data.)
  for(agecl_filter in agecl_focus){
    
    agecl_name <- str_replace_all(str_replace_all(agecl_filter, "<", "under_"),
                                  "-", "_to_")
    
    to_plot <- df %>%
      filter(agecl == agecl_filter) %>%
      summarise_by(var, "year", quantile_prob)
    
    file_path <- paste0(out_path, "/01_", var, "_", qps, "_",
                        agecl_name, "_by_year.jpeg")
    
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      scale_y_continuous(label = dollar) +
      scale_x_continuous(breaks = seq(year_min, year_max, 3),
                         limits = c(year_min, year_max)) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(make_title(paste0(var_title, " by Year"),
                         paste0("For Households ", agecl_filter))) +
      labs(x = "Year", y = paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
  }
}

create_time_series_chart("networth", "25th Percentile Real Net Worth", 0.25)
create_time_series_chart("networth", "Real Median Net Worth", 0.5)
create_time_series_chart("networth", "75th Percentile Real Net Worth", 0.75)
create_time_series_chart("networth", "90th Percentile Real Net Worth", 0.9)
create_time_series_chart("networth", "Real Average Net Worth", 0)

# ##################################################################### #
# SECTION 2: The headline chart - change by percentile
# ##################################################################### #

create_change_by_percentile <- function(var, var_title){
  
  pctiles <- summarise_pctiles_by(df_two_year, var, "year", change_probs) %>%
    mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
    select(prob, year_label, value) %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    mutate(dollar_change = latest - prior,
           pct_change    = safe_pct_change(latest, prior),
           prob_label    = factor(prob_label(prob),
                                  levels = prob_label(sort(change_probs))))
  
  # ---- Chart 2a: levels, side by side ----
  to_plot <- pctiles %>%
    select(prob_label, prior, latest) %>%
    pivot_longer(cols = c(prior, latest),
                 names_to = "period", values_to = "value") %>%
    mutate(period = period_factor(ifelse(period == "prior",
                                         prior_year, latest_year)))
  
  file_path <- paste0(out_path, "/02_", var, "_levels_by_percentile.jpeg")
  
  plot <- ggplot(to_plot, aes(x = prob_label, y = value, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(label = dollar) +
    scale_fill_grey(start = 0.6, end = 0.2) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(make_title(paste0("Real ", var_title, " by Percentile"),
                       paste0(prior_year, " vs. ", latest_year))) +
    labs(x = "Percentile", y = paste0("Real ", var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ---- Chart 2b: percent change (this is the money chart) ----
  to_plot <- pctiles %>% filter(!is.na(pct_change))
  
  text_labels <- to_plot %>%
    mutate(label = make_pct_labels(pct_change))
  
  file_path <- paste0(out_path, "/02_", var, "_pct_change_by_percentile.jpeg")
  
  plot <- ggplot(to_plot, aes(x = prob_label, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = prob_label, y = pct_change, label = label),
              col = chart_standard_color,
              vjust = ifelse(text_labels$pct_change > 0, -0.5, 1.5),
              size = 1.8) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(make_title(paste0("Change in Real ", var_title, " by Percentile"),
                       paste0(prior_year, "-", latest_year))) +
    labs(x = "Percentile", y = paste0("Change in Real ", var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ---- Chart 2c: dollar change ----
  text_labels <- pctiles %>%
    mutate(label = make_dollar_labels(dollar_change))
  
  file_path <- paste0(out_path, "/02_", var, "_dollar_change_by_percentile.jpeg")
  
  plot <- ggplot(pctiles, aes(x = prob_label, y = dollar_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels,
              aes(x = prob_label, y = dollar_change, label = label),
              col = chart_standard_color,
              vjust = ifelse(text_labels$dollar_change > 0, -0.5, 1.5),
              size = 1.8) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(make_title(paste0("Dollar Change in Real ", var_title,
                              " by Percentile"),
                       paste0(prior_year, "-", latest_year))) +
    labs(x = "Percentile", y = paste0("Change in Real ", var_title),
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
  
  # ---- Table ----
  table_out <- pctiles %>%
    arrange(prob) %>%
    transmute(
      Percentile = as.character(prob_label),
      `Prior`    = format_as_dollar(prior),
      `Latest`   = format_as_dollar(latest),
      `$ Change` = format_as_dollar(dollar_change),
      `% Change` = make_pct_labels(pct_change)
    )
  
  names(table_out)[2] <- as.character(prior_year)
  names(table_out)[3] <- as.character(latest_year)
  
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        file = paste0(out_path, "/02_", var, "_change_by_percentile_table.html"))
  
  assign(paste0("pctiles_", var), pctiles, envir = .GlobalEnv)
}

create_change_by_percentile("networth", "Net Worth")
create_change_by_percentile("income", "Income")

# ##################################################################### #
# SECTION 3: Change by age and education
# ##################################################################### #

create_change_by_group <- function(var, var_title, quantile_prob){
  
  qps <- quantile_prob_string(quantile_prob)
  stat_name <- stat_title(quantile_prob)
  
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
    
    grouped <- summarise_by(df_two_year, var, c("year", group_var),
                            quantile_prob) %>%
      mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
      select(all_of(group_var), year_label, value) %>%
      pivot_wider(names_from = year_label, values_from = value) %>%
      mutate(dollar_change = latest - prior,
             pct_change    = safe_pct_change(latest, prior))
    
    # ---- Levels, side by side ----
    to_plot <- grouped %>%
      select(all_of(group_var), prior, latest) %>%
      pivot_longer(cols = c(prior, latest),
                   names_to = "period", values_to = "value") %>%
      mutate(period = period_factor(ifelse(period == "prior",
                                           prior_year, latest_year)))
    
    file_path <- paste0(out_path, "/03_", var, "_", qps,
                        "_levels_by_", end_filename, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = .data[[group_var]], y = value,
                                fill = period)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(label = dollar) +
      scale_fill_grey(start = 0.6, end = 0.2) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_blank(),
            legend.position = "bottom") +
      ggtitle(make_title(paste0("Real ", stat_name, " ", var_title,
                                " by ", x_var),
                         paste0(prior_year, " vs. ", latest_year))) +
      labs(x = x_var, y = paste0("Real ", var_title),
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
    
    # ---- Percent change ----
    to_plot <- grouped %>% filter(!is.na(pct_change))
    
    text_labels <- to_plot %>%
      mutate(label = make_pct_labels(pct_change))
    
    file_path <- paste0(out_path, "/03_", var, "_", qps,
                        "_pct_change_by_", end_filename, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x = .data[[group_var]], y = pct_change)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      geom_text(data = text_labels,
                aes(x = .data[[group_var]], y = pct_change, label = label),
                col = chart_standard_color,
                vjust = ifelse(text_labels$pct_change > 0, -0.5, 1.5),
                size = 1.8) +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(make_title(paste0("Change in Real ", stat_name, " ", var_title,
                                " by ", x_var),
                         paste0(prior_year, "-", latest_year))) +
      labs(x = x_var, y = paste0("Change in Real ", var_title),
           caption = paste0(source_string, "\n", note_string))
    
    save_chart(plot, file_path)
    
    # ---- Table ----
    table_out <- grouped %>%
      transmute(
        Group      = as.character(.data[[group_var]]),
        `Prior`    = format_as_dollar(prior),
        `Latest`   = format_as_dollar(latest),
        `$ Change` = format_as_dollar(dollar_change),
        `% Change` = make_pct_labels(pct_change)
      )
    
    names(table_out)[1] <- x_var
    names(table_out)[2] <- as.character(prior_year)
    names(table_out)[3] <- as.character(latest_year)
    
    print(xtable(table_out),
          include.rownames = FALSE,
          type = "html",
          file = paste0(out_path, "/03_", var, "_", qps,
                        "_change_by_", end_filename, "_table.html"))
  }
}

create_change_by_group("networth", "Net Worth", 0.5)
create_change_by_group("networth", "Net Worth", 0.9)
create_change_by_group("networth", "Net Worth", 0)
create_change_by_group("income", "Income", 0.5)

# ##################################################################### #
# SECTION 4: Component decomposition - what actually moved
# ##################################################################### #

df_long <- df_two_year %>%
  select(year, wgt, all_of(component_vars)) %>%
  pivot_longer(cols = all_of(component_vars),
               names_to = "component", values_to = "value")

component_summary <- df_long %>%
  group_by(year, component) %>%
  summarise(
    participation = as.numeric(wtd.mean(as.numeric(value > 0), weights = wgt)),
    median_all    = as.numeric(wtd.quantile(value, weights = wgt, probs = 0.5)),
    mean_all      = as.numeric(wtd.mean(value, weights = wgt)),
    median_owners = if(sum(wgt[value > 0]) > 0){
      as.numeric(wtd.quantile(value[value > 0],
                              weights = wgt[value > 0], probs = 0.5))
    } else {
      NA_real_
    },
    .groups = "drop"
  )

component_change <- component_summary %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(-year) %>%
  pivot_longer(cols = c(participation, median_all, mean_all, median_owners),
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = year_label, values_from = value) %>%
  mutate(dollar_change = latest - prior,
         pct_change    = safe_pct_change(latest, prior))

# Components where basically every household has a positive value in both
# years tell us nothing on a participation chart, so drop them.
varying_components <- component_summary %>%
  group_by(component) %>%
  summarise(min_participation = min(participation), .groups = "drop") %>%
  filter(min_participation < universal_cutoff) %>%
  pull(component)

# ---- Chart 4a: participation, assets and debts charted separately ----
make_participation_chart <- function(comp_subset, chart_label, file_suffix){
  
  comp_subset <- intersect(comp_subset, varying_components)
  
  if(length(comp_subset) == 0){
    return(invisible(NULL))
  }
  
  to_plot <- component_summary %>%
    filter(component %in% comp_subset) %>%
    mutate(period = period_factor(year))
  
  file_path <- paste0(out_path, "/04_participation_", file_suffix, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = reorder(component, participation),
                              y = participation, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_fill_grey(start = 0.6, end = 0.2) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(make_title(chart_label,
                       paste0(prior_year, " vs. ", latest_year))) +
    labs(x = "Component", y = "Share of Households",
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
}

make_participation_chart(asset_components,
                         "Share of Households Owning Each Asset",
                         "assets")

make_participation_chart(debt_components,
                         "Share of Households Holding Each Type of Debt",
                         "debts")

# ---- Chart 4b: percent change in median value among owners ----
# IMPORTANT: when participation shifts, median-among-owners is NOT
# comparable across years. If a wave of new small holders enters, the
# median among owners falls even though nobody's holdings shrank. Those
# components get an asterisk so the chart can't be misread.
participation_shift <- component_change %>%
  filter(metric == "participation") %>%
  select(component, part_prior = prior, part_latest = latest) %>%
  mutate(part_change = part_latest - part_prior)

to_plot <- component_change %>%
  filter(metric == "median_owners", !is.na(pct_change)) %>%
  left_join(participation_shift, by = "component") %>%
  mutate(composition_flag = abs(part_change) > composition_cutoff,
         component_label  = ifelse(composition_flag,
                                   paste0(component, " *"), component))

composition_note <- paste0("* Participation shifted more than ",
                           formatC(100 * composition_cutoff, format = "f",
                                   digits = 1),
                           "pp; medians among owners are not comparable.")

file_path <- paste0(out_path, "/04_component_pct_change_owners.jpeg")

plot <- ggplot(to_plot, aes(x = reorder(component_label, pct_change),
                            y = pct_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  coord_flip() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(make_title("Change in Real Median Holdings (Among Owners)",
                     paste0(prior_year, "-", latest_year))) +
  labs(x = "Component", y = "Change in Real Median Value",
       caption = paste0(source_string, "\n", note_string, "\n",
                        composition_note))

save_chart(plot, file_path)

# ---- Chart 4c: the composition effect itself ----
# Read this next to 4b. A component that gained holders will usually show a
# falling median among owners for that reason alone.
to_plot <- participation_shift %>%
  filter(component %in% varying_components)

text_labels <- to_plot %>%
  mutate(label = paste0(ifelse(part_change > 0, "+", ""),
                        formatC(100 * part_change, format = "f", digits = 1),
                        "pp"))

file_path <- paste0(out_path, "/04_component_participation_change.jpeg")

plot <- ggplot(to_plot, aes(x = reorder(component, part_change),
                            y = part_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels,
            aes(x = reorder(component, part_change), y = part_change,
                label = label),
            col = chart_standard_color,
            hjust = ifelse(text_labels$part_change > 0, -0.1, 1.1),
            size = 1.8) +
  coord_flip() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(make_title("Change in Share of Households With Any Holdings",
                     paste0(prior_year, "-", latest_year))) +
  labs(x = "Component", y = "Change in Share of Households",
       caption = paste0(source_string, "\n", note_string))

save_chart(plot, file_path)

# ---- Table ----
component_table <- component_change %>%
  filter(metric %in% c("participation", "median_owners")) %>%
  mutate(display = ifelse(metric == "participation",
                          paste0(make_pct_labels(prior, 1), " -> ",
                                 make_pct_labels(latest, 1)),
                          paste0(format_as_dollar(prior), " -> ",
                                 format_as_dollar(latest)))) %>%
  select(component, metric, display, pct_change) %>%
  mutate(`% Change` = make_pct_labels(pct_change)) %>%
  select(Component = component, Metric = metric,
         `Prior -> Latest` = display, `% Change`)

print(xtable(component_table),
      include.rownames = FALSE,
      type = "html",
      file = paste0(out_path, "/04_component_change_table.html"))

# ##################################################################### #
# SECTION 5: Wealth group cuts - the mechanism
# ##################################################################### #

wealth_group_summary <- df_wealth %>%
  group_by(year, wealth_group) %>%
  summarise(
    median_networth = as.numeric(wtd.quantile(networth, weights = wgt,
                                              probs = 0.5)),
    mean_networth   = as.numeric(wtd.mean(networth, weights = wgt)),
    .groups = "drop"
  ) %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior"))

wealth_group_change <- wealth_group_summary %>%
  select(wealth_group, year_label, median_networth) %>%
  pivot_wider(names_from = year_label, values_from = median_networth) %>%
  mutate(dollar_change = latest - prior,
         pct_change    = safe_pct_change(latest, prior))

# ---- Chart 5a: percent change in median net worth by wealth group ----
to_plot <- wealth_group_change %>% filter(!is.na(pct_change))

text_labels <- to_plot %>%
  mutate(label = make_pct_labels(pct_change))

file_path <- paste0(out_path, "/05_networth_pct_change_by_wealth_group.jpeg")

plot <- ggplot(to_plot, aes(x = wealth_group, y = pct_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels,
            aes(x = wealth_group, y = pct_change, label = label),
            col = chart_standard_color,
            vjust = ifelse(text_labels$pct_change > 0, -0.5, 1.5),
            size = 1.8) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Change in Real Median Net Worth by Wealth Group",
                     paste0(prior_year, "-", latest_year))) +
  labs(x = "Wealth Group", y = "Change in Real Median Net Worth",
       caption = paste0(source_string, "\n", note_string))

save_chart(plot, file_path)

# ---- Chart 5b: participation by wealth group ----
# This is the "a bull market can't reach households that own no equities"
# chart. Deliberately excludes fin/liq - those sit near 100% in every wealth
# group and just waste panels.
participation_vars <- intersect(c("stocks", "bus", "retqliq", "homeeq"),
                                varying_components)

if(length(participation_vars) == 0){
  participation_vars <- intersect(c("fin"), component_vars)
}

if(length(participation_vars) > 0){
  
  wealth_participation <- df_wealth %>%
    select(year, wealth_group, wgt, all_of(participation_vars)) %>%
    pivot_longer(cols = all_of(participation_vars),
                 names_to = "component", values_to = "value") %>%
    group_by(year, wealth_group, component) %>%
    summarise(participation = as.numeric(wtd.mean(as.numeric(value > 0),
                                                  weights = wgt)),
              .groups = "drop") %>%
    mutate(period = period_factor(year))
  
  file_path <- paste0(out_path, "/05_participation_by_wealth_group.jpeg")
  
  plot <- ggplot(wealth_participation,
                 aes(x = wealth_group, y = participation, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_rep_wrap(component ~ ., repeat.tick.labels = c("left", "bottom")) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_fill_grey(start = 0.6, end = 0.2) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(make_title("Share of Households With Any Holdings",
                       "by Wealth Group")) +
    labs(x = "Wealth Group", y = "Share of Households",
         caption = paste0(source_string, "\n", note_string))
  
  save_chart(plot, file_path)
}

# ---- Table ----
wealth_group_table <- wealth_group_change %>%
  transmute(
    `Wealth Group` = as.character(wealth_group),
    `Prior`        = format_as_dollar(prior),
    `Latest`       = format_as_dollar(latest),
    `$ Change`     = format_as_dollar(dollar_change),
    `% Change`     = make_pct_labels(pct_change)
  )

names(wealth_group_table)[2] <- as.character(prior_year)
names(wealth_group_table)[3] <- as.character(latest_year)

print(xtable(wealth_group_table),
      include.rownames = FALSE,
      type = "html",
      file = paste0(out_path, "/05_wealth_group_change_table.html"))

# ##################################################################### #
# SECTION 6: Latest-year reference charts and tables (from 0369)
# ##################################################################### #

df_year <- df %>%
  filter(year == latest_year) %>%
  arrange(year, hh_id, imp_id)

to_plot <- summarise_pctiles_by(df_year, "networth", "agecl",
                                c(0.25, 0.50, 0.75, 0.90)) %>%
  mutate(key = factor(paste0(formatC(100 * prob, format = "f", digits = 0),
                             "th Percentile"),
                      levels = c("25th Percentile", "50th Percentile",
                                 "75th Percentile", "90th Percentile")))

# Labels are formatted PER FACET. The y-axis stays fixed across facets (so
# you can see relative differences), but a $3M facet shouldn't force the
# 25th-percentile facet to read "$0.02M".
text_labels <- to_plot %>%
  group_by(key) %>%
  mutate(label = make_dollar_labels(value)) %>%
  ungroup()

file_path <- paste0(out_path, "/06_", latest_year,
                    "_all_networth_percentiles_by_agecl.jpeg")

plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data = text_labels, aes(x = agecl, y = value, label = label),
            col = chart_standard_color,
            vjust = ifelse(text_labels$value > 0, 0, 1),
            size = 1.8) +
  facet_rep_wrap(key ~ ., repeat.tick.labels = c("left", "bottom")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(make_title("Net Worth Percentiles by Age",
                     as.character(latest_year))) +
  labs(x = "Age", y = "Net Worth",
       caption = paste0(source_string, "\n", note_string))

save_chart(plot, file_path)

# ---- Fine-grained age tables ----
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

make_age_table <- function(var){
  
  table_out <- df_year %>%
    add_agecl_new() %>%
    group_by(agecl_new) %>%
    summarise(
      avg    = format_as_dollar(as.numeric(wtd.mean(.data[[var]], weights = wgt))),
      pct_50 = format_as_dollar(as.numeric(wtd.quantile(.data[[var]],
                                                        weights = wgt,
                                                        probs = 0.5))),
      .groups = "drop"
    ) %>%
    select(agecl_new, avg, pct_50)
  
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        file = paste0(out_path, "/06_", latest_year, "_", var,
                      "_by_agecl_table.html"))
}

make_age_table("networth")

if("homeeq" %in% names(df_year)){
  make_age_table("homeeq")
}

# ##################################################################### #
# SECTION 7: Sanity checks
# ##################################################################### #
# These print to console so you can eyeball the refactor against what you
# already published. For the 2019 -> 2022 dry run the median net worth
# figures should come back at $141,145 and $192,700, a +36.5% real increase.

sanity <- pctiles_networth %>% filter(prob == 0.5)

print(paste0("Output folder: ", out_path))
print(paste0("Median net worth ", prior_year, ": ",
             format_as_dollar(sanity$prior)))
print(paste0("Median net worth ", latest_year, ": ",
             format_as_dollar(sanity$latest)))
print(paste0("Real change: ", make_pct_labels(sanity$pct_change, 1)))

print("Components flagged for composition effects (read 4b with care):")
print(participation_shift %>%
        filter(abs(part_change) > composition_cutoff) %>%
        arrange(desc(abs(part_change))))

print("Households per year (unweighted, all implicates): ")
print(df %>% count(year) %>% filter(year %in% c(prior_year, latest_year)))

# ############################  End  ################################## #