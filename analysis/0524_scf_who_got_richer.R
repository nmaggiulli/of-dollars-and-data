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

latest_year <- 2022
prior_year  <- 2019

# Percentiles used in the change-by-percentile chart/table. Capped at the
# 90th on purpose: everything above the 90th percentile is reserved for the
# whitepaper.
change_probs <- c(0.10, 0.20, 0.25, 0.30, 0.40, 0.50,
                  0.60, 0.70, 0.75, 0.80, 0.90)

# Age groups that get their own standalone time-series chart
agecl_focus <- c("<35")

# The two ends of the age story, compared side by side in Section 6
young_old <- c("<35", "75+")

# Participation above this in BOTH years means "basically everyone has it",
# so the component is dropped from the participation chart.
universal_cutoff <- 0.98

# A participation shift bigger than this makes median-among-owners
# non-comparable across years (composition effect). Flagged on the chart.
composition_cutoff <- 0.015

# Two-series charts (prior year vs. latest year). The prior year is muted
# so the eye lands on the current figures.
prior_year_color <- "#B3B3B3"

period_fill_scale <- function(){
  scale_fill_manual(values = setNames(c(prior_year_color, chart_standard_color),
                                      c(as.character(prior_year),
                                        as.character(latest_year))))
}

# Readable names for SCF variables on charts
component_names <- c(
  fin     = "Financial assets",
  nfin    = "Nonfinancial assets",
  homeeq  = "Home equity",
  houses  = "Primary home",
  vehic   = "Vehicles",
  liq     = "Bank accounts",
  retqliq = "Retirement accounts",
  stocks  = "Stocks (direct)",
  nmmf    = "Mutual funds",
  bus     = "Business",
  asset   = "Total assets",
  debt    = "Total debt",
  ccbal   = "Credit card debt",
  install = "Installment loans",
  resdbt  = "Other property debt",
  edn_inst = "Student loans"
)

label_size <- 2.4

########################## Output paths ############################### #
# Charts land in a year-stamped subfolder, so runs sit side by side instead
# of overwriting each other.

folder_name <- "0524_scf_who_got_richer"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", latest_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Console log ################################ #
# Everything printed to the console also goes to a text file in out_path:
#   - print()/cat() output via sink(split = TRUE), so it still shows in
#     the console
#   - message(), warning() and error text via global calling handlers
# If a previous run died mid-script its sink is still open - close it first
# so this run's log starts clean. Needs R 4.0+.

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
    "latest_year = ", latest_year, ", prior_year = ", prior_year, "\n\n",
    sep = "")

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

# Component variables for the decomposition. Only the ones that exist in
# the stack are used.
component_vars_all <- c("fin", "nfin", "homeeq", "vehic", "liq", "retqliq",
                        "stocks", "nmmf", "bus", "asset", "debt",
                        "ccbal", "install", "resdbt", "edn_inst")

component_vars <- intersect(component_vars_all, names(scf_stack))

message("Components found in stack: ", paste(component_vars, collapse = ", "))

missing_components <- setdiff(component_vars_all, names(scf_stack))
if(length(missing_components) > 0){
  message("Components NOT in stack (skipped): ",
          paste(missing_components, collapse = ", "))
}

debt_components <- intersect(c("debt", "ccbal", "install", "resdbt",
                               "edn_inst"), component_vars)

keep_vars <- unique(c("year", "hh_id", "imp_id", "agecl", "age",
                      "networth", "income", "houses", "wgt", component_vars))

df <- scf_stack %>%
  select(all_of(intersect(keep_vars, names(scf_stack)))) %>%
  arrange(year, hh_id, imp_id)

year_min <- min(df$year)
year_max <- max(df$year)

# Step back from year_max so the latest wave always gets a tick
year_breaks <- sort(seq(year_max, year_min, by = -3))

stopifnot(latest_year %in% df$year)
stopifnot(prior_year %in% df$year)

source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
note_string   <- paste0("Note: All figures are adjusted for inflation (",
                        latest_year, " dollars).")

make_caption <- function(extra = NULL){
  paste(c(source_string, note_string, extra), collapse = "\n")
}

########################## Helper Functions ########################### #

# Dollar labels: one $k / $M decision per vector, "-$" for negatives.
# On faceted charts, call this grouped by facet.
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

make_share_labels <- function(values, digits = 0){
  ifelse(is.na(values), "n/a",
         paste0(formatC(100 * values, format = "f", digits = digits), "%"))
}

pretty_component <- function(x){
  ifelse(x %in% names(component_names), component_names[x], x)
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

summarise_by <- function(data, var, group_vars, quantile_prob){
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(value = wtd_stat(.data[[var]], wgt, quantile_prob),
              .groups = "drop")
}

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

write_html_table <- function(table_out, file_path){
  print(xtable(table_out),
        include.rownames = FALSE,
        type = "html",
        file = file_path)
}

# Percent change, guarded against a non-positive base
safe_pct_change <- function(new_value, old_value){
  ifelse(old_value > 0, (new_value / old_value) - 1, NA_real_)
}

period_factor <- function(year_vector){
  factor(as.character(year_vector),
         levels = c(as.character(prior_year), as.character(latest_year)))
}

# Weighted within-year wealth group. The top is ONE "Top 10%" group on
# purpose: anything above the 90th percentile is reserved for the
# whitepaper.
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
             TRUE            ~ "Top 10%")) %>%
    ungroup() %>%
    mutate(wealth_group = factor(wealth_group,
                                 levels = c("Bottom 25%", "25th-50th",
                                            "50th-75th", "75th-90th",
                                            "Top 10%")))
}

df_two_year <- df %>% filter(year %in% c(prior_year, latest_year))
df_wealth   <- add_wealth_group(df_two_year)

# ##################################################################### #
# SECTION 1: Long time series
# ##################################################################### #
# Cut back to what the post uses: the median (overall and by age), the
# 25th percentile (overall), and under-35 standalones for the median and
# the average. Education moved to the net worth by age post.

create_time_series_chart <- function(var, var_title, quantile_prob,
                                     overall = TRUE, by_age = FALSE,
                                     focus = character(0)){
  
  qps <- quantile_prob_string(quantile_prob)
  
  # ---- Overall ----
  if(overall){
    to_plot <- summarise_by(df, var, "year", quantile_prob)
    
    print(paste0(var_title, " by year:"))
    print(to_plot %>%
            filter(year >= prior_year - 3) %>%
            mutate(value = format_as_dollar(value)) %>%
            as.data.frame())
    
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      scale_y_continuous(label = dollar) +
      scale_x_continuous(breaks = year_breaks,
                         limits = c(year_min, year_max)) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0(var_title, "\nby Year")) +
      labs(x = "Year", y = var_title,
           caption = make_caption())
    
    save_chart(plot, paste0(out_path, "/01_", var, "_", qps, "_by_year.jpeg"))
  }
  
  # ---- By age ----
  if(by_age){
    to_plot <- summarise_by(df, var, c("year", "agecl"), quantile_prob)
    
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      facet_wrap(vars(agecl), axes = "all") +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0(var_title, "\nby Year & Age")) +
      labs(x = "Year", y = var_title,
           caption = make_caption())
    
    save_chart(plot, paste0(out_path, "/01_", var, "_", qps, "_by_year_age.jpeg"))
  }
  
  # ---- Standalone chart for each focus age group ----
  for(agecl_filter in focus){
    
    agecl_name <- str_replace_all(str_replace_all(agecl_filter, "<", "under_"),
                                  "-", "_to_")
    
    to_plot <- df %>%
      filter(agecl == agecl_filter) %>%
      summarise_by(var, "year", quantile_prob)
    
    print(paste0(var_title, ", households ", agecl_filter, ":"))
    print(to_plot %>%
            filter(year >= prior_year - 3) %>%
            mutate(value = format_as_dollar(value)) %>%
            as.data.frame())
    
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      scale_y_continuous(label = dollar) +
      scale_x_continuous(breaks = year_breaks,
                         limits = c(year_min, year_max)) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0(var_title, "\nHouseholds Under 35")) +
      labs(x = "Year", y = var_title,
           caption = make_caption())
    
    save_chart(plot, paste0(out_path, "/01_", var, "_", qps, "_",
                            agecl_name, "_by_year.jpeg"))
  }
}

create_time_series_chart("networth", "Real Median Net Worth", 0.5,
                         by_age = TRUE, focus = agecl_focus)
create_time_series_chart("networth", "25th Percentile Net Worth", 0.25)
create_time_series_chart("networth", "Real Average Net Worth", 0,
                         overall = FALSE, focus = agecl_focus)

# ##################################################################### #
# SECTION 2: The headline chart - change by percentile (10th-90th)
# ##################################################################### #

pctiles <- summarise_pctiles_by(df_two_year, "networth", "year", change_probs) %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(prob, year_label, value) %>%
  pivot_wider(names_from = year_label, values_from = value) %>%
  mutate(dollar_change = latest - prior,
         pct_change    = safe_pct_change(latest, prior),
         prob_label    = factor(prob_label(prob),
                                levels = prob_label(sort(change_probs))))

print("Net worth by percentile:")
print(pctiles %>%
        transmute(percentile = prob_label,
                  prior = format_as_dollar(prior),
                  latest = format_as_dollar(latest),
                  pct_change = make_pct_labels(pct_change, 1)) %>%
        as.data.frame())

to_plot <- pctiles %>%
  filter(!is.na(pct_change)) %>%
  mutate(label = make_pct_labels(pct_change),
         vj    = ifelse(pct_change > 0, -0.5, 1.5))

plot <- ggplot(to_plot, aes(x = prob_label, y = pct_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label, vjust = vj),
            col = chart_standard_color, size = label_size) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.10, 0.10))) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Net Worth Change by Percentile\n",
                 prior_year, "-", latest_year)) +
  labs(x = "Percentile", y = "Change in Real Net Worth",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/02_networth_pct_change_by_percentile.jpeg"))

table_out <- pctiles %>%
  arrange(prob) %>%
  transmute(Percentile = as.character(prob_label),
            Prior      = format_as_dollar(prior),
            Latest     = format_as_dollar(latest),
            `$ Change` = format_as_dollar(dollar_change),
            `% Change` = make_pct_labels(pct_change))

names(table_out)[2] <- as.character(prior_year)
names(table_out)[3] <- as.character(latest_year)

write_html_table(table_out,
                 paste0(out_path, "/02_networth_change_by_percentile_table.html"))

# ##################################################################### #
# SECTION 3: Change by age
# ##################################################################### #
# Median (levels + change) and 90th percentile (change only). The 90th
# WITHIN an age group is fine for the blog; it is not the top 10% overall.

create_change_by_age <- function(var, var_title, quantile_prob,
                                 show_levels = TRUE){
  
  qps <- quantile_prob_string(quantile_prob)
  stat_name <- stat_title(quantile_prob)
  
  grouped <- summarise_by(df_two_year, var, c("year", "agecl"), quantile_prob) %>%
    mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
    select(agecl, year_label, value) %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    mutate(dollar_change = latest - prior,
           pct_change    = safe_pct_change(latest, prior))
  
  print(paste0(stat_name, " ", var_title, " by age:"))
  print(grouped %>%
          transmute(agecl,
                    prior = format_as_dollar(prior),
                    latest = format_as_dollar(latest),
                    pct_change = make_pct_labels(pct_change, 1)) %>%
          as.data.frame())
  
  # ---- Levels, side by side ----
  if(show_levels){
    to_plot <- grouped %>%
      select(agecl, prior, latest) %>%
      pivot_longer(cols = c(prior, latest),
                   names_to = "period", values_to = "value") %>%
      mutate(period = period_factor(ifelse(period == "prior",
                                           prior_year, latest_year)))
    
    plot <- ggplot(to_plot, aes(x = agecl, y = value, fill = period)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(label = dollar) +
      period_fill_scale() +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_blank(),
            legend.position = "bottom") +
      ggtitle(paste0(stat_name, " ", var_title, " by Age\n",
                     prior_year, " vs. ", latest_year)) +
      labs(x = "Age", y = paste0("Real ", var_title),
           caption = make_caption())
    
    save_chart(plot, paste0(out_path, "/03_", var, "_", qps, "_levels_by_age.jpeg"))
  }
  
  # ---- Percent change ----
  to_plot <- grouped %>%
    filter(!is.na(pct_change)) %>%
    mutate(label = make_pct_labels(pct_change),
           vj    = ifelse(pct_change > 0, -0.5, 1.5))
  
  plot <- ggplot(to_plot, aes(x = agecl, y = pct_change)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(aes(label = label, vjust = vj),
              col = chart_standard_color, size = label_size) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.10, 0.10))) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Change in ", var_title, " by Age\n",
                   stat_name, ", ", prior_year, "-", latest_year)) +
    labs(x = "Age", y = paste0("Change in Real ", var_title),
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/03_", var, "_", qps, "_pct_change_by_age.jpeg"))
  
  # ---- Table ----
  table_out <- grouped %>%
    transmute(Age        = as.character(agecl),
              Prior      = format_as_dollar(prior),
              Latest     = format_as_dollar(latest),
              `$ Change` = format_as_dollar(dollar_change),
              `% Change` = make_pct_labels(pct_change))
  
  names(table_out)[2] <- as.character(prior_year)
  names(table_out)[3] <- as.character(latest_year)
  
  write_html_table(table_out,
                   paste0(out_path, "/03_", var, "_", qps,
                          "_change_by_age_table.html"))
}

create_change_by_age("networth", "Net Worth", 0.5)
create_change_by_age("networth", "Net Worth", 0.9, show_levels = FALSE)

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
  pivot_longer(cols = c(participation, median_owners),
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = year_label, values_from = value) %>%
  mutate(pct_change = safe_pct_change(latest, prior))

varying_components <- component_summary %>%
  group_by(component) %>%
  summarise(min_participation = min(participation), .groups = "drop") %>%
  filter(min_participation < universal_cutoff) %>%
  pull(component)

participation_shift <- component_change %>%
  filter(metric == "participation") %>%
  select(component, part_prior = prior, part_latest = latest) %>%
  mutate(part_change = part_latest - part_prior)

# ---- Chart 4a: change in who owns what ----
to_plot <- participation_shift %>%
  filter(component %in% varying_components) %>%
  mutate(component_label = pretty_component(component),
         label = paste0(ifelse(part_change > 0, "+", ""),
                        formatC(100 * part_change, format = "f", digits = 1),
                        "pp"),
         hj = ifelse(part_change > 0, -0.1, 1.1))

print("Change in share of households holding each component (pp):")
print(to_plot %>%
        arrange(part_change) %>%
        transmute(component_label,
                  prior = make_share_labels(part_prior, 1),
                  latest = make_share_labels(part_latest, 1),
                  change = label) %>%
        as.data.frame())

plot <- ggplot(to_plot, aes(x = reorder(component_label, part_change),
                            y = part_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label, hjust = hj),
            col = chart_standard_color, size = label_size) +
  coord_flip() +
  scale_y_continuous(label = function(x) paste0(100 * x, "pp"),
                     expand = expansion(mult = c(0.15, 0.15))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Who Owns What\n",
                 "Change in Share, ", prior_year, "-", latest_year)) +
  labs(x = NULL, y = "Change in Share of Households",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/04_component_participation_change.jpeg"))

# ---- Chart 4b: percent change in median value among owners ----
# When participation shifts, median-among-owners is NOT comparable across
# years. Those components get an asterisk.
to_plot <- component_change %>%
  filter(metric == "median_owners", !is.na(pct_change)) %>%
  left_join(participation_shift, by = "component") %>%
  mutate(composition_flag = abs(part_change) > composition_cutoff,
         component_label  = paste0(pretty_component(component),
                                   ifelse(composition_flag, " *", "")),
         label = make_pct_labels(pct_change),
         hj = ifelse(pct_change > 0, -0.1, 1.1))

print("Change in median value among owners:")
print(to_plot %>%
        arrange(pct_change) %>%
        transmute(component_label,
                  prior = format_as_dollar(prior),
                  latest = format_as_dollar(latest),
                  change = label) %>%
        as.data.frame())

composition_note <- paste0("* Ownership shifted more than ",
                           formatC(100 * composition_cutoff, format = "f",
                                   digits = 1),
                           "pp, so medians are not comparable.")

plot <- ggplot(to_plot, aes(x = reorder(component_label, pct_change),
                            y = pct_change)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(aes(label = label, hjust = hj),
            col = chart_standard_color, size = label_size) +
  coord_flip() +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.15, 0.15))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Median Holdings Among Owners\n",
                 "Real Change, ", prior_year, "-", latest_year)) +
  labs(x = NULL, y = "Change in Real Median Value",
       caption = make_caption(composition_note))

save_chart(plot, paste0(out_path, "/04_component_pct_change_owners.jpeg"))

# ---- Table ----
component_table <- component_change %>%
  mutate(display = ifelse(metric == "participation",
                          paste0(make_share_labels(prior, 1), " -> ",
                                 make_share_labels(latest, 1)),
                          paste0(format_as_dollar(prior), " -> ",
                                 format_as_dollar(latest))),
         metric = ifelse(metric == "participation", "Share holding",
                         "Median among holders")) %>%
  transmute(Component = pretty_component(component),
            Metric = metric,
            `Prior -> Latest` = display,
            `% Change` = make_pct_labels(pct_change))

write_html_table(component_table,
                 paste0(out_path, "/04_component_change_table.html"))

# ##################################################################### #
# SECTION 5: Wealth group cuts (top grouped as Top 10%)
# ##################################################################### #

wealth_group_change <- df_wealth %>%
  group_by(year, wealth_group) %>%
  summarise(median_networth = as.numeric(wtd.quantile(networth, weights = wgt,
                                                      probs = 0.5)),
            .groups = "drop") %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(wealth_group, year_label, median_networth) %>%
  pivot_wider(names_from = year_label, values_from = median_networth) %>%
  mutate(dollar_change = latest - prior,
         pct_change    = safe_pct_change(latest, prior))

print("Median net worth by wealth group:")
print(wealth_group_change %>%
        transmute(wealth_group,
                  prior = format_as_dollar(prior),
                  latest = format_as_dollar(latest),
                  pct_change = make_pct_labels(pct_change, 1)) %>%
        as.data.frame())

# ---- Chart 5: participation by wealth group ----
participation_vars <- intersect(c("stocks", "bus", "retqliq", "homeeq"),
                                varying_components)

if(length(participation_vars) > 0){
  
  wealth_participation <- df_wealth %>%
    select(year, wealth_group, wgt, all_of(participation_vars)) %>%
    pivot_longer(cols = all_of(participation_vars),
                 names_to = "component", values_to = "value") %>%
    group_by(year, wealth_group, component) %>%
    summarise(participation = as.numeric(wtd.mean(as.numeric(value > 0),
                                                  weights = wgt)),
              .groups = "drop") %>%
    mutate(period = period_factor(year),
           component = pretty_component(component))
  
  print("Share holding each asset, by wealth group:")
  print(wealth_participation %>%
          select(component, wealth_group, year, participation) %>%
          mutate(participation = make_share_labels(participation)) %>%
          pivot_wider(names_from = year, values_from = participation) %>%
          arrange(component, wealth_group) %>%
          as.data.frame())
  
  plot <- ggplot(wealth_participation,
                 aes(x = wealth_group, y = participation, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(vars(component), axes = "all") +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    period_fill_scale() +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0("Who Owns What\nby Wealth Group")) +
    labs(x = "Wealth Group", y = "Share of Households",
         caption = make_caption())
  
  save_chart(plot, paste0(out_path, "/05_participation_by_wealth_group.jpeg"))
}

wealth_group_table <- wealth_group_change %>%
  transmute(`Wealth Group` = as.character(wealth_group),
            Prior          = format_as_dollar(prior),
            Latest         = format_as_dollar(latest),
            `$ Change`     = format_as_dollar(dollar_change),
            `% Change`     = make_pct_labels(pct_change))

names(wealth_group_table)[2] <- as.character(prior_year)
names(wealth_group_table)[3] <- as.character(latest_year)

write_html_table(wealth_group_table,
                 paste0(out_path, "/05_wealth_group_change_table.html"))

# ##################################################################### #
# SECTION 6: Young vs. old - why did they move in opposite directions?
# ##################################################################### #

# ---- 6a: who owns what, by age ----
age_own_vars <- c(houses = "Owns a home", stocks = "Stocks (direct)",
                  retqliq = "Retirement accounts", bus = "Business")
age_own_vars <- age_own_vars[names(age_own_vars) %in% names(df_two_year)]

age_participation <- df_two_year %>%
  select(year, agecl, wgt, all_of(names(age_own_vars))) %>%
  pivot_longer(cols = all_of(names(age_own_vars)),
               names_to = "component", values_to = "value") %>%
  group_by(year, agecl, component) %>%
  summarise(participation = wtd_share(value > 0, wgt), .groups = "drop") %>%
  mutate(period = period_factor(year),
         component = factor(age_own_vars[component], levels = age_own_vars))

print("Share holding each asset, by age:")
print(age_participation %>%
        select(component, agecl, year, participation) %>%
        mutate(participation = make_share_labels(participation, 1)) %>%
        pivot_wider(names_from = year, values_from = participation) %>%
        arrange(component, agecl) %>%
        as.data.frame())

plot <- ggplot(age_participation,
               aes(x = agecl, y = participation, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(component), axes = "all") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  period_fill_scale() +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Who Owns What by Age\n",
                 prior_year, " vs. ", latest_year)) +
  labs(x = "Age", y = "Share of Households",
       caption = make_caption())

save_chart(plot, paste0(out_path, "/06_participation_by_age.jpeg"))

# ---- 6b: balance sheet of the youngest and oldest households ----
# Medians across ALL households in the group (zeros included), so they
# answer "what does the typical young household have", not "among owners".
balance_vars <- intersect(c("networth", "income", "fin", "liq", "retqliq",
                            "homeeq", "debt"), names(df_two_year))

young_old_balance <- df_two_year %>%
  filter(agecl %in% young_old) %>%
  select(year, agecl, wgt, all_of(balance_vars)) %>%
  pivot_longer(cols = all_of(balance_vars),
               names_to = "measure", values_to = "value") %>%
  group_by(agecl, measure, year) %>%
  summarise(median = wtd_stat(value, wgt, 0.5), .groups = "drop") %>%
  mutate(year_label = ifelse(year == latest_year, "latest", "prior")) %>%
  select(-year) %>%
  pivot_wider(names_from = year_label, values_from = median) %>%
  mutate(pct_change = safe_pct_change(latest, prior),
         measure = ifelse(measure == "networth", "Net worth",
                          ifelse(measure == "income", "Income",
                                 pretty_component(measure))))

print("Typical balance sheet (medians, zeros included), youngest vs. oldest:")
print(young_old_balance %>%
        transmute(agecl, measure,
                  prior = format_as_dollar(prior),
                  latest = format_as_dollar(latest),
                  pct_change = make_pct_labels(pct_change, 1)) %>%
        arrange(agecl, measure) %>%
        as.data.frame())

yo_table <- young_old_balance %>%
  arrange(agecl, measure) %>%
  transmute(Age = as.character(agecl),
            Measure = measure,
            Prior = format_as_dollar(prior),
            Latest = format_as_dollar(latest),
            `% Change` = make_pct_labels(pct_change))

names(yo_table)[3] <- as.character(prior_year)
names(yo_table)[4] <- as.character(latest_year)

write_html_table(yo_table,
                 paste0(out_path, "/06_young_vs_old_balance_sheet_table.html"))

# ---- 6c: did the groups themselves change? ----
# If more young households formed (or older ones were more likely to
# survive into the 75+ group), the medians shift without anyone getting
# richer or poorer. Check before writing the "why".
age_mix <- df_two_year %>%
  group_by(year) %>%
  mutate(total_wgt = sum(wgt)) %>%
  group_by(year, agecl) %>%
  summarise(share_of_households = sum(wgt) / first(total_wgt),
            households = sum(wgt),
            median_age = wtd_stat(age, wgt, 0.5),
            unweighted = n_distinct(hh_id),
            .groups = "drop")

print("Age mix of households (check for composition effects):")
print(age_mix %>%
        mutate(share_of_households = make_share_labels(share_of_households, 1),
               households = paste0(formatC(households/10^6, format = "f",
                                           digits = 1), "M")) %>%
        arrange(agecl, year) %>%
        as.data.frame())

# ##################################################################### #
# SECTION 7: Sanity checks
# ##################################################################### #

sanity <- pctiles %>% filter(prob == 0.5)

print(paste0("Output folder: ", out_path))
print(paste0("Median net worth ", prior_year, ": ",
             format_as_dollar(sanity$prior)))
print(paste0("Median net worth ", latest_year, ": ",
             format_as_dollar(sanity$latest)))
print(paste0("Real change: ", make_pct_labels(sanity$pct_change, 1)))

print("Components flagged for composition effects (read 4b with care):")
print(participation_shift %>%
        filter(abs(part_change) > composition_cutoff) %>%
        arrange(desc(abs(part_change))) %>%
        as.data.frame())

print("Records per year (all five implicates): ")
print(df %>% count(year) %>% filter(year %in% c(prior_year, latest_year)) %>%
        as.data.frame())

########################## Close the log ############################## #

cat("\nRun finished: ", format(Sys.time()), "\n", sep = "")

globalCallingHandlers(NULL)
sink()
close(log_con)

print(paste0("Log saved to: ", log_file))

# ############################  End  ################################## #