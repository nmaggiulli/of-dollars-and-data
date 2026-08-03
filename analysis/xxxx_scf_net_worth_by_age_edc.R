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
# CHANGE data_year WHEN THE 2025 DATA LANDS. Everything downstream - the
# output subfolder, titles, source strings, notes, filenames - keys off it.

data_year   <- 2022   # -> 2025

# The dollar basis of 0003_scf_stack.Rds. If the stack deflates every wave
# to the most recent year, this is that year, NOT necessarily data_year.
dollar_year <- 2022   # -> 2025

########################## Output paths ############################### #

folder_name <- "xxxx_scf_net_worth_by_age_edc"
base_path   <- paste0(exportdir, folder_name)
out_path    <- paste0(base_path, "/", data_year)

dir.create(file.path(paste0(base_path)), showWarnings = FALSE)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
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
    ungroup()
  
  file_path <- paste0(out_path, "/", var, "_", qps,
                      "_age_edc_comb_", data_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x = agecl, y = value)) +
    geom_bar(stat = "identity", position = "dodge",
             fill = chart_standard_color) +
    facet_rep_wrap(edcl ~ ., scales = "free_y",
                   repeat.tick.labels = c("left", "bottom")) +
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
      mutate(label = make_dollar_labels(value))
    
    file_path <- paste0(out_path, "/", var, "_", qps, "_",
                        end_filename, ".jpeg")
    
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

print(paste0("Output folder: ", out_path))
print(paste0("Unweighted households (single implicate): ",
             formatC(n_hh, digits = 0, format = "f", big.mark = ",")))
print(paste0("Median net worth ", data_year, ": ",
             format_as_dollar(wtd_stat(df$networth, df$wgt, 0.5))))
print(paste0("Average net worth ", data_year, ": ",
             format_as_dollar(wtd_stat(df$networth, df$wgt, 0))))

# ############################  End  ################################## #