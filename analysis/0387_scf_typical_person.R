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

folder_name <- "0387_scf_typical_person"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

df <- scf_stack %>%
      select(year, birthyear, hh_id, imp_id, agecl, edcl,
             networth, income, homeeq, fin, houses,
             wgt) %>%
      arrange(year, hh_id, imp_id) %>%
      filter(birthyear >= 1965, birthyear < 1970)

n_hh <- length(unique(df$hh_id))

create_percentile_ot_chart <- function(var, var_title){
  
  to_plot <- df %>%
    rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
    group_by(year) %>%
    summarise(
      median = wtd.quantile(var_for_qtile, weights = wgt, probs=0.5),
    ) %>%
    ungroup()
  
  file_path <- paste0(out_path, "/", var, "_median_over_time_65_69_cohort.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Calculations based on weighted data from U.S. households. ",
                                  "All figures are adjusted for inflation in 2022 dollars.")
                           , width = 85)
  
  plot <- ggplot(to_plot, aes(x=year, y=median)) +
    geom_line(col = "black") +
    scale_color_discrete(guide = "none") +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0(var_title, " by Year\nFor People Born 1965-1969")) +
    labs(x="Year", y=paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  assign(var, to_plot, envir = .GlobalEnv)

}

create_percentile_ot_chart("income", "Real Median Income")
create_percentile_ot_chart("networth", "Real Median Net Worth")
create_percentile_ot_chart("houses", "Real Median Home Value")
create_percentile_ot_chart("homeeq", "Real Median Home Equity")
create_percentile_ot_chart("fin", "Real Median Financial Assets")

min_year <- min(df$year)
max_year <- max(df$year)

all_years <- seq(min_year, max_year, 1)

#Read in SP500
sp500_ret_pe <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  filter(month(date) == 1, year(date) >= min_year, year(date) <= max_year + 1) %>%
                  mutate(ret_stock_real = price_plus_div/lag(price_plus_div, 1) - 1,
                         ret_bond_real = bond_real/lag(bond_real, 1) - 1,
                         year = year(date) - 1) %>%
                  select(year, ret_stock_real, ret_bond_real)

all_years_df <- data.frame(year = all_years)

final <- all_years_df %>%
                  left_join(income) %>%
                  left_join(sp500_ret_pe) %>%
                  mutate(income = case_when(
                    year %% 3 == 0 ~ median,
                    year %% 3 == 1 ~ (lead(median, 2) - lag(median, 1))/3 + lag(median, 1),
                    year %% 3 == 2 ~ 2*(lead(median, 1) - lag(median, 2))/3 + lag(median, 2),
                    TRUE ~ -999
                  ),
                  constant_savings_1p3pct = 0.013,
                  constant_savings_5pct = 0.05,
                  constant_savings_10pct = 0.10,
                  constant_savings_15pct = 0.15,
                  save_5pct_above_50k = case_when(
                    income > 50000 ~ 0.05,
                    TRUE ~ 0
                  ),
                  save_10pct_above_50k = case_when(
                    income > 50000 ~ 0.1,
                    TRUE ~ 0
                  ),
                  save_1pct_per_year_max_20pct = case_when(
                    (year - min_year) > 19 ~ 0.2,
                    TRUE ~  0.01*(year - min_year) + 0.01
                  ),
                  savings_actual_1p3pct = income*constant_savings_1p3pct,
                  savings_actual_5pct = income*constant_savings_5pct,
                  savings_actual_10pct = income*constant_savings_10pct,
                  savings_actual_15pct = income*constant_savings_15pct,
                  stock_weight = 0.95 - 0.01*(year-min_year)
                  ) %>%
                  select(-median)

calculate_final_port <- function(savings_rate_scenario){

  for(i in 1:nrow(final)){
    stock_weight <- final[i, "stock_weight"]
    bond_weight <- 1 - stock_weight
    
    if(i == 1){
      final[i, "value_stock"] <- (final[i, "income"] * final[i, savings_rate_scenario] * stock_weight ) * (1 + final[i, "ret_stock_real"])
      final[i, "value_bond"] <- (final[i, "income"] * final[i, savings_rate_scenario] * bond_weight) * (1 + final[i, "ret_bond_real"])
    } else{
      final[i, "value_stock"] <- ((final[(i-1), "value_port"] * stock_weight)  + (final[i, "income"] * final[i, savings_rate_scenario] * stock_weight)) * (1 + final[i, "ret_stock_real"])
      final[i, "value_bond"] <- ((final[(i-1), "value_port"] * bond_weight) + (final[i, "income"] * final[i, savings_rate_scenario] * bond_weight)) * (1 + final[i, "ret_bond_real"])
    }
    final[i, "value_port"] <- final[i, "value_stock"] + final[i, "value_bond"]
  }
    
  print(final[nrow(final), "value_port"])
}

calculate_final_port("constant_savings_1p3pct")
calculate_final_port("constant_savings_5pct")
calculate_final_port("constant_savings_10pct")
calculate_final_port("constant_savings_15pct")
calculate_final_port("save_5pct_above_50k")
calculate_final_port("save_1pct_per_year_max_20pct")

# ############################  End  ################################## #