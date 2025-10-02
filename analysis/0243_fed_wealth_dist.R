cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0243_fed_wealth_dist"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Generation assumptions
# Silent Gen born in 1937 (1937 on average)
# Boomers born in 1946-964 (1955 on average)
# Born in 1965-1980 (1972 on average)
# Millennials born in 1981-1996 (1989 on average)

dollar_year <- 2025

my_colors <- c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9")

cpi <- readRDS(paste0(localdir, "0021_FRED_cpi.Rds")) %>%
        filter(year >= 1989) %>%
        arrange(-year)

for(i in 1:nrow(cpi)){
  if(i == 1){
    cpi[i, "inflator"] <- 1
  } else{
    cpi[i, "inflator"] <- cpi[(i-1), "inflator"]*(1+cpi[i, "rate_cpi"])
  }
}

cpi <- cpi %>%
        arrange(year) %>%
        select(year, inflator)

fed_wealth_data <- read.csv(paste0(importdir, "/0243_fed_distributional_financial_accounts/fed_dfa_data/dfa-generation-levels-detail.csv")) %>%
                      clean_cols() %>%
                      filter(grepl("Q4", date)) %>%
                      mutate(year = as.numeric(substr(date, 1, 4)),
                             re = real_estate,
                             non_mortgage_debt = loans__liabilities_ - home_mortgages) %>%
                      select(year, category, household_count, net_worth, assets, re, liabilities,
                             home_mortgages, financial_assets, consumer_credit, money_market_fund_shares,
                             non_mortgage_debt
                             ) %>%
                      mutate(age = case_when(
                        category == "Silent" ~ year - 1938,
                        category == "BabyBoom" ~ year - 1955,
                        category == "GenX" ~ year - 1972,
                        category == "Millennial" ~ year - 1989,
                        TRUE ~ 0
                      ))

df <- fed_wealth_data %>%
        inner_join(cpi) %>%
        mutate(real_nw = net_worth*inflator,
               real_assets = assets*inflator,
               real_re = re*inflator,
               real_liabilities = liabilities*inflator,
               real_mortgages = home_mortgages*inflator,
               real_fa = financial_assets*inflator,
               real_cc = consumer_credit*inflator,
               real_mm = money_market_fund_shares*inflator,
               real_nhd = non_mortgage_debt*inflator,
               category = ifelse(category == "BabyBoom", "BabyBoomers", category)) %>%
      select(year, category, age, contains("real_"), household_count) %>%
      arrange(category, year)

df$category <- factor(df$category, levels = c("Millennial", "GenX", "BabyBoomers", "Silent"))

vars_to_plot <- data.frame(var = c("nw", "assets", "re", "fa", "mm", "liabilities", "mortgages", "nhd", "cc"),
                           proper_name = c("Net Worth", "Assets", "Real Estate", "Financial Assets", "Money Market Assets",
                                           "Liabilities", "Mortgage Debt", "Non-Home Debt", "Consumer Credit"))

for(i in 1:nrow(vars_to_plot)){
  full_var <- paste0("real_", vars_to_plot[i, "var"])
  proper_name <- paste0(vars_to_plot[i, "proper_name"])
  
  to_plot <- df %>%
    rename_(.dots = setNames(full_var, "var_to_plot")) %>%
    mutate(var_to_plot = var_to_plot/(10^6)) %>%
    select(age, category, var_to_plot, household_count)
    
  file_path <- paste0(out_path, "/age_", full_var, ".jpeg")
  source_string <- "Source:  FRED, FED Distributional Accounts (OfDollarsAndData.com)"
  note_string <- str_wrap(paste0("Note: Assumes that, on average, the Silent Generation was born in 1938, BabyBoomers were born in 1955, 
                                 GenX was born in 1972, and Millennials were born in 1989."),
                          width = 75)
  
  plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = my_colors) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, "\nBy Age and Generation")) +
    labs(x="Average Age", y=paste0("Value Per Capita (", dollar_year," Dollars)"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do per capita plot
  to_plot <- to_plot %>%
            mutate(var_to_plot = var_to_plot/household_count*(10^6)*(10^6))
  
  assign(paste0("per_cap_", full_var), to_plot, envir = .GlobalEnv)
  
  file_path <- paste0(out_path, "/per_capita_age_", full_var, ".jpeg")
  source_string <- "Source:  FRED, FED Distributional Accounts (OfDollarsAndData.com)"
  
  plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = my_colors) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, " Per Capita\nBy Age and Generation")) +
    labs(x="Average Age", y=paste0("Value Per Capita (", dollar_year," Dollars)"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Bring in SCF data
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(agecl %in% c("<35")) %>%
              mutate(rent_over_income = case_when(
                rent > 0 & (rent*12 > income) ~ 1,
                rent > 0 ~ (rent*12)/income,
                TRUE ~ NaN),
                nhd = debt - mrthel
              )

summary_age_nw <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(agecl %in% c("<35", "35-44", "45-54", "55-64")) %>%
  mutate(rent_over_income = case_when(
    rent > 0 & (rent*12 > income) ~ 1,
    rent > 0 ~ (rent*12)/income,
    TRUE ~ NaN),
    nhd = debt - mrthel
  ) %>%
  group_by(year, agecl) %>%
  summarise(
    `50th Percentile` = wtd.quantile(networth, weights = wgt, probs= 0.5),
  ) %>%
  ungroup() %>%
  gather(-year, -agecl, key=key, value=value) 

file_path <- paste0(out_path, "/_age_nw_dist_by_year.jpeg")
source_string <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"

plot <- ggplot(summary_age_nw, aes(x=year, y = value, col = key)) +
  geom_line() +
  facet_rep_wrap(agecl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
  scale_color_manual(values = c(my_colors[1]), guide = FALSE) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Median Net Worth By Year\nBy Household Age")) +
  labs(x="Year", y="Networth",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


source_string <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"

summary <- scf_stack %>%
  group_by(edcl, year) %>%
  summarise(
    `10th Percentile` = wtd.quantile(networth, weights = wgt, probs= 0.1),
    `50th Percentile` = wtd.quantile(networth, weights = wgt, probs= 0.5),
    `90th Percentile` = wtd.quantile(networth, weights = wgt, probs= 0.90)
  ) %>%
  ungroup() %>%
  gather(-edcl, -year, key=key, value=value) 

to_plot <- summary %>%
  filter(key %in% c("10th Percentile", "50th Percentile"))

to_plot$key <- factor(to_plot$key, levels = c("50th Percentile", "10th Percentile"))

file_path <- paste0(out_path, "/under_35_nw_dist_by_year_10_50.jpeg")

plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
  geom_line() +
  facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
  scale_color_manual(values = c(my_colors[1:2])) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Inflation-Adjusted Net Worth By Year\nHouseholds under 35")) +
  labs(x="Year", y="Networth",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do summaries for other vars
scf_vars_to_plot <- data.frame(
  var = c("nhd", "income", "homeeq", "rent", "networth"),
  proper_name = c("Non-Home Debt", "Income", "Home Equity", "Monthly Rent", "Net Worth")
  )

for(i in 1:nrow(scf_vars_to_plot)){
  var <- scf_vars_to_plot[i, "var"]
  proper_name <- scf_vars_to_plot[i, "proper_name"]
  
  tmp <- scf_stack %>%
    rename_(.dots = setNames(var, "var_to_summarize")) %>%
    group_by(edcl, year) %>%
    summarise(
      `10th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.1),
      `25th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.25),
      `50th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.5),
      `75th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.75),
      `90th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.90)
    ) %>%
    ungroup() %>%
    gather(-edcl, -year, key=key, value=value) 
  
  if(var == "nhd"){
    to_plot <- tmp %>%
                filter(key %in% c("50th Percentile", "75th Percentile", "90th Percentile"))
  } else{
    to_plot <- tmp %>%
      filter(key %in% c("10th Percentile", "50th Percentile", "90th Percentile"))
  }
  
  file_path <- paste0(out_path, "/under_35_ed_", var, "_by_year.jpeg")
  
  plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
    geom_line() +
    facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
    scale_color_manual(values = c(my_colors[1:3])) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, " By Year\nHouseholds under 35")) +
    labs(x="Year", y=proper_name,
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  to_plot <- scf_stack %>%
    rename_(.dots = setNames(var, "var_to_summarize")) %>%
    group_by(year) %>%
    summarise(
      `50th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.5),
      `75th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.75),
      `90th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.90)
    ) %>%
    ungroup() %>%
    gather(-year, key=key, value=value) 
  
  file_path <- paste0(out_path, "/under_35_all_50_90_", var, "_by_year.jpeg")
  
  plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
    geom_line() +
    scale_color_manual(values = c(my_colors[1:3])) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, " By Year\nHouseholds under 35")) +
    labs(x="Year", y=proper_name,
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  #Do 10-50
  to_plot <- scf_stack %>%
    rename_(.dots = setNames(var, "var_to_summarize")) %>%
    group_by(year) %>%
    summarise(
      `10th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.1),
      `25th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.25),
      `50th Percentile` = wtd.quantile(var_to_summarize, weights = wgt, probs= 0.50)
    ) %>%
    ungroup() %>%
    gather(-year, key=key, value=value) 
  
  if(var == "networth"){
    assign("to_plot_nw_10_50", to_plot, envir = .GlobalEnv)
  }
  
  file_path <- paste0(out_path, "/under_35_all_10_50_", var, "_by_year.jpeg")
  
  plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
    geom_line() +
    scale_color_manual(values = c(my_colors[1:3])) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, " By Year\nHouseholds under 35")) +
    labs(x="Year", y=proper_name,
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Do rent over income summary
to_plot <- scf_stack %>%
  filter(!is.nan(rent_over_income)) %>%
  group_by(year) %>%
  summarise(
    `50th Percentile` = wtd.quantile(rent_over_income, weights = wgt, probs= 0.5),
    `75th Percentile` = wtd.quantile(rent_over_income, weights = wgt, probs= 0.75),
    `90th Percentile` = wtd.quantile(rent_over_income, weights = wgt, probs= 0.90)
  ) %>%
  ungroup() %>%
  gather(-year, key=key, value=value) 

file_path <- paste0(out_path, "/under_35_all_rent_over_income_by_year.jpeg")
note_string <- str_wrap(paste0("Note: Shows renters only. Annual rent exceeding annual income has been capped at 100%."),
                        width = 75)

plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
  geom_line() +
  scale_color_manual(values = c(my_colors[1:3])) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Inflation-Adjusted Rent Over Income By Year\nHouseholds under 35")) +
  labs(x="Year", y="Rent Over Income",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #