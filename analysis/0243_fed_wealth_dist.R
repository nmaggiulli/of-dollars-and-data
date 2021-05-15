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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0243_fed_wealth_dist"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Generation assumptions
# Silent Gen born in 1937 (on average)
# Boomers born in 1955 (on average)
# Born in 1972 (on average)
# Millennials born in 1989 (on average)

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
                        category == "Silent" ~ year - 1937,
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
               real_nmd = non_mortgage_debt*inflator,
               category = ifelse(category == "BabyBoom", "BabyBoomers", category)) %>%
      select(year, category, age, contains("real_"), household_count) %>%
      arrange(category, year) %>%
      mutate(growth_real_nw = case_when(
        category == lag(category, 1) ~ real_nw/lag(real_nw, 1) - 1,
        TRUE ~ NaN
      ))

df$category <- factor(df$category, levels = c("Millennial", "GenX", "BabyBoomers", "Silent"))

vars_to_plot <- data.frame(var = c("nw", "assets", "re", "fa", "mm", "liabilities", "mortgages", "nmd", "cc"),
                           proper_name = c("Net Worth", "Assets", "Real Estate", "Financial Assets", "Money Market Assets",
                                           "Liabilities", "Mortgage Debt", "Non-Mortgage Debt", "Consumer Credit"))

for(i in 1:nrow(vars_to_plot)){
  full_var <- paste0("real_", vars_to_plot[i, "var"])
  proper_name <- paste0(vars_to_plot[i, "proper_name"])
  
  to_plot <- df %>%
    rename_(.dots = setNames(full_var, "var_to_plot")) %>%
    select(age, category, var_to_plot, household_count)
    
  file_path <- paste0(out_path, "/age_", full_var, ".jpeg")
  source_string <- "Source:  FRED, FED Distributional Accounts (OfDollarsAndData.com)"
  note_string <- str_wrap(paste0("Note: Assumes that the Silent Generation was born in 1937, BabyBoomers were born in 1955, 
                                 GenX was born in 1972, and Millennials were born in 1989."),
                          width = 75)
  
  plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, "\nBy Age and Generation")) +
    labs(x="Age", y="Value (2020 Dollars)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Do per capita plot
  to_plot <- to_plot %>%
            mutate(var_to_plot = var_to_plot/household_count*(10^6))
  
  file_path <- paste0(out_path, "/per_capita_age_", full_var, ".jpeg")
  source_string <- "Source:  FRED, FED Distributional Accounts (OfDollarsAndData.com)"
  
  plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted ", proper_name, " Per Capita\nBy Age and Generation")) +
    labs(x="Age", y="Value Per Capita (2020 Dollars)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}









# ############################  End  ################################## #