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
library(tidyverse)

folder_name <- "_jkb/0016_fed_distributional_financial_accounts"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#000000", "#969696", "#cccccc")

# Get CPI data
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

# Bring in Fed DFA data
fed_wealth_data <- read.csv(paste0(importdir, folder_name, "/dfa-generation-levels-detail.csv")) %>%
  clean_cols() %>%
  filter(grepl("Q4", date), category != "Silent") %>%
  mutate(year = as.numeric(substr(date, 1, 4)),
         re = real_estate,
         non_mortgage_debt = loans__liabilities_ - home_mortgages) %>%
  select(year, category, household_count, net_worth, assets, re, liabilities,
         home_mortgages, financial_assets, consumer_credit, money_market_fund_shares,
         non_mortgage_debt
  ) %>%
  mutate(age = case_when(
    category == "BabyBoom" ~ year - 1955,
    category == "GenX" ~ year - 1972,
    category == "Millennial" ~ year - 1989,
    TRUE ~ 0
  ))

df <- fed_wealth_data %>%
  inner_join(cpi) %>%
  mutate(real_nw = net_worth*inflator,
         real_fa = financial_assets*inflator,
         category = ifelse(category == "BabyBoom", "BabyBoomers", category)) %>%
  select(year, category, age, contains("real_"), household_count) %>%
  arrange(category, year) %>%
  filter(age <=50)

df$category <- factor(df$category, levels = c("Millennial", "GenX", "BabyBoomers"))

to_plot <- df %>%
            mutate(var_to_plot = real_fa/household_count*(10^6))

file_path <- paste0(out_path, "/per_capita_real_financial_assets.jpeg")

plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = bw_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Inflation-Adjusted Financial Assets Per Capita\nBy Age and Generation")) +
  labs(x="Age", y="Value Per Capita (2020 Dollars)")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- df %>%
  mutate(var_to_plot = real_nw/household_count*(10^6))

file_path <- paste0(out_path, "/per_capita_real_networth.jpeg")

plot <- ggplot(to_plot, aes(x=age, y = var_to_plot, col = category)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = bw_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Inflation-Adjusted Net Worth Per Capita\nBy Age and Generation")) +
  labs(x="Age", y="Value Per Capita (2020 Dollars)")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #