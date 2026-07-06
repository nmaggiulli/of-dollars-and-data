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

folder_name <- "_fl/xxxx_networth_percentile_calculator"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

min_age <- 20
max_age <- 25

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(age >= min_age,
                     age<= max_age) %>%
                mutate(wealth_level = case_when(
                  networth < 10000 ~ "L1 (<$10k)",
                  floor(log10(networth)) == 4 ~ "L2 ($10k)",
                  floor(log10(networth)) == 5 ~ "L3 ($100k)",
                  floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                  floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                  floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                  TRUE ~ "ERROR"
                ),
                liquid_assets = asset - reteq - nfin,
                liquid_networth = networth - reteq - nfin,
                networth_ex_homeeq = networth - homeeq,
                ) %>%
                select(networth, liquid_networth, networth_ex_homeeq, age, income, wealth_level, wgt, year)


min_year <- min(scf_stack$year)
max_year <- max(scf_stack$year)
p_all <- c(0.9, 0.95, 0.96, 0.97, 0.98, 0.99)

# --- Weighted net-worth percentiles by year (tidy/long) ----------------------
nw_pctiles <- scf_stack %>%
  group_by(year) %>%
  reframe(
    percentile = p_all,
    networth   = wtd.quantile(networth, weights = wgt, probs = p_all)
  ) %>%
  mutate(pct_label = factor(paste0(percentile * 100, "th"),
                            levels = paste0(p_all * 100, "th")))

to_plot <- nw_pctiles

file_path <- paste0(out_path, "/networth_pcts_over_time_", min_age, "_", max_age, ".jpeg")
source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: All figures are in 2022 inflation-adjusted dollars."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=year, y=  networth, fill = as.factor(pct_label))) +
  geom_bar(stat = "identity", position = "dodge") +
  of_dollars_and_data_theme +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(breaks = seq(min_year, max_year, 3)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Inflation-Adjusted Net Worth Percentiles\n", min_age, "-", max_age, " Year-Old Households")) +
  labs(x = "Year", y = "Net Worth",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")  


# ############################  End  ################################## #`