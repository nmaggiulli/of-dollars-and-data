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

folder_name <- "_twl/xxxx_age_by_wealth_level"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# asset = value of all assets (fin + nfin)

# Add Wealth levels
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
              mutate(wealth_level = case_when(
                            networth < 10000 ~ "L1 (<$10k)",
                            floor(log10(networth)) == 4 ~ "L2 ($10k)",
                            floor(log10(networth)) == 5 ~ "L3 ($100k)",
                            floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                            floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                            floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                            TRUE ~ "ERROR"
                          )) %>%
                select(wealth_level, age, wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                    "L3 ($100k)", "L4 ($1M)",
                                                                    "L5 ($10M)", "L6 ($100M+)"))

avg_age_by_wealth_lvl <- scf_stack %>%
              group_by(wealth_level) %>%
              summarise(average_age = wtd.mean(age, weights = wgt)) %>%
              ungroup()

#Calculate the age distribution across each wealth level
age_percentile_summary_by_level <- scf_stack %>%
  group_by(wealth_level) %>%
  summarise(age = wtd.quantile(age, 
                               weights = wgt, 
                               probs = seq(0.01, 0.99, 0.01)
  )
  ) %>%
  ungroup() %>%
  mutate(percentile = rep(seq(1, 99), 6))

pct_10_25_50_age_by_wealth_level <- age_percentile_summary_by_level %>%
            filter(percentile %in% c(10, 25, 50))

pct_1 <- age_percentile_summary_by_level %>%
  filter(percentile == 1)

# Now do all data years to get all wealth levels over time
scf_stack_full <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  mutate(wealth_level = case_when(
    networth < 10000 ~ 1,
    floor(log10(networth)) == 4 ~ 2,
    floor(log10(networth)) == 5 ~ 3,
    floor(log10(networth)) == 6 ~ 4,  
    floor(log10(networth)) == 7 ~ 5,  
    floor(log10(networth)) > 7 ~ 6, 
    TRUE ~ -999
  )) %>%
  select(year, wgt, age, wealth_level)

wealth_lvls_over_time <- scf_stack_full %>%
  mutate(
    l1_dummy = ifelse(wealth_level == 1, 1, 0),
    l2_dummy = ifelse(wealth_level == 2, 1, 0),
    l3_dummy = ifelse(wealth_level == 3, 1, 0),
    l4_dummy = ifelse(wealth_level == 4, 1, 0)) %>%
  group_by(year) %>%
  summarise(avg_age = wtd.mean(age, wgt),
            avg_wealth_level = wtd.mean(wealth_level, wgt),
            `L1 (<$10k)` = wtd.mean(l1_dummy, wgt),
            `L2 ($10k)` = wtd.mean(l2_dummy, wgt),
            `L3 ($100k)` = wtd.mean(l3_dummy, wgt),
            `L4 ($1M)` = wtd.mean(l4_dummy, wgt)
  ) %>%
  ungroup()

to_plot <- wealth_lvls_over_time %>%
  select(-avg_age, -avg_wealth_level) %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  gather(-year, key=key, value=value)

file_path <- paste0(out_path, "/wealth_levels_over_time.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (1989-2022)")

bw_colors <- c("#cccccc", "#969696", "#525252", "#252525")

# Create plot 
plot <- ggplot(data = to_plot, aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = bw_colors) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_date() +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Wealth Level Breakdown by Year\n1989-2022")) +
  labs(x = "Year" , y = "Percentage of All Households",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #