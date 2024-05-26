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

folder_name <- "_twl/0007_income_education_percentiles"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# Bring in assets and normalize percentages
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
                ),
               ) %>%
                select(networth, income, wealth_level, edcl, agecl, wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                    "L3 ($100k)", "L4 ($1M)",
                                                                    "L5 ($10M)", "L6 ($100M+)"))

#Plot income by education level
to_plot <- scf_stack %>%
  group_by(edcl) %>%
  summarise(income = wtd.quantile(income, weights = wgt, probs = 0.5)) %>%
  ungroup() 

file_path <- paste0(out_path, "/income_by_edcl.jpeg")
source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=edcl, y=income)) +
  geom_bar(stat = "identity", position = "dodge", fill = "black") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Median Income\nby Education Level")) +
  labs(x="Education Level", y=paste0("Income"),
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now plot by wealth level as well
income_by_edcl <- scf_stack %>%
  group_by(wealth_level, edcl) %>%
  summarise(income = wtd.quantile(income, weights = wgt, probs = 0.5)) %>%
  ungroup() 

to_plot <- income_by_edcl %>%
            filter(!grepl("L5|L6", wealth_level))

file_path <- paste0(out_path, "/edcl_income_by_wealth_level.jpeg")
source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x=edcl, y=income)) +
  geom_bar(stat = "identity", position = "dodge", fill = "black") +
  facet_rep_wrap(wealth_level ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Median Income\nby Education & Wealth Level")) +
  labs(x="Education Level", y=paste0("Income"),
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Do income by wealth level
income_by_wealth_level <- scf_stack %>%
  group_by(wealth_level) %>%
  summarise(pct_10 = wtd.quantile(income, weights = wgt, probs = 0.1),
            pct_25 = wtd.quantile(income, weights = wgt, probs = 0.25),
            pct_50 = wtd.quantile(income, weights = wgt, probs = 0.5),
            pct_75 = wtd.quantile(income, weights = wgt, probs = 0.75),
            pct_90 = wtd.quantile(income, weights = wgt, probs = 0.9)) %>%
  ungroup() 

# ############################  End  ################################## #