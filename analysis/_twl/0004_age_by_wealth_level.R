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

folder_name <- "_twl/0004_age_by_wealth_level"
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

to_plot <- scf_stack %>%
              group_by(wealth_level) %>%
              summarise(average_age = wtd.mean(age, weights = wgt)) %>%
              ungroup()

file_path <- paste0(out_path, "/age_breakdown_by_wealth_level.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (2022)")

text_labels <- to_plot %>%
                  mutate(label = round(average_age, 0))

# Create plot 
plot <- ggplot(data = to_plot, aes(x = wealth_level, y=average_age)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  geom_text(data = text_labels, aes(x = wealth_level, y=average_age, label = label),
            vjust = 1.5,
            color = "white") +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Average Age by Wealth Level")) +
  labs(x = "Wealth Level (Net Worth Tier)" , y = "Average Age",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

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

median <- age_percentile_summary_by_level %>%
            filter(percentile == 50)

pct_1 <- age_percentile_summary_by_level %>%
  filter(percentile == 1)

# ############################  End  ################################## #