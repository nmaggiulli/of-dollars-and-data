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

folder_name <- "_twl/0008_income_by_wealth_level"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# asset = value of all assets (fin + nfin)

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year, networth > 1000) %>%
              mutate(`Income` = income,
                     wealth_level = case_when(
                       networth < 10000 ~ "L1 (<$10k)",
                       floor(log10(networth)) == 4 ~ "L2 ($10k)",
                       floor(log10(networth)) == 5 ~ "L3 ($100k)",
                       floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                       floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                       floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                             TRUE ~ "ERROR"
                     )) %>%
                select(wealth_level, networth, `Income`,
                       wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                      "L3 ($100k)", "L4 ($1M)",
                                                                      "L5 ($10M)", "L6 ($100M+)"))

to_plot <- scf_stack %>%
              filter(!grepl("L6", wealth_level)) %>%
              group_by(wealth_level) %>%
              summarise(`Income` = wtd.quantile(`Income`, weights = wgt, probs = 0.5)
                        ) %>%
              ungroup() %>%
              gather(-wealth_level, key=key, value=value)

all_levels_median <- to_plot %>%
                arrange(wealth_level, desc(value))

file_path <- paste0(out_path, "/median_income_by_wealth_level.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (2022)")

# Create plot 
plot <- ggplot(data = to_plot, aes(x = wealth_level, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = "black", guide = "none") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("Median Income by Wealth Level")) +
  labs(x = "Wealth Level (Net Worth Tier)" , y = "Income",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #