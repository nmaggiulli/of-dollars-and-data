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

folder_name <- "_ctwl/0001_asset_breakdown_by_wealth_level"
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
                       floor(log10(networth)) == 4 ~ "L2 ($100k)",
                       floor(log10(networth)) == 5 ~ "L3 ($1M)",
                       floor(log10(networth)) == 6 ~ "L4 ($10M)",  
                       floor(log10(networth)) == 7 ~ "L5 ($100M)",  
                       floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                             TRUE ~ "ERROR"
                     )) %>%
                select(wealth_level, age, wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($100k)",
                                                                      "L3 ($1M)", "L4 ($10M)",
                                                                      "L5 ($100M)", "L6 ($100M+)"))

to_plot <- scf_stack %>%
              group_by(wealth_level) %>%
              summarise(`Business Interests` = wtd.mean(`Business Interests`, weights = wgt),
                        `Real Estate` = wtd.mean(`Real Estate`, weights = wgt),
                        `Primary Residence` = wtd.mean(`Primary Residence`, weights = wgt),
                        `Vehicles` = wtd.mean(`Vehicles`, weights = wgt),
                        `Retirement` = wtd.mean(`Retirement`, weights = wgt),
                        `Stocks & Mutual Funds` = wtd.mean(`Stocks & Mutual Funds`, weights = wgt),
                        `Cash` = wtd.mean(`Cash`, weights = wgt),
                        `Other` = wtd.mean(`Other`, weights = wgt)
                        ) %>%
              ungroup() %>%
              gather(-wealth_level, key=key, value=value)

file_path <- paste0(out_path, "/_asset_breakdown_by_nw_tier_all_color.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (2022)")


# Create plot 
plot <- ggplot(data = to_plot, aes(x = wealth_level, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("Asset Breakdown by Wealth Level")) +
  labs(x = "Wealth Level (Net Worth Tier)" , y = "Percentage of Assets",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #