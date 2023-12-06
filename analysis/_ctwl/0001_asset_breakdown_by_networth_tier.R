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

folder_name <- "_ctwl/0001_asset_breakdown_by_networth_tier"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# asset = value of all assets (fin + nfin)

# Bring in assets and normalize percentages
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year) %>%
              mutate(`Business Interests` = bus/asset,
                     `Real Estate` = (oresre+nnresre)/asset,
                     `Primary Residence` = houses/asset,
                     `Vehicles` = vehic/asset,
                     `Retirement` =  retqliq/asset,
                     `Mutual Funds/Stocks` = (nmmf + stocks)/asset, 
                     `Cash` = liq/asset,
                     `Other` = (savbnd + othfin + othnfin + cashli + othma + bond + cds)/asset,
                     networth_tier = case_when(
                       networth < 10000 ~ "<$10k (L1)",
                       floor(log10(networth)) == 4 ~ "$100k (L2)",
                       floor(log10(networth)) == 5 ~ "$1M (L3)",
                       floor(log10(networth)) == 6 ~ "$10M (L4)",  
                       floor(log10(networth)) == 7 ~ "$100M (L5)",  
                       floor(log10(networth)) > 7 ~ "$100M+ (L6)", 
                             TRUE ~ "ERROR"
                     )) %>%
                select(networth_tier, `Business Interests`, `Real Estate`,`Primary Residence`,
                       `Vehicles`, `Retirement`,
                       `Mutual Funds/Stocks`, `Cash`, `Other`,
                       wgt)

scf_stack$networth_tier <- factor(scf_stack$networth_tier, levels = c("<$10k (L1)", "$100k (L2)",
                                                                      "$1M (L3)", "$10M (L4)",
                                                                      "$100M (L5)", "$100M+ (L6)"))

to_plot <- scf_stack %>%
              group_by(networth_tier) %>%
              summarise(`Business Interests` = wtd.mean(`Business Interests`, weights = wgt),
                        `Real Estate` = wtd.mean(`Real Estate`, weights = wgt),
                        `Primary Residence` = wtd.mean(`Primary Residence`, weights = wgt),
                        `Vehicles` = wtd.mean(`Vehicles`, weights = wgt),
                        `Retirement` = wtd.mean(`Retirement`, weights = wgt),
                        `Mutual Funds/Stocks` = wtd.mean(`Mutual Funds/Stocks`, weights = wgt),
                        `Cash` = wtd.mean(`Cash`, weights = wgt),
                        `Other` = wtd.mean(`Other`, weights = wgt)
                        ) %>%
              ungroup() %>%
              gather(-networth_tier, key=key, value=value)

file_path <- paste0(out_path, "/asset_breakdown_by_nw_tier.jpeg")

my_colors <- c("#1f78b4", "#a6cee3", "#33a02c", "#FFDB58", "#e31a1c", "#fb9a99",
            "purple", "#ff7f00")

# Create plot 
plot <- ggplot(data = to_plot, aes(x = networth_tier, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Networth Tier" , y = "Percentage of Assets") +
  ggtitle(paste0("Asset Breakdown by Networth Tier"))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #