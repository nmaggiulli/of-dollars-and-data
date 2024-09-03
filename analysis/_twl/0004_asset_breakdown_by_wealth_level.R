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

folder_name <- "_twl/0004_asset_breakdown_by_wealth_level"
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
                     `Stocks & Mutual Funds` = (nmmf + stocks)/asset, 
                     `Cash` = liq/asset,
                     `Other` = (savbnd + othfin + othnfin + cashli + othma + bond + cds)/asset,
                     owns_home = ifelse(houses > 0, 1, 0),
                     wealth_level = case_when(
                       networth < 10000 ~ "L1 (<$10k)",
                       floor(log10(networth)) == 4 ~ "L2 ($10k)",
                       floor(log10(networth)) == 5 ~ "L3 ($100k)",
                       floor(log10(networth)) == 6 ~ "L4 ($1M)",  
                       floor(log10(networth)) == 7 ~ "L5 ($10M)",  
                       floor(log10(networth)) > 7 ~ "L6 ($100M+)", 
                             TRUE ~ "ERROR"
                     )) %>%
                select(wealth_level, networth, `Business Interests`, `Real Estate`,`Primary Residence`,
                       `Vehicles`, `Retirement`,
                       `Stocks & Mutual Funds`, `Cash`, `Other`,
                       owns_home,
                       wgt)

scf_stack$wealth_level <- factor(scf_stack$wealth_level, levels = c("L1 (<$10k)", "L2 ($10k)",
                                                                      "L3 ($100k)", "L4 ($1M)",
                                                                      "L5 ($10M)", "L6 ($100M+)"))

all_levels_median <- scf_stack %>%
  group_by(wealth_level) %>%
  summarise(`Business Interests` = wtd.quantile(`Business Interests`, weights = wgt, probs = 0.5),
            `Real Estate` =  wtd.quantile(`Real Estate`, weights = wgt, probs = 0.5),
            `Primary Residence` =  wtd.quantile(`Primary Residence`, weights = wgt, probs = 0.5),
            `Vehicles` =  wtd.quantile(`Vehicles`, weights = wgt, probs = 0.5),
            `Retirement` =  wtd.quantile(`Retirement`, weights = wgt, probs = 0.5),
            `Stocks & Mutual Funds` =  wtd.quantile(`Stocks & Mutual Funds`, weights = wgt, probs = 0.5),
            `Cash` =  wtd.quantile(`Cash`, weights = wgt, probs = 0.5),
            `Other` =  wtd.quantile(`Other`, weights = wgt, probs = 0.5)
  ) %>%
  ungroup() %>%
  gather(-wealth_level, key=key, value=value) %>%
  arrange(wealth_level, desc(value))

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

all_levels <- to_plot %>%
                arrange(wealth_level, desc(value))

assign("all_levels", all_levels, envir = .GlobalEnv)

export_to_excel(df = all_levels,
                outfile = paste0(out_path, "/asset_breakdown_by_wealth_level.xlsx"),
                sheetname = "all_levels",
                new_file = 1,
                fancy_formatting = 0)

income_producing_summary <- all_levels %>%
                      mutate(key = case_when(
                        key %in% c("Vehicles", "Cash", "Other", "Primary Residence") ~ "Non-income producing",
                        TRUE ~ "Income-producing"
                      )) %>%
                    group_by(wealth_level, key) %>%
                    summarise(value = sum(value)) %>%
                    ungroup()

income_producing_summary$key <- factor(income_producing_summary$key, levels = c("Non-income producing", "Income-producing"))

assign("income_producing_summary", income_producing_summary, envir = .GlobalEnv)

#Do income-producing summary
my_grayscale <- c("#969696", "black")

file_path <- paste0(out_path, "/_income_producing_breakdown_by_wealth_level.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (2022)")

plot <- ggplot(data = income_producing_summary, aes(x = wealth_level, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = my_grayscale) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("Income-Producing Assets by Wealth Level")) +
  labs(x = "Wealth Level (Net Worth Tier)" , y = "Percentage of Assets",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do overall breakdown
file_path <- paste0(out_path, "/_asset_breakdown_by_wealth_level_all_color.jpeg")

my_colors <- c("#1f78b4", "#a6cee3", "#33a02c", "#FFDB58", "#e31a1c", "#fb9a99",
            "purple", "#ff7f00")

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

# Now do grayscale
file_path <- paste0(out_path, "/_asset_breakdown_by_wealth_level_grayscale.jpeg")

# Create plot 
plot <- ggplot(data = to_plot, aes(x = wealth_level, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = my_colors) +
  scale_fill_grey(start = 0, end = .9) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("Asset Breakdown by Wealth Level")) +
  labs(x = "Wealth Level" , y = "Percentage of Assets",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

all_assets <- c("Business Interests", "Cash", "Stocks & Mutual Funds", 
                "Primary Residence", "Real Estate", "Retirement",
                "Vehicles")

# Loop through assets (in grayscale)
for(a in all_assets){
  to_plot <- scf_stack %>%
    group_by(wealth_level) %>%
    rename_(.dots = setNames(paste0("`", a, "`"), "var_for_qtile")) %>%
    summarise(pct_var = wtd.mean(var_for_qtile, weights = wgt)) %>%
    ungroup() %>%
    mutate(`All Other Assets` = 1 - pct_var) %>%
    rename_(.dots = setNames("pct_var", a)) %>%
    gather(-wealth_level, key=key, value=value)
  
  a_string <- str_replace_all(a, "\\s|&", "_")
  
  file_path <- paste0(out_path, "/", a_string, "_by_wealth_level_grayscale.jpeg")
  
  if(a == "Retirement"){
    a_title <- "Retirement Wealth"
  } else if (a == "Vehicles"){
    a_title <- "Vehicle Wealth"
  } else{
    a_title <- a
  }
  
  # Create plot 
  plot <- ggplot(data = to_plot, aes(x = wealth_level, y=value, fill = key)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
    scale_fill_manual(values = my_grayscale) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    ggtitle(paste0(a_title, " by Wealth Level")) +
    labs(x = "Wealth Level (Net Worth Tier)" , y = "Percentage of Assets",
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Do homeowner only analysis
homeowner_only_avg <- scf_stack %>%
  filter(owns_home == 1) %>%
  group_by(wealth_level) %>%
  summarise(
            `Primary Residence` = wtd.mean(`Primary Residence`, weights = wgt),
  ) %>%
  ungroup() %>%
  mutate(`All Other Assets` = 1 - `Primary Residence`) %>%
  gather(-wealth_level, key=key, value=value)

file_path <- paste0(out_path, "/homeowner_residence_by_wealth_level_grayscale.jpeg")

# Create plot 
plot <- ggplot(data = homeowner_only_avg, aes(x = wealth_level, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = my_grayscale) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle(paste0("Primary Residence by Wealth Level\nAmong Homeowners")) +
  labs(x = "Wealth Level (Net Worth Tier)" , y = "Percentage of Assets",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Calculate homeownership rate
homeown_rate <- scf_stack %>%
  group_by(wealth_level) %>%
  summarise(
    owns_home = wtd.mean(owns_home, weights = wgt),
  ) %>%
  ungroup()

# ############################  End  ################################## #