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
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0447_inheritance_data_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Import UPenn data
# https://budgetmodel.wharton.upenn.edu/issues/2021/7/16/inheritances-by-age-and-income-group

raw <- read_excel(paste0(importdir, "/0447_inheritance_data/upenn_inheritance_data_scf.xlsx")) %>%
          clean_cols() %>%
          rename(`<26` = under_26) 

long <- raw %>%
          gather(-income_group, key=key, value=value)

pre_plot <- long %>%
              filter(income_group == "All Incomes" | key == "all_ages",
                     !(income_group == "All Incomes" & key == "all_ages"))

to_plot <- pre_plot %>%
            filter(income_group == "All Incomes")

## Add plot ##
file_path <- paste0(out_path, "/average_inheritance_by_age.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances, University of Pennsylvania")
note_string <- strwrap(paste0("Note: Figures include only individuals who received an inheritance."),
                       width = 80)

plot <- ggplot(to_plot, aes(x= as.factor(key), y = value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(label = dollar, limits = c(0, 200000)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Average Inheritance by Age Group")) +
  labs(x="Age Group", y="Total Inheritance",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


to_plot <- pre_plot %>%
  filter(key == "all_ages")

## Add plot ##
file_path <- paste0(out_path, "/average_inheritance_by_inc_percentile.jpeg")

plot <- ggplot(to_plot, aes(x= as.factor(income_group), y = value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Average Inheritance by Income Percentile")) +
  labs(x="Income Percentile", y="Total Inheritance",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

## Export heatmap data
for_heatmap <- raw %>%
                select(-all_ages) %>%
                filter(income_group != "All Incomes")

export_to_excel(df = for_heatmap,
                outfile = paste0(out_path, "/inheritance_data_for_heatmap.xlsx"),
                sheetname = "heatmap",
                new_file = 1,
                fancy_formatting = 0)

# ############################  End  ################################## #