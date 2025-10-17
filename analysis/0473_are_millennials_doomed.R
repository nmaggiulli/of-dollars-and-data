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

folder_name <- "0473_are_millennials_doomed"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

my_colors <- c("#d7191c", "#fdae61", "black", "#2c7bb6", "#abd9e9")

#Generation assumptions
# Millennials born in 1981-1996

# Bring in SCF data
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              mutate(age_26_41 = case_when(
                age >= 26 & age <= 41 ~ 1,
                TRUE ~ 0
              ))
              
cohort_26_41 <- scf_stack %>%
                  filter(age_26_41 == 1)

max_year <- max(scf_stack$year)

nw_stats <- cohort_26_41 %>%
            group_by(year) %>%
            summarise(`90th Percentile` = wtd.quantile(networth, probs = 0.9, weights = wgt),
                      `75th Percentile` = wtd.quantile(networth, probs = 0.75, weights = wgt),
                      `50th Percentile` = wtd.quantile(networth, probs = 0.5, weights = wgt),
                      `25th Percentile` = wtd.quantile(networth, probs = 0.25, weights = wgt),
                      `15th Percentile` = wtd.quantile(networth, probs = 0.15, weights = wgt)) %>%
            ungroup() %>%
            gather(-year, key=key, value=value)

plot_percentiles <- function(grepl_string, pct_file_string, percentile_text){
  
  if(grepl_string == "50th|75th|90th"){
    tmp_colors <- rev(my_colors[1:3])
  } else{
    tmp_colors <- rev(my_colors[3:5])
  }
  
  to_plot <- nw_stats %>%
                filter(grepl(grepl_string, key))
  
  file_path <- paste0(out_path, "/millennial_cohort_net_worth_by_year_", pct_file_string, "_", max_year, ".jpeg")
  source_string <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"
  
  plot <- ggplot(to_plot, aes(x=year, y = value, col = key)) +
    geom_line() +
    scale_color_manual(values = tmp_colors) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Inflation-Adjusted Net Worth By Year\nHouseholds Age 26-41\n", percentile_text)) +
    labs(x="Year", y="Inflation-Adjusted Net Worth",
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_percentiles("50th|75th|90th", "50_90", "50th-90th Percentile")
plot_percentiles("15th|25th|50th", "15_50", "15th-50th Percentile")

# ############################  End  ################################## #