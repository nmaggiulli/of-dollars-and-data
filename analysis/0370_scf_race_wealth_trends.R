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

folder_name <- "0370_scf_race_wealth_trends"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

df <- scf_stack %>%
      filter(race %in% c("White", "Black"), year >= 1992) %>%
      select(year, hh_id, imp_id, race,
             nfin, fin,
             networth, income, debt, liq, wgt) %>%
      arrange(year, hh_id, imp_id)

year_min <- min(df$year)
year_max <- max(df$year)

create_time_series_chart <- function(var, var_title, quantile_prob){
  
  if(quantile_prob != 0){
    to_plot <- df %>%
              rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
              group_by(year, race) %>%
              summarise(
                 percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                ) %>%
              ungroup() %>%
            gather(-year, -race, key=key, value=value) %>%
            arrange(year, race) %>%
            mutate(wealth_gap = case_when(
              lag(race) != race ~ value/lag(value, 1),
              TRUE ~ NA
            )) %>%
            filter(race == "White") %>%
            select(-value) %>%
            rename(value = wealth_gap)
  } else{
    to_plot <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      group_by(year, race) %>%
      summarise(
        percentile = wtd.mean(var_for_qtile, weights = wgt)
      ) %>%
      ungroup() %>%
      gather(-year, -race, key=key, value=value) %>%
      arrange(year, race) %>%
      mutate(wealth_gap = case_when(
        lag(race) != race ~ value/lag(value, 1),
        TRUE ~ NA
        )) %>%
      filter(race == "White") %>%
      select(-value) %>%
      rename(value = wealth_gap)
  }
  
  quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_wb_gap_by_year.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
  note_string <- paste0("Note: All figures are in 2022 dollars.")
  
  plot <- ggplot(to_plot, aes(x=year, y=value)) +
    geom_line() +
    scale_y_continuous(label = comma) +
    scale_x_continuous(breaks = seq(year_min, year_max, 3), limits = c(year_min, year_max)) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("White ", var_title, " Over\nBlack ", var_title , "\nby Year")) +
    labs(x="Year", y=paste0("Ratio of White/Black\n", var_title),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_time_series_chart("networth", "25th Percentile Real Net Worth", 0.25)
create_time_series_chart("networth", "Real Median Net Worth", 0.5)
create_time_series_chart("networth", "75th Percentile Real Net Worth", 0.75)
create_time_series_chart("networth", "Real Average Net Worth", 0)

# ############################  End  ################################## #