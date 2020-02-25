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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0165_liquid_net_worth"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2016

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

df <- scf_stack %>%
      mutate(liquid_net_worth = fin - reteq - ccbal,
             homeeq_pct = homeeq/networth) %>%
      select(hh_id, imp_id, 
             networth, debt, fin, reteq, homeeq, homeeq_pct, ccbal, liquid_net_worth, wgt, 
             agecl, edcl) %>%
      arrange(hh_id, imp_id)

n_hh <- length(unique(df$hh_id))

create_percentile_chart <- function(var, var_title){

  to_plot <- df %>%
                    rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                    group_by(agecl, edcl) %>%
                    summarize(
                      `50th Percentile` = wtd.quantile(var_for_qtile, weights = wgt, probs=0.5)
                      ) %>%
                    ungroup() %>%
                  gather(-agecl, -edcl, key=key, value=value)
  
  file_path <- paste0(out_path, "/", var, "_age_educ_median.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data from ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                                  , width = 85)
  
  if(var != "homeeq_pct"){
  plot <- ggplot(to_plot, aes(x=edcl, y=value)) +
    geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
    facet_rep_wrap(agecl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Median ", var_title, "\nby Age & Education Level")) +
    labs(x="Education", y=paste0("Median ", var_title),
         caption = paste0(source_string, "\n", note_string))
  } else{
    plot <- ggplot(to_plot, aes(x=edcl, y=value)) +
      geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
      facet_rep_wrap(agecl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0("Median ", var_title, "\nby Age & Education Level")) +
      labs(x="Education", y=paste0("Median ", var_title),
           caption = paste0(source_string, "\n", note_string))
  }
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Now do age only
  to_plot <- df %>%
              rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
              group_by(agecl) %>%
              summarize(
                `50th Percentile` = wtd.quantile(var_for_qtile, weights = wgt, probs=0.5)
              ) %>%
              ungroup() %>%
              gather(-agecl, key=key, value=value)
  
  file_path <- paste0(out_path, "/", var, "_age_median.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                           , width = 85)
  
  text_labels <- to_plot %>%
    mutate(label = ifelse(value > 0, paste0("$", round(value/1000, 0), "k"),
                          paste0("$0")))
  
  nudge_y_calc <- max(text_labels$value)*0.02
  
  if(var != "homeeq_pct"){
  plot <- ggplot(to_plot, aes(x=agecl, y=value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text_repel(data=text_labels, aes(x=agecl, y=value, label = label),
                    col = chart_standard_color,
                    size = 3,
                    max.iter = 1,
                    nudge_y = nudge_y_calc) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Median ", var_title, "\nby Age")) +
    labs(x="Age", y=paste0("Median ", var_title),
         caption = paste0(source_string, "\n", note_string))
  } else{
    plot <- ggplot(to_plot, aes(x=agecl, y=value)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      scale_color_discrete(guide = FALSE) +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Median ", var_title, "\nby Age")) +
      labs(x="Age", y=paste0("Median ", var_title),
           caption = paste0(source_string, "\n", note_string))
  }
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_percentile_chart("networth", "Net Worth")
create_percentile_chart("liquid_net_worth", "Liquid Net Worth")
create_percentile_chart("ccbal", "Credit Card Balance")
create_percentile_chart("reteq", "Retirement Account Balance")
create_percentile_chart("homeeq", "Home Equity")
create_percentile_chart("homeeq_pct", "Home Equity Over Net Worth")
create_percentile_chart("fin", "Total Financial Assets")

# ############################  End  ################################## #