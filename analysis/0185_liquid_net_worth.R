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

folder_name <- "0185_liquid_net_worth"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2022

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

df <- scf_stack %>%
      mutate(liquid_assets = asset - reteq - nfin,
        liquid_networth = liquid_assets - debt,
             homeeq_pct = homeeq/networth) %>%
      select(hh_id, imp_id, 
             networth,
             homeeq, 
             liquid_assets,
             liquid_networth, 
             homeeq_pct,
             fin,
             debt,
             wgt, 
             agecl, edcl) %>%
      arrange(hh_id, imp_id)

n_hh <- length(unique(df$hh_id))

create_percentile_chart <- function(var, var_title, quantile_prob){

  if(quantile_prob != 0){
    to_plot <- df %>%
                      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                      group_by(edcl, agecl) %>%
                      summarise(
                         percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                        ) %>%
                      ungroup() %>%
                    gather(-edcl, -agecl, key=key, value=value)
    
    percentile_var <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      summarise(percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)) %>%
      pull(percentile)
    
    quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
  } else{
    to_plot <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      group_by(edcl, agecl) %>%
      summarise(
        percentile = wtd.mean(var_for_qtile, weights = wgt)
      ) %>%
      ungroup() %>%
      gather(-edcl, -agecl, key=key, value=value)
    
    percentile_var <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      summarise(percentile = wtd.mean(var_for_qtile, weights = wgt)) %>%
      pull(percentile)
    
    quantile_prob_string <- "avg"
  }
  
  print(paste0("Overall ", var_title, " is: $", formatC(percentile_var, digits = 2, format = "f", big.mark = ",")))
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_age_edc_comb_", data_year, ".jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Calculations based on weighted data from ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                                  , width = 85)
  
  assign(paste0("age_edc_", var, "_", quantile_prob_string), to_plot, envir = .GlobalEnv)
  
  export_to_excel(to_plot %>%
                    mutate(value = paste0("$", formatC(value, big.mark = ",", format="f", digits = 0))), 
                  paste0(out_path, "/all_var_summaries_", data_year, ".xlsx"), 
                  paste0("age_edc_", var, "_", quantile_prob_string),
                  create_new_file,
                  0)
  
  if (create_new_file == 1){
    assign("create_new_file", 0, envir = .GlobalEnv)
  } 
  
  text_labels <- to_plot %>%
    mutate(label = ifelse(abs(value) > 100, paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
                          paste0("$0")))
  
  plot <- ggplot(to_plot, aes(x=agecl, y=value)) +
    geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
    facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
    geom_text(data = text_labels, aes(x=agecl, y=value, label = label),
                    col = chart_standard_color,
                    size = 1.8,
                    vjust= ifelse(text_labels$value >0, 0, 1)) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0(var_title, "\nby Age & Education Level")) +
    labs(x="Age", y=paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Now do age only and then education only
  for(i in 1:2){
    if(i == 1){
      group_var <- "agecl"
      end_filename <- "age"
      x_var <- "Age"
    } else{
      group_var <- "edcl"
      end_filename <- "edc"
      x_var <- "Education Level"
    }
  
    if(quantile_prob != 0){
      to_plot <- df %>%
                  rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                  rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
                  group_by(group_var) %>%
                  summarise(
                    percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                  ) %>%
                  ungroup() %>%
                  gather(-group_var, key=key, value=value)
    } else{
      to_plot <- df %>%
        rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
        rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
        group_by(group_var) %>%
        summarise(
          percentile = wtd.mean(var_for_qtile, weights = wgt)
        ) %>%
        ungroup() %>%
        gather(-group_var, key=key, value=value)
    }
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_", end_filename, "_", data_year, ".jpeg")
    source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
    note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                    formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                    " U.S. households.")
                             , width = 85)
    
    text_labels <- to_plot %>%
      mutate(label = ifelse(value > 0, paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
                            paste0("$0")))
    
    plot <- ggplot(to_plot, aes(x=group_var, y=value)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      geom_text(data=text_labels, aes(x=group_var, y=value, label = label),
                      col = chart_standard_color,
                      vjust = -0.2,
                      size = 3) +
      scale_color_discrete(guide = FALSE) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      ggtitle(paste0(var_title, "\nby ", x_var)) +
      labs(x=x_var, y=paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

create_new_file <- 1
create_percentile_chart("liquid_networth", "Median Liquid Net Worth", 0.5)
create_percentile_chart("liquid_networth", "75th Percentile Liquid Net Worth", 0.75)
create_percentile_chart("liquid_networth", "90th Percentile Liquid Net Worth", 0.9)
create_percentile_chart("liquid_assets", "25th Percentile Liquid Assets", 0.25)
create_percentile_chart("liquid_assets", "90th Percentile Liquid Assets", 0.9)
create_percentile_chart("liquid_assets", "Median Liquid Assets", 0.5)
create_percentile_chart("homeeq", "Median Home Equity", 0.5)


# ############################  End  ################################## #