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

folder_name <- "0369_scf_trends_over_time"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

df <- scf_stack %>%
      select(year, hh_id, imp_id, agecl, edcl,
             networth, income, debt, homeeq, liq, wgt) %>%
      arrange(year, hh_id, imp_id)

year_min <- min(df$year)
year_max <- max(df$year)

create_time_series_chart <- function(var, var_title, quantile_prob){
  
  if(quantile_prob != 0){
    to_plot <- df %>%
              rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
              group_by(year) %>%
              summarise(
                 percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                ) %>%
              ungroup() %>%
            gather(-year, key=key, value=value)
  } else{
    to_plot <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      group_by(year) %>%
      summarise(
        percentile = wtd.mean(var_for_qtile, weights = wgt)
      ) %>%
      ungroup() %>%
      gather(-year, key=key, value=value)
  }
  
  quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_by_year.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
  note_string <- paste0("Note: All figures are in 2022 dollars.")
  
  plot <- ggplot(to_plot, aes(x=year, y=value)) +
    geom_line() +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(breaks = seq(year_min, year_max, 3), limits = c(year_min, year_max)) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0(var_title, "\nby Year")) +
    labs(x="Year", y=paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  #Now do by Age class
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
        group_by(year, group_var) %>%
        summarise(
          percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
        ) %>%
        ungroup() %>%
        gather(-year, -group_var, key=key, value=value)
    } else{
      to_plot <- df %>%
        rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
        rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
        group_by(year, group_var) %>%
        summarise(
          percentile = wtd.mean(var_for_qtile, weights = wgt)
        ) %>%
        ungroup() %>%
        gather(-year, -group_var, key=key, value=value)
    }
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_by_year_", end_filename, ".jpeg")
  
    plot <- ggplot(to_plot, aes(x=year, y=value)) +
      geom_line() +
      facet_rep_wrap(group_var ~ ., repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0(var_title, "\nby Year & ", x_var)) +
      labs(x="Year", y=paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
    if(group_var == "agecl"){
      for(i in 1:2){
        if(i == 1){
          agecl_filter <- "<35"
          agecl_name <- "under_35"
        } else{
          agecl_filter <- "35-44"
          agecl_name <- "35_to_44"
        }
        if(quantile_prob != 0){
          to_plot <- df %>%
            filter(agecl == agecl_filter) %>%
            rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
            rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
            group_by(year) %>%
            summarise(
              percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
            ) %>%
            ungroup() %>%
            gather(-year, key=key, value=value)
        } else{
          to_plot <- df %>%
            filter(agecl == "<35") %>%
            rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
            rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
            group_by(year) %>%
            summarise(
              percentile = wtd.mean(var_for_qtile, weights = wgt)
            ) %>%
            ungroup() %>%
            gather(-year, key=key, value=value)
        }
        
        file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_", agecl_name, "_by_year.jpeg")
        
        plot <- ggplot(to_plot, aes(x=year, y=value)) +
          geom_line() +
          scale_y_continuous(label = dollar) +
          scale_x_continuous(breaks = seq(year_min, year_max, 3), limits = c(year_min, year_max)) +
          of_dollars_and_data_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle(paste0(var_title, " by Year\nFor Households ", agecl_filter)) +
          labs(x="Year", y=paste0(var_title),
               caption = paste0(source_string, "\n", note_string))
        
        # Save the plot
        ggsave(file_path, plot, width = 15, height = 12, units = "cm")
      }
    }
  }
}


create_time_series_chart("networth", "25th Percentile Real Net Worth", 0.25)
create_time_series_chart("networth", "Real Median Net Worth", 0.5)
create_time_series_chart("networth", "75th Percentile Real Net Worth", 0.75)
create_time_series_chart("networth", "Real Average Net Worth", 0)
create_time_series_chart("income", "Real Median Income", 0.5)
create_time_series_chart("debt", "Real Median Debt", 0.5)
create_time_series_chart("liq", "Real Median Liquid Net Worth", 0.5)
create_time_series_chart("homeeq", "Real Median Home Equity", 0.5)


# ############################  End  ################################## #