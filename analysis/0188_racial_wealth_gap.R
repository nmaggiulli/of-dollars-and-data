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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(ggrepel)
library(tidyverse)

folder_name <- "0188_racial_wealth_gap"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2016

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

income_quartiles <- scf_stack %>%
  summarise(
    inc_25 = wtd.quantile(income, weights = wgt, probs=0.25),
    inc_50 = wtd.quantile(income, weights = wgt, probs=0.5),
    inc_75 = wtd.quantile(income, weights = wgt, probs=0.75)
  )

df <- scf_stack %>%
      select(hh_id, imp_id, 
             networth, income, wgt, 
             race, agecl, edcl) %>%
      arrange(hh_id, imp_id) %>%
      mutate(inccl = case_when(income < 25000 ~ "Income (<$25k)",
                                income < 50000 ~ "Income ($25k-$50k)",
                                income < 100000 ~ "Income ($50k-$100k)",
                                TRUE ~ "Income ($100k+)"))

df$inccl <- factor(df$inccl,levels = c("Income (<$25k)", 
                                       "Income ($25k-$50k)", 
                                       "Income ($50k-$100k)", 
                                       "Income ($100k+)"))

n_hh <- length(unique(df$hh_id))

create_percentile_chart <- function(var, var_title, quantile_prob){
  
  for(c in 1:3){
    if(c == 1){
      class_var <- "edcl"
    } else if(c == 2){
      class_var <- "agecl"
    } else if (c == 3){
      class_var <- "inccl"
    }
    
    group_list <- c("race", class_var)
    
    if(quantile_prob != 0){
      to_plot <- df %>%
                  rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                  group_by_(.dots = group_list) %>%
                  summarise(
                     value = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                    ) %>%
                  ungroup()
      
      percentile_var <- df %>%
        rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
        summarise(percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)) %>%
        pull(percentile)
      
      quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
    } else{
      to_plot <- df %>%
        rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
        group_by(.dots = group_list) %>%
        summarise(
          value = wtd.mean(var_for_qtile, weights = wgt)
        ) %>%
        ungroup()
      
      percentile_var <- df %>%
        rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
        summarise(percentile = wtd.mean(var_for_qtile, weights = wgt)) %>%
        pull(percentile)
      
      quantile_prob_string <- "avg"
    }
    
    if(class_var == "edcl"){
      print(paste0("Overall ", var_title, " is: $", formatC(percentile_var, digits = 0, format = "f", big.mark = ",")))
    }
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_race_", class_var, "_grid.jpeg")
    source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
    note_string <-  str_wrap(paste0("Note:  Calculations based on weighted data from ", 
                                    formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                    " U.S. households.")
                                    , width = 85)
    
    assign(paste0("race_", class_var, "_", var, "_", quantile_prob_string), to_plot, envir = .GlobalEnv)
    
    export_to_excel(to_plot %>%
                      mutate(value = paste0("$", formatC(value, big.mark = ",", format="f", digits = 0))), 
                    paste0(out_path, "/all_var_summaries.xlsx"), 
                    paste0("race_", class_var, "_", var, "_", quantile_prob_string),
                    create_new_file,
                    0)
    
    if (create_new_file == 1){
      assign("create_new_file", 0, envir = .GlobalEnv)
    } 
    
    text_labels <- to_plot %>%
      mutate(label = ifelse(abs(value) > 100, paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
                            paste0("$0")))
    
    if(class_var == "edcl"){
      plot <- ggplot(to_plot, aes(x=race, y=value)) +
        geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
        facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        geom_text(data = text_labels, aes(x=race, y=value, label = label),
                  col = "white",
                  size = 1.8,
                  vjust= ifelse(text_labels$value >0, 1, 0)) +
        scale_y_continuous(label = dollar) +
        of_dollars_and_data_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle(paste0(var_title, "\nby Race & Education Level")) +
        labs(x="Race", y=paste0(var_title),
             caption = paste0(source_string, "\n", note_string))
    } else if (class_var == "agecl"){
      plot <- ggplot(to_plot, aes(x=race, y=value)) +
        geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
        facet_rep_wrap(agecl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        geom_text(data = text_labels, aes(x=race, y=value, label = label),
                  col = "white",
                  size = 1.8,
                  vjust= ifelse(text_labels$value >0, 1, 0)) +
        scale_y_continuous(label = dollar) +
        of_dollars_and_data_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle(paste0(var_title, "\nby Race & Age")) +
        labs(x="Race", y=paste0(var_title),
             caption = paste0(source_string, "\n", note_string))
    } else if (class_var == "inccl"){
      plot <- ggplot(to_plot, aes(x=race, y=value)) +
        geom_bar(stat = "identity", position = "dodge", fill = chart_standard_color) +
        facet_rep_wrap(inccl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        geom_text(data = text_labels, aes(x=race, y=value, label = label),
                  col = "white",
                  size = 1.8,
                  vjust= ifelse(text_labels$value >0, 1, 0)) +
        scale_y_continuous(label = dollar) +
        of_dollars_and_data_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle(paste0(var_title, "\nby Race & Income Quartile")) +
        labs(x="Race", y=paste0(var_title),
             caption = paste0(source_string, "\n", note_string))
    }
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
  
  # Do Race only
  group_var <- "race"
  end_filename <- "race"
  x_var <- "Race"

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
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_", end_filename, ".jpeg")
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
  
  if(var_title == "Median Net Worth"){
    to_plot <- df %>%
      filter(agecl == "<35") %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      rename_(.dots = setNames(paste0(group_var), "group_var")) %>%
      group_by(group_var) %>%
      summarise(
        percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
      ) %>%
      ungroup() %>%
      gather(-group_var, key=key, value=value)
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_under35_", end_filename, ".jpeg")
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
      ggtitle(paste0(var_title, "\nby ", x_var, " For Households <35")) +
      labs(x=x_var, y=paste0(var_title),
           caption = paste0(source_string, "\n", note_string))
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

create_new_file <- 1
create_percentile_chart("networth", "Average Net Worth", 0)
create_percentile_chart("networth", "Median Net Worth", 0.5)
create_percentile_chart("networth", "75th Percentile Net Worth", 0.75)
create_percentile_chart("networth", "80th Percentile Net Worth", 0.80)
create_percentile_chart("networth", "25th Percentile Net Worth", 0.25)
create_percentile_chart("networth", "90th Percentile Net Worth", 0.9)


# ############################  End  ################################## #