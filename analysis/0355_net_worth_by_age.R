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

folder_name <- "0355_net_worth_by_age"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2019

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

df <- scf_stack %>%
      select(hh_id, imp_id, 
             networth, wgt, 
             agecl) %>%
      arrange(hh_id, imp_id)

n_hh <- length(unique(df$hh_id))

create_percentile_chart <- function(var, var_title, quantile_prob){

  if(quantile_prob != 0){
    to_plot <- df %>%
                      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                      group_by(agecl) %>%
                      summarise(
                         percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)
                        ) %>%
                      ungroup() %>%
                    gather(-agecl, key=key, value=value)
    
    percentile_var <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      summarise(percentile = wtd.quantile(var_for_qtile, weights = wgt, probs=quantile_prob)) %>%
      pull(percentile)
    
    quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
  } else{
    to_plot <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      group_by(agecl) %>%
      summarise(
        percentile = wtd.mean(var_for_qtile, weights = wgt)
      ) %>%
      ungroup() %>%
      gather(-agecl, key=key, value=value)
    
    percentile_var <- df %>%
      rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
      summarise(percentile = wtd.mean(var_for_qtile, weights = wgt)) %>%
      pull(percentile)
    
    quantile_prob_string <- "avg"
  }
  
  print(paste0("Overall ", var_title, " is: $", formatC(percentile_var, digits = 0, format = "f", big.mark = ",")))
  
  # Now do age only
      group_var <- "agecl"
      end_filename <- "age"
      x_var <- "Age"
  
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
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_", end_filename, "_", data_year, "_scf.jpeg")
    source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
    note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                    formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                    " U.S. households.")
                             , width = 85)
    
    text_labels <- to_plot %>%
      mutate(label = ifelse(value > 0, paste0("$", formatC(round(value/1000000, 2), big.mark=",", format="f", digits=1), "M"),
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

create_new_file <- 1
# create_percentile_chart("networth", "Average Net Worth", 0)
create_percentile_chart("networth", "95th Percentile Net Worth", 0.95)
create_percentile_chart("networth", "98th Percentile Net Worth", 0.98)
create_percentile_chart("networth", "99th Percentile Net Worth", 0.99)


# ############################  End  ################################## #