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

folder_name <- "_jkb/0012_net_worth_stats"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2019

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

df <- scf_stack %>%
      select(hh_id, imp_id, 
             networth, debt, mrthel, homeeq,
             wgt, 
             agecl, edcl) %>%
      mutate(eq_over_nw = homeeq/networth,
             non_mortgage_debt = debt - mrthel,
             mdebt_over_eq = mrthel/homeeq) %>%
      arrange(hh_id, imp_id)

max_num <- 1 * 10^6

homeeq_summary <- df %>%
            group_by(hh_id) %>%
            summarise(homeeq = mean(homeeq),
                      networth = mean(networth)) %>%
            ungroup() %>%
            mutate(nw_bucket = case_when(
              networth < 250000 ~ "<$250k",
              networth < 500000 ~ "$250k-$500k",
              networth < 1000000 ~ "$500k-$1M",
              networth < 5000000 ~ "$1M-$5M",
              TRUE ~ "$5M+"
            ),
                  homeeq_pct = homeeq/networth,
                   homeeq_dummy = ifelse(homeeq > 0, 1, 0))

homeeq_summary$nw_bucket <- factor(homeeq_summary$nw_bucket, levels = c("<$250k","$250k-$500k", "$500k-$1M",
                                                                        "$1M-$5M", "$5M+"
                                                                        ))

max_num <- 10 * 10^6
to_plot <- homeeq_summary %>%
  filter(networth < max_num, networth > 10000) %>%
  group_by(nw_bucket) %>%
  summarise(home_own_pct = mean(homeeq_dummy),
            homeeq_pct = mean(homeeq_pct)) %>%
  ungroup()

file_path <- paste0(out_path, "/homeeq_pct_vs_networth.jpeg")

plot <- ggplot(to_plot, aes(x=networth, y=homeeq_dummy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(label = dollar, limits = c(0, max_num*1.1), breaks = seq(0, max_num, max_num/4)) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle("Home Equity Percentage vs. Net Worth") +
  labs(x="Net Worth", y="Home Equity Percentage")

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

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
  
  print(paste0("Overall ", var_title, " is: $", formatC(percentile_var, digits = 0, format = "f", big.mark = ",")))
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_age_edc_comb_", data_year, ".jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Calculations based on weighted data from ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                                  , width = 85)
  
  assign(paste0("age_edc_", var, "_", quantile_prob_string), to_plot, envir = .GlobalEnv)
  
  export_to_excel(to_plot %>%
                    mutate(value = paste0("$", formatC(value, big.mark = ",", format="f", digits = 0))), 
                  paste0(out_path, "/all_var_summaries.xlsx"), 
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
    geom_bar(stat = "identity", position = "dodge", fill = "black") +
    facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
    # geom_text(data = text_labels, aes(x=agecl, y=value, label = label),
    #                 col = chart_standard_color,
    #                 size = 1.8,
    #                 vjust= ifelse(text_labels$value >0, 0, 1)) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0(var_title, "\nby Age & Education Level")) +
    labs(x="Age", y=paste0(var_title))
  
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
    
    file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_", end_filename, ".jpeg")
    
    text_labels <- to_plot %>%
      mutate(label = ifelse(value > 0, paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
                            paste0("$0")))
    
    plot <- ggplot(to_plot, aes(x=group_var, y=value)) +
      geom_bar(stat = "identity", fill = "black") +
      geom_text(data=text_labels, aes(x=group_var, y=value, label = label),
                      col = chart_standard_color,
                      vjust = -0.2,
                      size = 3) +
      scale_color_discrete(guide = FALSE) +
      scale_y_continuous(label = dollar) +
      of_dollars_and_data_theme +
      ggtitle(paste0(var_title, "\nby ", x_var)) +
      labs(x=x_var, y=paste0(var_title))
    
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

create_new_file <- 1
create_percentile_chart("debt", "50th Percentile Debt", 0.5)
create_percentile_chart("mrthel", "50th Percentile Mortgage Debt", 0.5)
create_percentile_chart("homeeq", "50th Percentile Home Equity", 0.5)
create_percentile_chart("eq_over_nw", "50th Percentile Equity Over Net Worth", 0.50)
create_percentile_chart("non_mortgage_debt", "50th Percentile Non-Mortgage Debt", 0.50)
create_percentile_chart("mdebt_over_eq", "50th Percentile Mortage Debt/Equity", 0.50)



# ############################  End  ################################## #