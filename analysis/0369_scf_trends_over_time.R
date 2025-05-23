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
library(xtable)
library(tidyverse)

folder_name <- "0369_scf_trends_over_time"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

df <- scf_stack %>%
      select(year, hh_id, imp_id, agecl, edcl,
             networth, income, asset, debt, homeeq, liq, fin, nfin, vehic,
             ccbal, install, resdbt,
             wgt) %>%
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
  
  if(quantile_prob == 0.5 & var == "networth"){
    assign("to_plot_nw_year_median", to_plot, envir = .GlobalEnv)
  }
  
  quantile_prob_string <- str_pad(100*quantile_prob, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/", var, "_", quantile_prob_string, "_by_year.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
  note_string <- paste0("Note: All figures are adjusted for inflation (2022 dollars).")
  
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

# create_time_series_chart("networth", "25th Percentile Real Net Worth", 0.25)
# create_time_series_chart("networth", "Real Median Net Worth", 0.5)
# create_time_series_chart("homeeq", "Real Median Home Equity", 0.5)
# create_time_series_chart("homeeq", "Real 75th Percentile Home Equity", 0.75)
# create_time_series_chart("networth", "75th Percentile Real Net Worth", 0.75)
create_time_series_chart("networth", "82nd Percentile Real Net Worth", 0.82)
create_time_series_chart("networth", "90th Percentile Real Net Worth", 0.9)
# create_time_series_chart("networth", "Real Average Net Worth", 0)
# create_time_series_chart("income", "Real Median Income", 0.5)
# create_time_series_chart("debt", "Real Median Debt", 0.5)
# create_time_series_chart("fin", "Real Median Financial Assets", 0.5)
# create_time_series_chart("nfin", "Real Median Non-Financial Assets", 0.5)
# create_time_series_chart("vehic", "Real Median Vehicle Value", 0.5)
# create_time_series_chart("asset", "Real Median Assets", 0.5)
# create_time_series_chart("networth", "99th Percentile Real Net Worth", 0.99)

#Plot all percentiles by age for given year
data_year <- 2022

df_year <- scf_stack %>%
  filter(year == data_year) %>%
  select(year, hh_id, imp_id, agecl, wgt, age,
         networth, homeeq) %>%
  arrange(year, hh_id, imp_id)

to_plot <- df_year %>%
  group_by(agecl) %>%
  summarise(
    value = wtd.quantile(networth, weights = wgt, probs=c(0.25, 0.5, 0.75, 0.9))
  ) %>%
  ungroup() %>%
  mutate(key = case_when(
    row_number() %% 4 == 0 ~ "90th Percentile",
    row_number() %% 4 == 3 ~ "75th Percentile",
    row_number() %% 4 == 2  ~ "50th Percentile",
    row_number() %% 4 == 1 ~ "25th Percentile",
    TRUE ~ "Error"
  ))

file_path <- paste0(out_path, "/2022_all_networth_percentiles_by_agecl.jpeg")
source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
note_string <- paste0("Note: All figures are in 2022 dollars.")

text_labels <- to_plot %>%
  mutate(label = case_when(
    value > 10^6 ~ paste0("$", formatC(round(value/1000000, 3), big.mark=",", format="f", digits=2), "M"),
    value > 0 ~ paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
    TRUE ~ paste0("$0"))
    )

max_y <- max(to_plot$value)

plot <- ggplot(to_plot, aes(x=agecl, y=value)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data=text_labels, aes(x=agecl, y=value, label = label),
            col = chart_standard_color,
            vjust = ifelse(text_labels$value >0, 0, 1),
            size = 1.8) +
  facet_rep_wrap(key ~ ., repeat.tick.labels = c("left", "bottom")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste0("Net Worth Percentiles by Age\n", data_year)) +
  labs(x="Age", y=paste0("Net Worth"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do average and median for table
wealth_table_by_age_html <- df_year %>%
  filter(age >= 20, age<=80) %>%
  mutate(agecl_new = case_when(age < 25 ~ "20-24",
                               age < 30 ~ "25-29",
                               age < 35 ~ "30-34",
                               age < 40 ~ "35-39",
                               age < 45 ~ "40-44",
                               age < 50 ~ "45-49",
                               age < 55 ~ "50-54",
                               age < 60 ~ "55-59",
                               age < 65 ~ "60-64",
                               age < 70 ~ "65-69",
                               age < 75 ~ "70-74",
                               TRUE ~ "75-80")) %>%
  group_by(agecl_new) %>%
  summarise(
    pct_50 = format_as_dollar(wtd.quantile(networth, weights = wgt, probs=c(0.5))),
    avg = format_as_dollar(wtd.mean(networth, weights = wgt))
  ) %>%
  ungroup() %>%
  select(agecl_new, avg, pct_50)

print(xtable(wealth_table_by_age_html), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/", data_year, "_wealth_by_agecl_table.html"))

#Now do home equity
homeeq_table_by_age_html <- df_year %>%
  filter(age >= 20, age<=80) %>%
  mutate(agecl_new = case_when(age < 25 ~ "20-24",
                               age < 30 ~ "25-29",
                               age < 35 ~ "30-34",
                               age < 40 ~ "35-39",
                               age < 45 ~ "40-44",
                               age < 50 ~ "45-49",
                               age < 55 ~ "50-54",
                               age < 60 ~ "55-59",
                               age < 65 ~ "60-64",
                               age < 70 ~ "65-69",
                               age < 75 ~ "70-74",
                               TRUE ~ "75-80")) %>%
  group_by(agecl_new) %>%
  summarise(
    pct_50 = format_as_dollar(wtd.quantile(homeeq, weights = wgt, probs=c(0.5))),
    avg = format_as_dollar(wtd.mean(homeeq, weights = wgt))
  ) %>%
  ungroup() %>%
  select(agecl_new, avg, pct_50)

print(xtable(homeeq_table_by_age_html), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/", data_year, "_homeeq_by_agecl_table.html"))

# ############################  End  ################################## #