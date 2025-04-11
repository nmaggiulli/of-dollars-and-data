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

folder_name <- "0459_scf_net_worth_over_time"
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

create_time_series_chart("networth", "Real Median Net Worth", 0.5)
create_time_series_chart("networth", "90th Percentile Real Net Worth", 0.9)

# Now do Wealth Level breakdown over time
# First, calculate net worth percentiles
find_percentile <- function(yr, amount, var, varname){
  
  if(amount < 10^6){
    p_change <- 0.001
    p_guess <- 0.5
    log_reduce <- 1
  } else if(amount < 10^7){
    p_change <- 0.001
    p_guess <- 0.8
    log_reduce <- 2
  } else if (amount < 10^8){
    p_change <- 0.001
    p_guess <- 0.97
    log_reduce <- 2
  } else{
    p_change <- 0.001
    p_guess <- 0.99
    log_reduce <- 2
  }
  
  solved <- 0
  p_guess_prior <- 0
  p_guess2_prior <- 1
  while(solved == 0){
    guess <- scf_stack %>%
      filter(year == yr) %>%
      rename(summary_col = !!sym(var)) %>%
      summarise(nw_percentile = wtd.quantile(summary_col, weights = wgt, probs = p_guess)) %>%
      pull(nw_percentile)
    
    diff_allowed <- 10^(floor(log10(amount) - log_reduce))
    
    if(p_guess2_prior == p_guess){
      solved <- 1
    }else if(guess - amount > diff_allowed){
      p_guess2_prior <- p_guess_prior
      p_guess_prior <- p_guess
      p_guess <- p_guess - p_change
    } else if (amount - guess > diff_allowed){
      p_guess2_prior <- p_guess_prior
      p_guess_prior <- p_guess
      p_guess <- p_guess + p_change
    } else{
      solved <- 1
    }
  }
  return(p_guess)
}

all_years <- unique(scf_stack$year)
all_wealth_levels <- c(10000, 10^5, 10^6, 10^7)

final_results <- data.frame()
counter <- 1
for(y in all_years){
  print(y)
  for(a in all_wealth_levels){
    print(a)
    tmp <- find_percentile(y, a, "networth", "Net Worth")
    
    final_results[counter, "year"] <- y
    final_results[counter, "wealth_level"] <- log10(a)-3
    final_results[counter, "pct"] <- tmp
    counter <- counter + 1
  }
}

to_plot <- final_results %>%
              mutate(wealth_level = case_when(
                wealth_level == 1 ~ "L1 (<$10k)",
                wealth_level == 2 ~ "L2 ($10k-$100k)",
                wealth_level == 3 ~ "L3 ($100k-$1M)",
                TRUE ~ "L4 ($1M-$10M)"
              ),
              pct = case_when(
                wealth_level == "L1 (<$10k)" ~ pct,
                TRUE ~ pct - lag(pct)
              ))

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/scf_wealth_percentiles_by_year.jpeg")
source_string <- paste0("Source:  Survey of Consumer Finances (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: All figures are adjusted for inflation to 2022 dollars."),
                        width = 85)

# Create the plot object
plot <- ggplot(to_plot, aes(x = year, y = pct, fill = as.factor(wealth_level))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#bdd7e7", "#6baed6", "#3182bd", "#08519c")) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(breaks = seq(1992, 2022, 6)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("U.S. Wealth Levels Over Time")) +
  labs(x = "Year", y = "Percentage of Households",
       caption = paste0(source_string, "\n", note_string))

# Save the plot  
ggsave(file_path, plot, width = 15, height = 12, units = "cm") 

export_to_excel(df = to_plot,
                outfile = paste0(out_path, "/scf_wealth_levels_by_year.xlsx"),
                sheetname = "scf",
                new_file = 1,
                fancy_formatting = 0)

# ############################  End  ################################## #