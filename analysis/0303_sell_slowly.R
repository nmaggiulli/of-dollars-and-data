cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(readxl)
library(tidyverse)

folder_name <- "0303_sell_slowly"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

initial_portfolio <- 10^6
withdrawal_rate <- 0.04

df <- read.csv(paste0(importdir, "/0300_bond_stock_cpi/DFA_GrowthOfWealth_20220613095224.csv"), skip = 7,
               col.names = c("date", "index_bond", "index_sp500", "index_cpi")) %>%
            filter(!is.na(index_bond)) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y"),
                   index_sp500_real = index_sp500/index_cpi,
                   index_bond_real = index_bond/index_cpi,
                   cpi_rate = 100*(index_cpi/lag(index_cpi, 12) - 1),
                   ret_sp500_real = index_sp500_real/lag(index_sp500_real, 1) - 1) %>%
          select(date, index_sp500_real, ret_sp500_real) %>%
          filter(year(date) < 2022)

all_jans <- df %>%
  filter(month(date) == 1) %>%
  pull(date)

calc_jan_vs_qtr <- function(n_years){
  
  final_results <- data.frame()
  
  for(i in 1:(length(all_jans) - n_years + 1)){
    start_date <- all_jans[i]
    end_date <- start_date + years(n_years)

    tmp <- df %>%
              filter(date >= start_date, date < end_date)
    
    for(j in 1:nrow(tmp)){
      mt <- month(tmp[j, "date"])
      ret <- tmp[j, "ret_sp500_real"]
      
      if(j == 1){
        tmp[j, "annual_spend"] <- initial_portfolio*withdrawal_rate
        tmp[j, "port_jan"] <- initial_portfolio - tmp[j, "annual_spend"]
        tmp[j, "port_qtr"] <- initial_portfolio - tmp[j, "annual_spend"]/4
      } else{
        if(mt == 1){
          tmp[j, "annual_spend"] <- tmp[(j-1), "port_jan"]*withdrawal_rate
          
          tmp[j, "port_jan"] <- (tmp[(j-1), "port_jan"] - tmp[j, "annual_spend"]) * (1 + ret)
          tmp[j, "port_qtr"] <- (tmp[(j-1), "port_qtr"] - tmp[j, "annual_spend"]/4) * (1 + ret)
          
        } else if (mt == 4 | mt == 7 | mt == 10){
          tmp[j, "annual_spend"] <- tmp[(j-1), "annual_spend"]
          
          tmp[j, "port_jan"] <- tmp[(j-1), "port_jan"] * (1 + ret)
          tmp[j, "port_qtr"] <- (tmp[(j-1), "port_qtr"] - tmp[j, "annual_spend"]/4) * (1 + ret)
          
        } else{
          tmp[j, "annual_spend"] <- tmp[(j-1), "annual_spend"]
          
          tmp[j, "port_jan"] <- tmp[(j-1), "port_jan"] * (1 + ret)
          tmp[j, "port_qtr"] <- tmp[(j-1), "port_qtr"] * (1 + ret)
        }
      }
    }
    
    final_results[i, "start_date"] <- start_date
    final_results[i, "end_date"] <- end_date - months(1)
    final_results[i, "final_port_jan"] <- tmp[nrow(tmp), "port_jan"]
    final_results[i, "final_port_qtr"] <- tmp[nrow(tmp), "port_qtr"]
    final_results[i, "final_port_diff"] <- final_results[i, "final_port_qtr"]/final_results[i, "final_port_jan"] - 1
    final_results[i, "port_qtr_win"] <- ifelse(final_results[i, "final_port_diff"] > 0, 1, 0)
  }
  
  qtr_win <- final_results %>%
                              filter(port_qtr_win == 1) %>%
                              summarise(final_port_diff = (1+quantile(final_port_diff, probs = 0.5))^(1/n_years) - 1) %>%
                              pull(final_port_diff)
  
  jan_win <- final_results %>%
    filter(port_qtr_win == 0) %>%
    summarise(final_port_diff = (1+quantile(final_port_diff, probs = 0.5))^(1/n_years) - 1) %>%
    pull(final_port_diff)
  
  print(paste0("Quarterly withdrawals beat Jan withdrawals in ", 100*round(mean(final_results$port_qtr_win), 2), "% of ", n_years, "-year simulations."))
  print(paste0("Quarterly typically wins by ", 100*round(qtr_win, 4), "% on an annualized basis."))
  print(paste0("Jan-only typically wins by ", -100*round(jan_win, 4), "% on an annualized basis."))
  return(final_results)
}

years_to_run <- c(1, 5, 10, 20, 30)

for(y in years_to_run){
  t <- calc_jan_vs_qtr(y)
  
  if(y == 1){
    stacked_final <- t %>%
                        mutate(n_years = y)
  } else{
    stacked_final <- stacked_final %>%
      bind_rows(t %>% mutate(n_years = y))
  }
}

to_plot <- stacked_final %>%
                  group_by(n_years) %>%
                  summarise(port_qtr_win = mean(port_qtr_win)) %>%
                  ungroup()

text_labels <- to_plot %>%
  mutate(label = paste0(100*round(port_qtr_win, 2), "%"))

file_path <- paste0(out_path, "/sell_slowly_win_rate_", 100*withdrawal_rate, "_pct.jpg")
source_string <- paste0("Source: Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Assumes a portfolio invested in 100% U.S. stocks and a ", 100*withdrawal_rate, "% withdrawal rate.  ",
                               "All figures have include reinvested dividends and have been adjusted inflation."),
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=as.factor(n_years), y=port_qtr_win)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_hline(yintercept = 0.5, col = "black", linetype = "dashed") +
  geom_text(data=text_labels, aes(x=as.factor(n_years), y=port_qtr_win, label = label),
            col = chart_standard_color,
            vjust = -0.2,
            size = 3) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("How Often Quarterly Withdrawals Beat\nBeginning of Year Withdrawals")) +
  labs(x = "Years Invested", y = "Win Rate",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #