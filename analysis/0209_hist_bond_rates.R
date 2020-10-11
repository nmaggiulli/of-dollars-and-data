cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0209_hist_bond_rates"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bond_ret <- read.csv(paste0(importdir, "/0209_bond_rets/treasury_5yr.csv"), skip = 7,
                     col.names = c("date", "index_cpi", "index_bond")) %>%
            drop_na() %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
            mutate(date = as.Date(paste0(year(date), "-", month(date), "-01")),
                   index_bond_real = index_bond/index_cpi,
                   decade = paste0(as.character(year(floor_date(date, years(10))))))

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
            select(date, price_plus_div, long_irate)

n_months_ret <- 120

to_plot <- bond_ret %>%
        left_join(shiller) %>%
        mutate(ret = (lead(index_bond_real, n_months_ret)/index_bond_real)^(1/(n_months_ret/12)) - 1) %>%
        select(date, decade, long_irate, ret) %>%
        drop_na()

min_year <- year(min(bond_ret$date))
max_year <- year(max(bond_ret$date))

file_path <- paste0(out_path, "/yield_vs_ret_next_decade_bond.jpeg")
source_string <- paste0("Source:  Returns 2.0, Shiller Data, ", min_year, "-", max_year, " (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = long_irate, y = ret)) +
  geom_point(col = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 0.0079, linetype = "dashed") +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16), breaks = seq(0, 0.16, 0.04)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Starting Yield vs.\nReal Bond Return Over Next Decade")) +
  labs(x = "Starting Bond Yield" , y = "Annualized Real Return Over Next Decade",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do rebalance
bond_stock <- bond_ret %>%
              select(-index_bond) %>%
              left_join(shiller) %>%
              mutate(year = year(date),
                     month = month(date)) %>%
              rename(index_bond = index_bond_real,
                     index_sp500 = price_plus_div) %>%
              select(date, year, month, index_bond, index_sp500)

rebal_dates <- bond_stock %>%
  group_by(year, month) %>%
  summarize(min_date = min(date),
            rebal = 1) %>%
  ungroup() %>%
  filter(month == 1 | month == 7)

n_years <- 10

# Do rebalancing analysis
run_rebal <- function(wt_bond, start_year){
  
  df <- bond_stock %>%
    left_join(rebal_dates) %>%
    filter(year(date) >= start_year, year(date) <= (start_year + n_years - 1))
  
  first_sp500 <- df[1, "index_sp500"]
  
  for(i in 1:nrow(df)){
    if(i == 1){
      df[i, "value_stock"] <- 1 - wt_bond
      df[i, "value_bond"] <- wt_bond
    } else{
      rebal <- df[i, "rebal"]
      ret_sp500 <- df[i, "index_sp500"]/df[(i-1), "index_sp500"]
      ret_bond <- df[i, "index_bond"]/df[(i-1), "index_bond"]
      
      if(!is.na(rebal)){
        df[i, "value_stock"] <- df[(i-1), "value_rebal"] * (1 - wt_bond) * ret_sp500
        df[i, "value_bond"] <- df[(i-1), "value_rebal"] * wt_bond * ret_bond
      } else{
        df[i, "value_stock"] <- df[(i-1), "value_stock"] * (ret_sp500)
        df[i, "value_bond"] <- df[(i-1), "value_bond"] * (ret_bond)
      }
    }
    df[i, "value_rebal"] <- df[i, "value_stock"] + df[i, "value_bond"]
    df[i, "value_sp500"] <- df[i, "index_sp500"]/first_sp500
    
    df[i,"rebal_premium"] <- df[i, "value_rebal"]/df[i, "value_sp500"]
  }
  
  return(df[nrow(df), "rebal_premium"])
}

final_results <- data.frame()
wts <- seq(0.1, 0.3, 0.1)
yrs <- seq(year(min(bond_stock$date)), year(max(bond_stock$date))-n_years, 1)

counter <- 1
for(w in wts){
  print(w)
  for(y in yrs){
    final_results[counter, "year"] <- y
    final_results[counter, "bond_weight"] <- w
    final_results[counter, "rebal_premium"] <- run_rebal(w, y) - 1 
    
    counter <- counter + 1
  }
}

to_plot <- final_results %>%
              mutate(bond_weight = paste0(100*bond_weight, "% Bonds"))

file_path <- paste0(out_path, "/rebal_premium_vs_all_stock_", n_years, ".jpeg")
source_string <- paste0("Source:  Returns 2.0, Shiller Data, ", min_year, "-", max_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  Stocks are represented by the S&P 500 while ",
                               "bonds are represented by 5-Year Treasury Notes.  Performance includes dividends and is adjusted for inflation.  ",
                               "The rebalanced portfolio rebalances semi-annually in January and July."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "year"] <- 1970
text_labels[1, "rebal_premium"] <- 0.45
text_labels[1, "label"] <- "Rebalancing Outperforms"
text_labels[1, "bond_weight"] <- "10%"

text_labels[2, "year"] <- 1970
text_labels[2, "rebal_premium"] <- -0.45
text_labels[2, "label"] <- "All-Stock Outperforms"
text_labels[2, "bond_weight"] <- "10%"

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = rebal_premium, col = bond_weight)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=year, y=rebal_premium, label = label), 
            col = "black", 
            family = "my_font") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Rebalanced Stock/Bond vs. All-Stock Portfolio\nOver All ", n_years, "-Year Periods")) +
  labs(x = "Starting Year" , y = paste0("Rebalancing Premium After ", n_years, " Years"),
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #