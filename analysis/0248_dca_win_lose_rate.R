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
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0248_dca_win_lose_rate"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

period_length <- 120

df <- read.csv(paste0(importdir, "/0248_dca_win_lose_rate/dfa_sp500_bond_cpi.csv"), skip = 7,
                     col.names = c("date", "index_bond", "index_sp500", "index_cpi")) %>%
  drop_na() %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(date = as.Date(paste0(year(date), "-", month(date), "-01")),
         ret_bond = index_bond/lag(index_bond, 1) - 1,
         ret_sp500 = index_sp500/lag(index_sp500, 1) - 1) %>%
  drop_na %>%
  mutate(ret_10yr_sp500 = index_sp500/lag(index_sp500, period_length) - 1,
         sp500_10yr_pos = ifelse(ret_10yr_sp500 > 0, 1, 0))

print(paste0("10 year chance S&P 500 is positive: ", 100*round(mean(df$sp500_10yr_pos, na.rm = TRUE), 2), "%"))

start_months <- df[1:(nrow(df)-period_length+1), "date"]

run_dca <- function(start_month, end_month, w_stock){
  dca_amount <- 100
  tmp <- df %>%
            filter(date >= start_month, date <= end_month)
  
  for(j in 1:nrow(tmp)){
    if(j == 1){
      tmp[j, "cost_basis"] <- dca_amount
      tmp[j, "value_stock"] <- dca_amount * w_stock
      tmp[j, "value_bond"] <- dca_amount * (1- w_stock)
    } else{
      ret_stock <- 1 + tmp[j, "ret_sp500"]
      ret_bond  <- 1 + tmp[j, "ret_bond"]
      mt <- month(tmp[j, "date"])
        
      tmp[j, "cost_basis"] <- tmp[(j-1), "cost_basis"] + dca_amount
      
      if(mt == 12){
        tmp[j, "value_stock"] <- tmp[(j-1), "value_port"] * w_stock * ret_stock + (dca_amount * w_stock)
        tmp[j, "value_bond"] <-  tmp[(j-1), "value_port"] * (1- w_stock) * ret_bond + (dca_amount * (1- w_stock))
      }else{
        tmp[j, "value_stock"] <- tmp[(j-1), "value_stock"] * ret_stock + (dca_amount * w_stock)
        tmp[j, "value_bond"] <-  tmp[(j-1), "value_bond"] * ret_bond + (dca_amount * (1- w_stock))
      }
    }
    tmp[j, "value_port"] <- tmp[j, "value_stock"] + tmp[j, "value_bond"]
  }
  tail <- tmp %>%
            tail(1)
  
  return(tail)
}

final_results <- data.frame()

for(i in 1:length(start_months)){
  start_month <- start_months[i]
  end_month <- start_month + months(period_length-1)
  
  print(start_month)
  print(end_month)
  print("---")
  
  dca_stock <- run_dca(start_month, end_month, 1) %>%
                 select(cost_basis, value_port)
  dca_bond <- run_dca(start_month, end_month, 0)%>%
                pull(value_port)
  dca_6040 <- run_dca(start_month, end_month, 0.6)%>%
                pull(value_port)
  
  final_results[i, "start_date"] <- start_month
  final_results[i, "end_date"] <- end_month
  final_results[i, "cost_basis"] <- dca_stock$cost_basis
  final_results[i, "value_stock"] <- dca_stock$value_port
  final_results[i, "value_bond"] <- dca_bond
  final_results[i, "value_6040"] <- dca_6040

}

final_results <- final_results %>%
                  mutate(stock_beats_cash = ifelse(value_stock > cost_basis, 1, 0),
                         stock_beats_bonds = ifelse(value_stock > value_bond, 1, 0),
                         bonds_beats_cash = ifelse(value_bond > cost_basis, 1, 0),
                         p6040_beats_cash = ifelse(value_6040 > cost_basis, 1, 0),
                         stock_perf_premium = value_stock/cost_basis - 1,
                         bond_perf_premium = value_bond/cost_basis - 1)

print(paste0("Stock beats cash: ", 100*round(mean(final_results$stock_beats_cash), 2), "%"))
print(paste0("Stock beats bonds: ", 100*round(mean(final_results$stock_beats_bonds), 2), "%"))
print(paste0("Bonds beats cash: ", 100*round(mean(final_results$bonds_beats_cash), 2), "%"))
print(paste0("6040 beats cash: ", 100*round(mean(final_results$p6040_beats_cash), 2), "%"))

to_plot <- final_results %>%
            select(start_date, contains("premium")) %>%
            gather(-start_date, key=key, value=value) %>%
            mutate(key = case_when(
              key == "stock_perf_premium" ~ "S&P 500",
              key == "bond_perf_premium" ~ "100% Bonds",
              key == "p6040_pref_premium" ~ "60/40 Portfolio",
              TRUE ~ "Error"
            ))

file_path <- paste0(out_path, "/dca_perf_premium.jpeg")
source_string <- paste0("Source: Returns 2.0")

plot <- ggplot(to_plot, aes(x= value, fill = key)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("DCA Performance Premium Over 10 Years\nBy Portfolio")) +
  labs(x="Total Performance Premium Above Cash", y="Frequency",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #