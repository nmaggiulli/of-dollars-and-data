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
library(tidyverse)

folder_name <- "0405_retire_inflation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Do all simulations using a 60/40 portfolio
s_weight <- 0.6

#Bring in raw data
raw <- read.csv(paste0(importdir, "/_fl/0014_discretionary_sims/GrowthOfWealth_20230206173453.csv"),
                skip = 7, 
                row.names = NULL,
                col.names = c("date", "index_bond",	"index_sp500", "cpi"))  %>%
  filter(!is.na(index_sp500)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         yr = year(date),
         change_in_cpi = cpi/lag(cpi, 12) - 1)

for(i in 1:nrow(raw)){
  if(i == 1){
    raw[i, "value_bond"] <- 1 - s_weight
    raw[i, "value_stock"] <- s_weight
    raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
    raw[i, "ret_port"] <- 0
  } else{
    mt <- month(raw[i, "date"])
    if(mt == 1){
      raw[i, "value_bond"] <- raw[(i-1), "value_port"] * (1 - s_weight) * (raw[i, "index_bond"]/raw[(i-1), "index_bond"])
      raw[i, "value_stock"] <- raw[(i-1), "value_port"] * s_weight * (raw[i, "index_sp500"]/raw[(i-1), "index_sp500"])
    } else{
      raw[i, "value_bond"] <- raw[(i-1), "value_bond"] * raw[i, "index_bond"]/raw[(i-1), "index_bond"]
      raw[i, "value_stock"] <- raw[(i-1), "value_stock"] * raw[i, "index_sp500"]/raw[(i-1), "index_sp500"]
    }
    raw[i, "value_port"] <- raw[i, "value_bond"] + raw[i, "value_stock"]
    raw[i, "ret_port"] <- raw[i, "value_port"]/raw[(i-1), "value_port"] - 1
  }
}

#Define inputs for inflation simulations
years_to_run <- data.frame(
  start_year = c("1947-01-01", "1979-01-01", "1990-01-01"),
  n_years <- rep(30, 3),
  withdrawal_rate <- rep(0.04, 3)
)

run_retirement_sim <- function(start_year, n_years, withdrawal_rate){
    end_year <- as.Date(start_year) + years(n_years)
    print(end_year)
  
    df <- raw %>%
      filter(date >= as.Date(start_year), date <= end_year) %>%
      select(date, change_in_cpi, ret_port)
    
    start_port <- 1* 10^6
    for(i in 1:nrow(df)){
      df[i, "year"] <- floor((i-1)/12) + 1
      ret_port <- df[i, "ret_port"]
      
      if(i == 1){
        current_year <- df[i, "yr"]
        
        required_spend <- start_port * withdrawal_rate
        monthly_spend <- required_spend/12
        
        df[i, "monthly_spend"] <- monthly_spend
        df[i, "port"] <- (start_port - monthly_spend) * (1 + ret_port)
      } else{
        mt <- month(df[i, "date"])
        
        if(mt == 1){
          current_year <- df[i, "yr"]
          
          change_in_cpi <- df[i, "change_in_cpi"]
          
          required_spend <- required_spend * (1 + change_in_cpi)
          monthly_spend <- required_spend/12
          
          df[i, "monthly_spend"] <- monthly_spend
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        } else{
          df[i, "monthly_spend"] <- monthly_spend
          df[i, "port"] <- (df[(i-1), "port"] - monthly_spend) * (1 + ret_port)
        }
      }
      if(df[i, "port"] < 0){
        df[i, "port"] <- 0
      }
    }
  return(df)
}

start_years <- c("1974-01-01",
                 "1964-01-01", 
                 "1954-01-01",
                 "1990-01-01")

periods_to_run <- data.frame(
  start_year = start_years,
  n_years = rep(30, length(start_years)),
  withdrawal_rate = rep(0.04, length(start_years)),
  label = c("Early (1974)", "Middle (1964)", "Late (1954)", "Low (1990)")
)

for(i in 1:nrow(periods_to_run)){
  s <- periods_to_run[i, "start_year"]
  n <- periods_to_run[i, "n_years"]
  w <- periods_to_run[i, "withdrawal_rate"]
  l <- periods_to_run[i, "label"]
  
  tmp <- run_retirement_sim(s, n, w) %>%
            mutate(start_year = year(s),
                   label = l)
  
  if(i == 1){
    df_stack <- tmp
  } else{
    df_stack <- df_stack %>% bind_rows(tmp)
  }
}

summary <- df_stack %>%
              group_by(label, year) %>%
              summarise(avg_spend = mean(monthly_spend*12),
                        avg_port = mean(port)) %>%
              ungroup()

# Plot spending
to_plot <- summary %>%
            select(year, label, avg_spend) %>%
            rename(key = label,
                   value = avg_spend)

#Plot
file_path <- paste0(out_path, "/retire_spending_by_inflation.jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Assumes a 4% withdrawal rate to determine your initial spending level which is adjusted by inflation thereafter.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=year, y=value, col = key)) +
  geom_line() +
  scale_color_discrete() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("Annual Retirement Spending\nby Inflation Regime")) +
  labs(x="Year", y="Annual Nominal Spending",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot the portfolio
to_plot <- summary %>%
  select(year, label, avg_port) %>%
  rename(key = label,
         value = avg_port)

#Plot
file_path <- paste0(out_path, "/retire_port_by_inflation.jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Assumes a 4% withdrawal rate to determine your initial spending level which is adjusted by inflation thereafter. ",
                                "Assumes all money is invested in a 60/40 U.S. Stock/Bond portfolio, rebalanced annually.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=year, y=value, col = key)) +
  geom_line() +
  scale_color_discrete() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("Retirement Portfolio Value\nby Inflation Regime")) +
  labs(x="Year", y="Nominal Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #