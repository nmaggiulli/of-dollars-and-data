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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0240_bet_next_decade"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_months <- 120
aa_data <- read.csv(paste0(importdir, "/0240_bet_next_decade/alcoa_hist_data.csv")) %>%
  rename(value = `Adj.Close`) %>%
  mutate(date = as.Date(Date) + months(1) - days(1),
         symbol = "AA") %>%
  select(date, symbol, value)

raw <- import_ycharts_timeseries(paste0(importdir, "/0240_bet_next_decade/timeseries_5-1-2021.csv")) %>%
        select(date, symbol, value) %>%
        bind_rows(aa_data) %>%
        arrange(symbol, date) %>%
        mutate(total_ret = ifelse(lead(symbol, n_months) == symbol, lead(value, n_months)/value - 1, NA))
                        
all_dates <- sort(unique(raw$date))
start_dates <- all_dates[1:(length(all_dates) - n_months)]
end_dates <- all_dates[(1+n_months):length(all_dates)]

all_date_sims <- data.frame(start_date = start_dates,
                            end_date = end_dates,
                            join = 1) 

all_ind_stocks <- raw %>%
                    filter(symbol != "^SPXTR") %>%
                    select(symbol) %>%
                    distinct() %>%
                    rename(stock2 = symbol) %>%
                    mutate(join = 1)

all_sims <- all_ind_stocks %>%
                    rename(stock1 = stock2) %>%
                    left_join(all_ind_stocks) %>%
                    filter(stock1 != stock2, stock2 > stock1) %>%
                    left_join(all_date_sims) %>%
                    select(-join)

final_results <- data.frame()

for(i in 1:nrow(all_sims)){
  stock1 <- all_sims[i, "stock1"]
  stock2 <- all_sims[i, "stock2"]
  start_date <- all_sims[i, "start_date"]
  
  stock1_ret <-raw %>%
                  filter(date == start_date, symbol == stock1) %>%
                  pull(total_ret)
  
  stock2_ret <-raw %>%
                  filter(date == start_date, symbol == stock2) %>%
                  pull(total_ret)

  spxtr_ret <- raw %>%
                  filter(date == start_date, symbol == "^SPXTR") %>%
                  pull(total_ret)
  
  final_results[i, "start_date"] <- start_date
  final_results[i, "end_date"] <- all_sims[i, "end_date"]
  final_results[i, "stock1"] <- stock1
  final_results[i, "stock2"] <- stock2
  final_results[i, "ret_2stocks"] <- stock1_ret*0.5 + stock2_ret*0.5
  final_results[i, "ret_spxtr"] <- spxtr_ret
  
  if(i %% 1000 == 0){
    print(i)
  }
}

final_results <- final_results %>%
                    mutate(spxtr_winner = ifelse(ret_spxtr > ret_2stocks, 1, 0),
                           ret_spxtr_ann = (1 + ret_spxtr)^(1/(n_months/12)) - 1,
                           ret_2stocks_ann = (1 + ret_2stocks)^(1/(n_months/12)) - 1,
                           spxtr_op = ret_spxtr_ann - ret_2stocks_ann)

print(mean(final_results$spxtr_winner))


file_path <- paste0(out_path, "/spxtr_outperformance_vs_dow_sims.jpeg")
source_string <- "Source:  YCharts (OfDollarsAndData.com)"

to_plot <- final_results

plot <- ggplot(to_plot, aes(x=spxtr_op)) +
  geom_density(fill = chart_standard_color) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("Annualized S&P 500 Outperformance\nAgainst Dow 2-Stock Portfolio\nOver 10 Years")) +
  labs(x="S&P 500 Annualized Outperformance", y="Frequency",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #