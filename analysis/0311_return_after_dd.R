cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0311_return_after_dd"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_1926 <- read.csv(paste0(importdir, "/0310_sp500_acwi/GrowthOfWealth_20220823141917.csv"),
                     skip = 7, 
                     row.names = NULL,
                     col.names = c("date", "index_sp500")) %>%
                    filter(!is.na(index_sp500)) %>%
                  mutate(date = as.Date(date, format = "%m/%d/%Y"))

raw_1988 <- read.csv(paste0(importdir, "/0310_sp500_acwi/GrowthOfWealth_20220823122405.csv"),
                skip = 7, 
                row.names = NULL,
                col.names = c("date", "index_acwi", "index_sp500"))  %>%
          select(date, index_acwi) %>%
          filter(!is.na(index_acwi)) %>%
          mutate(date = as.Date(date, format = "%m/%d/%Y"))
                 
raw <- raw_1926 %>%
          left_join(raw_1988) %>%
          mutate(
                 ret_acwi_1yr = lead(index_acwi, 12)/index_acwi - 1,
                 ret_acwi_3yr = (lead(index_acwi, 36)/index_acwi)^(1/3) - 1,
                 ret_acwi_5yr = (lead(index_acwi, 60)/index_acwi)^(1/5) - 1,
                 ret_sp500_1yr = lead(index_sp500, 12)/index_sp500 - 1,
                 ret_sp500_3yr = (lead(index_sp500, 36)/index_sp500)^(1/3) - 1,
                 ret_sp500_5yr = (lead(index_sp500, 60)/index_sp500)^(1/5) - 1,
                 )

dd_sp500 <- drawdown_path(select(raw, date, index_sp500)) %>%
              add_dd_counter() %>%
              rename(dd_sp500 = pct,
                     dd_counter_sp500 = dd_counter)

dd_acwi <- drawdown_path(select(raw, date, index_acwi) %>% drop_na()) %>%
  add_dd_counter() %>%
  rename(dd_acwi = pct,
         dd_counter_acwi = dd_counter)

df <- raw %>%
        left_join(dd_sp500) %>%
        left_join(dd_acwi)

dd_thresholds <- c(0, -0.2, -0.3, -0.4)

final_results <- data.frame()
counter <- 1
for(d in dd_thresholds){
  tmp <- filter(df, dd_sp500 <= d) %>%
            summarise(n_obs = n(),
                      min_year = min(year(date)),
                      median_ret_1yr = quantile(ret_sp500_1yr, probs = 0.5, na.rm = TRUE),
                      median_ret_3yr = quantile(ret_sp500_3yr, probs = 0.5, na.rm = TRUE),
                      median_ret_5yr = quantile(ret_sp500_5yr, probs = 0.5, na.rm = TRUE))
  
  final_results[counter, "dd_level"] <- d
  final_results[counter, "n_months_sp500"] <- tmp$n_obs
  final_results[counter, "ret_sp500_1yr"] <- tmp$median_ret_1yr
  final_results[counter, "ret_sp500_3yr"] <- tmp$median_ret_3yr
  final_results[counter, "ret_sp500_5yr"] <- tmp$median_ret_5yr
  final_results[counter, "sp500_year"] <- tmp$min_year
  
  tmp <- filter(df, dd_acwi <= d) %>%
    drop_na() %>%
    summarise(n_obs = n(),
              min_year = min(year(date)),
              median_ret_1yr = quantile(ret_acwi_1yr, probs = 0.5),
              median_ret_3yr = quantile(ret_acwi_3yr, probs = 0.5),
              median_ret_5yr = quantile(ret_acwi_5yr, probs = 0.5))
  
  final_results[counter, "n_months_acwi"] <- tmp$n_obs
  final_results[counter, "ret_acwi_1yr"] <- tmp$median_ret_1yr
  final_results[counter, "ret_acwi_3yr"] <- tmp$median_ret_3yr
  final_results[counter, "ret_acwi_5yr"] <- tmp$median_ret_5yr
  final_results[counter, "acwi_year"] <- tmp$min_year
  
  counter <- counter + 1
}

plot_index <- function(ret_name, proper_name){
  
  ret_var <- paste0("ret_", ret_name)
  
  to_plot <- final_results %>%
                select(dd_level, contains(ret_var)) %>%
                gather(-dd_level, key=key, value = value) %>%
                mutate(key = case_when(
                  grepl("1yr", key) ~ "1-Year",
                  grepl("3yr", key) ~ "3-Year",
                  grepl("5yr", key) ~ "5-Year",
                  TRUE ~ "Error"
                ),
                dd_level = case_when(
                  dd_level == 0 ~ "All Months",
                  dd_level == -0.2 ~ ">20% Drawdown",
                  dd_level == -0.3 ~ ">30% Drawdown",
                  dd_level == -0.4 ~ ">40% Drawdown"
                )
                )
  
  to_plot$dd_level <- factor(to_plot$dd_level, 
                             levels = c("All Months", ">20% Drawdown", ">30% Drawdown", ">40% Drawdown"))
  
  year_var <- paste0(ret_name, "_year")
  
  min_year <- final_results[1, year_var]
  max_year <- max(year(raw$date))
  
  file_path <- paste0(out_path, "/", ret_name, "_ret_after_dd.jpg")
  source_string <- paste0("Source: Returns2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Includes dividends but does not adjust for inflation."), 
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=key, y=value, fill = dd_level)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#fee5d9", "#fcae91", "#fb6a4a", "#cb181d")) +
    scale_y_continuous(label = percent_format(accuracy = 0.1)) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0(proper_name, "\nFuture Returns by Drawdown Level\n", min_year, "-", max_year)) +
    labs(x = "Time Horizon", y = "Annualized Return",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_index("sp500", "S&P 500")
plot_index("acwi", "ACWI ex US")

# ############################  End  ################################## #