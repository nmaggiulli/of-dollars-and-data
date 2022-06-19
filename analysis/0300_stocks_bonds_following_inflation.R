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

folder_name <- "0300_stocks_bonds_following_inflation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- read.csv(paste0(importdir, "/0300_bond_stock_cpi/DFA_GrowthOfWealth_20220613095224.csv"), skip = 7,
               col.names = c("date", "index_bond", "index_sp500", "index_cpi")) %>%
            filter(!is.na(index_bond)) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y"),
                   index_sp500_real = index_sp500/index_cpi,
                   index_bond_real = index_bond/index_cpi,
                   cpi_rate = 100*(index_cpi/lag(index_cpi, 12) - 1)) %>%
          select(date, index_sp500_real, index_bond_real, cpi_rate)

cpi_over_8 <- df %>%
                filter(cpi_rate > 8)

all_jans <- df %>%
              filter(month(date) == 1)

all_years <- all_jans %>%
              filter(year(date) <= 2019) %>%
              select(date) %>%
              pull(date)

for(i in 1:length(all_years)){
  i_year <- all_years[i]
  
  tmp <- df %>%
            filter(date >= i_year, date < i_year + years(3))
  
  first_sp500 <- tmp[1, "index_sp500_real"]
  first_bond <- tmp[1, "index_bond_real"]
  
  tmp2 <- tmp %>%
            mutate(index_sp500_real = index_sp500_real/first_sp500 - 1,
                   index_bond_real = index_bond_real/first_bond - 1,
                   index_60_40_real = 0.6*index_sp500_real+0.4*index_bond_real,
                   year_start = as.character(year(i_year)),
                   year = row_number()/12)
  
  if(i == 1){
    stacked <- tmp2
  } else{
    stacked <- bind_rows(stacked, tmp2)
  }
}

inflation_years <- c(1942, 1947, 1951, 1974, 1979)

all_yr_median <- stacked %>%
                group_by(year) %>%
                summarise(`S&P 500` = quantile(index_sp500_real, probs = 0.5),
                          `5YR Treasuries` = quantile(index_bond_real, probs = 0.5),
                  `60/40 Portfolio` = quantile(index_60_40_real, probs = 0.5)) %>%
                ungroup() %>%
                mutate(year_start = "Median (1926-2019)") %>%
                gather(-year_start, -year, key=key, value=value)

long <- stacked %>%
              filter(year_start %in% inflation_years) %>%
              select(-date) %>%
              rename(`S&P 500` = index_sp500_real,
                     `5YR Treasuries` = index_bond_real,
                     `60/40 Portfolio` = index_60_40_real) %>%
              gather(-year_start, -year, key=key, value=value)

plot_asset <- function(key_name, file_stub, title_string){
  to_plot <- long %>%
              filter(key == key_name) %>%
              select(year, year_start, key, value) 
  
  median <- to_plot %>%
            filter(year == max(to_plot$year)) %>%
            summarise(val = quantile(value, probs = 0.5)) %>%
            pull(val)
  
  print(median)
  
  to_plot <- to_plot %>%
    bind_rows(all_yr_median %>% filter(key == key_name))
  
  file_path <- paste0(out_path, "/pct_changes_", file_stub, ".jpeg")
  source_string <- paste0("Source: Returns 2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Figures include dividends and are adjusted for inflation."),
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x=year, y=value, col = year_start, size = year_start)) +
    geom_line() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(0, 3, 1)) +
    scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "black")) + 
    scale_size_manual(values = c(rep(0.5, 5), 1.5)) + 
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(paste0(title_string, " Performance\nFollowing Periods of High Inflation")) +
    labs(x = "Years", y = "Inflation-Adjusted Percentage Change",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_asset("60/40 Portfolio", "60_40", "60/40 Portfolio")
plot_asset("S&P 500", "sp500", "S&P 500")
plot_asset("5YR Treasuries", "bond", "5YR U.S. Treasury")
 

# ############################  End  ################################## #