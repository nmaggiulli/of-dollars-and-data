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
library(FinCal)
library(tidyverse)

folder_name <- "0149_dca_by_year"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "0059_goyal_stock_bond_data.Rds")) %>%
          mutate(yr = year(date),
                 ret_sp500 = stock_real,
                 ret_bond = lt_bond_real) %>%
          filter(yr >= 1930) %>%
          select(date, ret_sp500, ret_bond, yr, cpi)

raw_by_year <- raw %>%
                group_by(yr) %>%
                summarize(ret_sp500 = prod(1+ret_sp500)-1,
                          ret_bond = prod(1+ret_bond)-1,
                          cpi = prod(1+cpi)-1) %>%
                ungroup()

n_years <- 30

year_list <- raw_by_year %>%
  filter(yr %% 5 == 0, yr < 2019 - n_years) %>%
  pull(yr)

weight_sequence <- c(0, 0.6, 1)

for(y in year_list){
  
  df <- raw_by_year %>%
    filter(yr >= y, yr < y + n_years)
  
  for(w_sp500 in weight_sequence){
    print(w_sp500)

    w_bond <- 1 - w_sp500  
  
    #Initialize contributions
    value_add <- 1000
    total_contributions <- 0
    
    if(w_sp500 == 1){
      w_string <- "All Stocks"
    } else if (w_sp500 == 0){
      w_string <- "All Bonds"
    } else{
      w_string <- paste0(100*w_sp500, "% Stocks")
    }
    
    for(i in 1:n_years){
      df[i, "sim_year"] <- i
      df[i, "start_year"] <- y
      df[i, "w_sp500"] <- w_string
      ret_sp500 <- df[i, "ret_sp500"]
      ret_bond <- df[i, "ret_bond"]
      cpi <- pull(df[i, "cpi"])
  
      if(i == 1){
        df[i, "value_port"] <- (value_add * w_sp500 * (1 + ret_sp500)) + (value_add * w_bond * (1 + ret_bond))
        df[i, "total_contributions"] <- value_add
      } else{
        value_add <- value_add * (1 + cpi)
        df[i, "value_port"] <- ((df[(i-1), "value_port"] + value_add) * w_sp500 * (1 + ret_sp500)) + ((df[(i-1), "value_port"] + value_add) * w_bond * (1 + ret_bond))
        df[i, "total_contributions"] <- df[(i-1), "total_contributions"] + value_add
      }
    }
    
    if(w_sp500 == weight_sequence[1]){
      final_results <- df
    } else{
      final_results <- bind_rows(final_results, df)
    }
  }
  
  value_add <- 1000
  
  contributions <- final_results %>%
                      select(yr, sim_year, total_contributions) %>%
                      distinct %>%
                      arrange(sim_year) %>%
                      rename(value_port = total_contributions) %>%
                      mutate(w_sp500 = "Inflation")
  
  to_plot <- final_results %>%
                select(sim_year, w_sp500, value_port, start_year) %>%
                mutate(w_sp500 = as.factor(w_sp500),
                       yr = start_year + sim_year) %>%
                bind_rows(contributions)

  start_year_string <- y
  
  w_padded <- str_pad(100*w_sp500, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/sp500_", y, "_pct_30yr_period_dca.jpeg")
  
  source_string <- str_wrap(paste0("Source:  Amit Goyal, http://www.hec.unil.ch/agoyal/ (OfDollarsAndData.com)"), 
                            width = 85)
  note_string <-  str_wrap(paste0("Note:  Assumes you invest $", formatC(value_add, big.mark = ",", format="f", digits = 0), " a year into a ", 
                                  "stock/bond portfolio that is rebalanced annually.  Annual contribution amount grows with inflation.  ",  
                                  "Amounts shown are adjusted for dividends and inflation."))
  
  plot <- ggplot(data = to_plot, aes(x=sim_year, y = value_port, col = w_sp500)) +
    geom_line(aes(linetype = w_sp500)) +
    geom_text_repel(data = filter(to_plot, sim_year == max(to_plot$sim_year)),
                    aes(x = sim_year,
                        y = value_port,
                        col = w_sp500,
                        label = w_sp500,
                        family = "my_font"),
                    size = 2.5,
                    segment.colour = "transparent",
                    max.iter  = 1000
    ) +
    scale_color_manual(guide = FALSE, values = c("lightseagreen", "blue", "green", "black")) +
    scale_linetype_manual(guide = FALSE, values = c("solid", "solid", "solid", "dashed")) +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(limits = c(0, 30)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("30-Year DCA Results by Stock Weight\n", y)) +
    labs(x = paste0("Year"), y = paste0("Portfolio Value"),
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_gif(out_path,
            paste0("sp500_*.jpeg"),
            170,
            0,
            paste0("_gif_dca_30yr_periods.gif"))

# ############################  End  ################################## #