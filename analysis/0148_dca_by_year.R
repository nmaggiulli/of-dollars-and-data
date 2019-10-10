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

folder_name <- "0148_dca_by_year"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "0059_goyal_stock_bond_data.Rds")) %>%
          mutate(yr = year(date),
                 ret_sp500 = stock,
                 ret_bond = lt_bond) %>%
          filter(yr >= 1930) %>%
          select(date, ret_sp500, ret_bond, yr, cpi)

raw_by_year <- raw %>%
                group_by(yr) %>%
                summarize(ret_sp500 = prod(1+ret_sp500)-1,
                          ret_bond = prod(1+ret_bond)-1,
                          cpi = prod(1+cpi)-1) %>%
                ungroup() %>%
                mutate(ret_sp500 = ret_sp500 - cpi,
                       ret_bond = ret_bond - cpi)

n_years <- 30

value_add <- 1000

weight_sequence <- seq(0, 1, 0.1)

for(w_sp500 in weight_sequence){

  w_bond <- 1 - w_sp500  
    
  year_list <- raw_by_year %>%
                filter(yr %% 5 == 0, yr < 2019 - n_years) %>%
                pull(yr)
  
  for(y in year_list){
    
    df <- raw_by_year %>%
            filter(yr >= y, yr < y + n_years)
    
    for(i in 1:n_years){
      df[i, "year"] <- i
      df[i, "start_year"] <- y
      ret_sp500 <- df[i, "ret_sp500"]
      ret_bond <- df[i, "ret_bond"]
  
      if(i == 1){
        df[i, "value_port"] <- (value_add * w_sp500 * (1 + ret_sp500)) + (value_add * w_bond * (1 + ret_bond))
      } else{
        df[i, "value_port"] <- ((df[(i-1), "value_port"] + value_add) * w_sp500 * (1 + ret_sp500)) + ((df[(i-1), "value_port"] + value_add) * w_bond * (1 + ret_bond))
      }
    }
    
    if(y == year_list[1]){
      final_results <- df
    } else{
      final_results <- bind_rows(final_results, df)
    }
  }
  
  to_plot <- final_results %>%
                select(year, start_year, value_port)
  
  if(w_sp500 != 0 & w_sp500 != 1){
    weight_string <- paste0(100*w_sp500, "/", 100*w_bond, "(Stock/Bond)")
  } else if (w_sp500 == 0){
    weight_string <- paste0("All Bond")
  } else if (w_sp500 == 1){
    weight_string <- paste0("All Stock")
  }
  
  w_padded <- str_pad(100*w_sp500, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/sp500_", w_padded, "_pct_30yr_period_dca.jpeg")
  
  source_string <- str_wrap(paste0("Source:  Amit Goyal, http://www.hec.unil.ch/agoyal/ (OfDollarsAndData.com)"), 
                            width = 85)
  note_string <-  str_wrap(paste0("Note:  Assumes you invest $", formatC(value_add, big.mark = ",", format="f", digits = 0), " annually with rebalancing into a ", 
                                  100*w_sp500, "/", 100*w_bond,
                                  " (stock/bond) portfolio.  Adjusted for dividends and inflation."))
  
  plot <- ggplot(data = to_plot, aes(x=year, y = value_port, col = as.factor(start_year))) +
    geom_line() +
    geom_hline(yintercept = n_years*value_add, col = "black", linetype = "dashed") + 
    geom_text_repel(data = filter(to_plot, year == max(to_plot$year)),
                    aes(x = year, 
                        y = value_port,
                        col = as.factor(start_year),
                        label = round(start_year, digits=0),
                        family = "my_font"),
                    size = 2.5,
                    segment.colour = "transparent"
    ) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(label = dollar, limits = c(0, 70000)) +
    scale_x_continuous(limits = c(0, 30)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("30-Year DCA Results by Year For\n", weight_string, " Portfolio")) +
    labs(x = paste0("Year"), y = paste0("Portfolio Value"),
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_gif(out_path,
           paste0("sp500_*.jpeg"),
           100,
           0,
           paste0("_gif_dca_30yr_periods.gif"))

# ############################  End  ################################## #