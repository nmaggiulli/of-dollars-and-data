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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0166_why_we_invest_in_bonds"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0166_ycharts_spxtr_vustx/SPX_MVUSTX_data.csv"),
                col.names = c("date","index_sp500", "index_bond")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

first_year <- year(min(raw$date))
last_year <- year(max(raw$date))

plot_stock_vs_bonds <- function(n_days_back, title_string, n_days_forward){
  to_plot <- raw %>%
              mutate(ret_sp500 = -1*(index_sp500/lag(index_sp500, n_days_back) - 1),
                     ret_bond = lead(index_bond, n_days_forward)/index_bond - 1) %>%
              drop_na() %>%
              filter(ret_sp500 > 0.1)
  
  n_bond_pos <- nrow(filter(to_plot, ret_bond > 0))
  n_total <- nrow(to_plot)
  
  n_days_back_string <- str_pad(n_days_back, width = 3, side = "left", pad = "0")
  
  file_path <- paste0(out_path, "/fwd_", n_days_forward, "_bonds_perf_stocks_down_", n_days_back_string,".jpeg")
  source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Stocks are represented by the S&P 500 Total Return while ",
                                 "bonds are represented by the Vanguard Long-Term Treasury Fund (VUSTX).  ",
                                 "Bond returns were positive in ", round(100*(n_bond_pos/n_total), 0), "% of the periods analyzed."),
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=ret_sp500, y=ret_bond)) + 
    geom_point() +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(label = percent, limits = c(0, 0.5)) +
    scale_y_continuous(label = percent, limits = c(-0.1, 0.4)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("How Bonds Peform Over Next ", n_days_forward, " Sessions\nWhen Stocks Are Down 10%+ Over ", title_string)) +
    labs(x = paste0("Stock Decline"), 
         y = paste0("Bond Return"),
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

to_run_df <- data.frame(n_days_back = c(5, 10, 120, 250, 500),
                        titles = c("5 Sessions", "10 Sessions",
                                   "6 Months", "1 Year",
                                   "2 Years")
)

fwd_days <- c(5, 10, 250, 500)

for(fwd in fwd_days){
  for(i in 1:nrow(to_run_df)){
    n_days_back <- to_run_df[i, "n_days_back"]
    title_string <- to_run_df[i, "titles"]
    
    plot_stock_vs_bonds(n_days_back, title_string, fwd)
  }
  
  create_gif(out_path, 
             paste0("fwd_", fwd, "_bonds_perf_*.jpeg"), 
             120, 
             out_name = paste0("_fwd_", fwd, "_bond_perf_periods.gif"))
}





# ############################  End  ################################## #