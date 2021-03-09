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
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date)) %>%
  arrange(date) %>%
  drop_na()

first_sp500 <- raw[1, "index_sp500"]
first_bond <- raw[1, "index_bond"]

raw <- raw %>%
        mutate(index_sp500 = index_sp500/first_sp500,
               index_bond = index_bond/first_bond)

first_year <- year(min(raw$date))
last_year <- year(max(raw$date))

plot_stock_vs_bonds <- function(n_days_back, title_string, n_days_forward, fwd_title_string){
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
    scale_x_continuous(label = percent, limits = c(0, 0.6)) +
    scale_y_continuous(label = percent) +
    of_dollars_and_data_theme +
    ggtitle(paste0("How Bonds Peform Over Next ", fwd_title_string, "\nWhen Stocks Are Down 10%+ Over ", title_string)) +
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
  if(fwd == 250){
    fwd_title_string <- "1 Year"
  } else if (fwd == 500){
    fwd_title_string <- "2 Years"
  } else{
    fwd_title_string <- paste0(fwd, " Sessions")
  }
  
  for(i in 1:nrow(to_run_df)){
    n_days_back <- to_run_df[i, "n_days_back"]
    title_string <- to_run_df[i, "titles"]
    
    plot_stock_vs_bonds(n_days_back, title_string, fwd, fwd_title_string)
  }
}

# Get earliest date in each month
rebal_dates <- raw %>%
                        group_by(year, month) %>%
                        summarise(min_date = min(date),
                                  rebal = 1) %>%
                        ungroup() %>%
                        filter(month == 1 | month == 7)

# Do rebalancing analysis
run_rebal <- function(wt_bond){
  df <- raw %>%
          left_join(rebal_dates)
  
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
    df[i, "value_sp500"] <- df[i, "index_sp500"]
  }
  
  to_plot <- df %>%
              select(date, value_rebal, value_sp500) %>%
              rename(`S&P 500` = value_sp500,
                     `Rebalanced` = value_rebal) %>%
              gather(-date, key=key, value=value)
  
  wt_bond_string <- str_pad(100*wt_bond, side = "left", width = 3, pad = "0")
  
  file_path <- paste0(out_path, "/rebal_bond_", wt_bond_string, "_pct.jpeg")
  source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Stocks are represented by the S&P 500 Total Return while ",
                                 "bonds are represented by the Vanguard Long-Term Treasury Fund (VUSTX).  ",
                                 "The rebalanced portfolio rebalances semi-annually in January and July."),
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
    geom_line() +
    scale_color_manual(values = c("#10253F", "#ff0000")) +
    scale_y_continuous(label = dollar, limits = c(0, 15)) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("S&P 500 Only vs.\nRebalanced Portfolio With ", 100*wt_bond, "% Bonds")) +
    labs(x = paste0("Date"), 
         y = paste0("Growth of $1"),
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

wts <- seq(0.1, 0.5, 0.1)
for(w in wts){
  run_rebal(w)
}

create_gif(path = out_path, 
           file_stub = "rebal_bond_*.jpeg", 
           speed_milliseconds = 140,
             out_name = "_all_rebal_bond_wt_stock.gif")

# Show annual calendar returns
to_plot <- rebal_dates %>%
                filter(month == 1) %>%
                rename(date = min_date) %>%
                select(date) %>%
                inner_join(raw) %>%
                mutate(`S&P 500` = lead(index_sp500)/index_sp500 - 1,
                       `Bonds` = lead(index_bond)/index_bond - 1,
                       year = year(date)) %>%
                select(year, `S&P 500`, `Bonds`) %>%
                gather(-year, key=key, value=value) %>%
                drop_na()

file_path <- paste0(out_path, "/calendar_ret_stock_bond.jpeg")
source_string <- paste0("Source:  YCharts, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  Stocks are represented by the S&P 500 Total Return while ",
                               "bonds are represented by the Vanguard Long-Term Treasury Fund (VUSTX).  "),
                        width = 80)

plot <- ggplot(to_plot, aes(x=year, y=value, fill = key)) + 
  geom_bar(stat="identity") +
  facet_grid(~key) +
  scale_fill_manual(values = c("#10253F", "#ff0000"), guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Bonds Have Far Less Downside Volatility\nThan Stocks")) +
  labs(x = paste0("Year"), 
       y = paste0("Annual Return"),
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

  

# ############################  End  ################################## #