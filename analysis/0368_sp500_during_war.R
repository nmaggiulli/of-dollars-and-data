cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyverse)

folder_name <- "0368_sp500_during_war"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    mutate(ret_stock_real = price_plus_div/lag(price_plus_div, 1) - 1,
                           ret_cpi = cpi/lag(cpi, 1) - 1) %>%
                    select(date, contains("ret_")) %>%
                    filter(date >= "1900-01-01")

war_dates <- data.frame(date = c(
  seq.Date(as.Date("1917-04-01"), as.Date("1918-11-01"), "month"),
  seq.Date(as.Date("1939-09-01"), as.Date("1945-09-01"), "month"),
  seq.Date(as.Date("1950-06-01"), as.Date("1953-07-01"), "month"),
  seq.Date(as.Date("1965-03-01"), as.Date("1973-03-01"), "month")
)) %>%
  mutate(war_name = case_when(
    date < as.Date("1920-01-01") ~ "WWI",
    date < as.Date("1950-01-01") ~ "WWII",
    date < as.Date("1960-01-01") ~ "Korea",
    date < as.Date("1980-01-01") ~ "Vietnam",
    TRUE ~ "Error"
  ))

df <- sp500_ret_pe %>%
          left_join(war_dates) %>%
          mutate(war = ifelse(is.na(war_name), 0, 1))

t.test(df$ret_stock_real~df$war)
t.test(df$ret_bond_real~df$war)
t.test(df$ret_cpi~df$war)

plot_monthly_average <- function(out_name, war_filter, war_label){
  
  to_plot <- df %>%
              mutate(wartime = case_when(
                war_name %in% war_filter ~ 1,
                TRUE ~ 0
              )) %>%
              group_by(wartime) %>%
              summarise(ret_stock_real = quantile(ret_stock_real, probs = 0.5),
                        ret_bond_real = quantile(ret_bond_real, probs = 0.5)) %>%
              ungroup() %>%
              mutate(wartime = case_when(
                wartime == 1 ~ war_label,
                TRUE ~ "Peacetime"
              ),
              ret_stock_real = (1+ret_stock_real)^12 - 1,
              ret_bond_real = (1+ret_bond_real)^12 - 1) %>%
              rename(`U.S. Stocks` = ret_stock_real,
                     `U.S. Bonds` = ret_bond_real) %>%
              gather(-wartime, key=key, value=value)
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
              
  
  file_path <- paste0(out_path, "/", out_name, ".jpeg")
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Returns are annualized based on monthly data, include dividends, and are adjusted for inflation. ",
                                 "U.S. Bond returns are the return on 10-Year U.S. Treasury bonds."),
                          width = 85)
  
  plot <- ggplot(data = to_plot, aes(x = key, y = value, fill = wartime)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("blue", "red")) +
    scale_y_continuous(label = percent_format(accuracy = 0.1), breaks = seq(-0.04, 0.14, 0.02), limits = c(-0.04, 0.14)) +
    ggtitle("U.S. Stock and U.S. Bond Real Returns\nBy Wartime Status") +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    labs(x = "Asset Class" , y = "Median Annualized Return",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  #Now do just CPI
  if(out_name == "us_stock_bond_ret_all_wars"){
  
    to_plot <- df %>%
      mutate(wartime = case_when(
        war_name %in% war_filter ~ 1,
        TRUE ~ 0
      )) %>%
      group_by(wartime) %>%
      summarise(ret_cpi = mean(ret_cpi)) %>%
      ungroup() %>%
      mutate(wartime = case_when(
        wartime == 1 ~ war_label,
        TRUE ~ "Peacetime"
      ),
      ret_cpi = (1+ret_cpi)^12 - 1) %>%
      rename(`CPI` = ret_cpi) %>%
      gather(-wartime, key=key, value=value)      
    
    assign("to_plot_cpi", to_plot, envir = .GlobalEnv)
    
    file_path <- paste0(out_path, "/cpi_avg_ret_all_wars.jpeg")
    source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
    note_string <- str_wrap(paste0("Note:  Returns are annualized based on monthly data."),
                            width = 85)
    
    plot <- ggplot(data = to_plot, aes(x = key, y = value, fill = wartime)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("blue", "red")) +
      scale_y_continuous(label = percent_format(accuracy = 0.1)) +
      ggtitle("U.S. CPI\nBy Wartime Status") +
      of_dollars_and_data_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom") +
      labs(x = "" , y = "Average Annualized Change",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

ww_vector <- c("WWI", "WWII")
all_w_vector <- c(ww_vector, "Vietnam", "Korea")

plot_monthly_average("us_stock_bond_ret_world_wars", ww_vector, "WWI & WWII")
plot_monthly_average("us_stock_bond_ret_all_wars", all_w_vector, "WWI, WWII, Korea & Vietnam")

# ############################  End  ################################## #