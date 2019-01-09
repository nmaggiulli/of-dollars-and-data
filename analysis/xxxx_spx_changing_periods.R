cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(ggjoy)
library(tidyr)
library(readxl)
library(dplyr)

folder_name <- "xxxx_spx_changing_periods"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read_excel(paste0(importdir, "xxxx_spx_monthly_tr/dfa_sp500_tr.xlsx"), skip = 4) %>%
  filter(!is.na(Date)) %>%
  rename(index = `S&P 500 Index`) %>%
  mutate(date = as.Date(Date)) %>%
  filter(date >= "1926-11-01") %>%
  select(-Date)

first_value <- pull(spx[1, "index"])

spx <- spx %>%
        mutate(sp500 = index/first_value) %>%
        select(-index)

trend <- read_excel(paste0(importdir, "xxxx_spx_monthly_tr/econompic_trend_data.xlsx")) %>%
            select(1, 8, 9)

colnames(trend) <- c("date", "trend", "sixty_forty")

trend <- trend %>%
          mutate(date = as.Date(paste0(
            year(date), 
            "-",
            month(date),
            "-01"),
            format = "%Y-%m-%d"
          )) %>%
        filter(!is.na(trend))

pre_plot <- spx %>%
              inner_join(trend)

plot_period <- function(n_years, multiplot, multivars, multiname, multititle){
  n_periods <- n_years * 12
  
  to_plot <- pre_plot %>%
              mutate(`SP500` = (lead(sp500, n_periods)/sp500),
                     `Trend` = (lead(trend, n_periods)/trend),
                     `Portfolio_60_40` = (lead(sixty_forty, n_periods)/sixty_forty)) %>%
              filter(!is.na(`SP500`)) %>%
              mutate(number_of_years = n_years)
  
  if(n_years < 10){
    year_string <- paste0("0", n_years)
  } else{
    year_string <- n_years
  }
  
  if (multiplot == 0){
    # File path
    file_path <- paste0(out_path, "/period_plot_", year_string, ".jpeg")
    
    source_string <- paste0("Source:  DFA (OfDollarsAndData.com)")
    note_string <- paste0("Note:  Returns include dividends, but not adjusted for inflation.")
    
    plot <- ggplot(to_plot, aes(x=date, y=`SP500`)) + 
      geom_point(alpha = 0.5) +
      scale_y_continuous(label = dollar, trans = log2_trans(), limits = c(0.25, 64)) +
      scale_x_date(date_labels = "%Y", breaks = c(
        as.Date("1920-01-01"),
        as.Date("1940-01-01"),
        as.Date("1960-01-01"),
        as.Date("1980-01-01"),
        as.Date("2000-01-01"),
        as.Date("2020-01-01")
      ),
                   limits = c(as.Date("1920-01-01"), as.Date("2020-01-01"))) +
      of_dollars_and_data_theme +
      ggtitle(paste0("S&P 500 Growth of $1\nOver ", n_years, " Years")) +
      labs(x="Year", y=paste0("Growth of $1"),
           caption = paste0(source_string, "\n", note_string))
  
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm") 
  
  
    if(n_years == 1){
      stacked <- to_plot
    } else{
      stacked <- get("stacked", envir = .GlobalEnv)
      stacked <- stacked %>% bind_rows(to_plot)
    }
  
    assign("stacked", stacked, envir = .GlobalEnv)
    
  } else if (multiplot == 1){
    to_plot <- to_plot %>%
                  select_(.dots = multivars) %>%
                  gather(key=key, value=value, -date)
    
    # File path
    file_path <- paste0(out_path, "/", multiname, "_", year_string, ".jpeg")
    
    source_string <- paste0("Source:  DFA, @EconomPic (OfDollarsAndData.com)")
    note_string <- paste0("Note:  Returns include dividends, but not adjusted for inflation.")
    
    plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
      geom_point(alpha = 0.3) +
      scale_color_discrete() +
      scale_y_continuous(label = dollar, trans = log2_trans(), limits = c(0.25, 64)) +
      scale_x_date(date_labels = "%Y", breaks = c(
        as.Date("1920-01-01"),
        as.Date("1940-01-01"),
        as.Date("1960-01-01"),
        as.Date("1980-01-01"),
        as.Date("2000-01-01"),
        as.Date("2020-01-01")
      ),
      limits = c(as.Date("1920-01-01"), as.Date("2020-01-01"))) +
      of_dollars_and_data_theme +
      ggtitle(paste0(multititle, " Growth of $1\nOver ", n_years, " Years")) +
      labs(x="Year", y=paste0("Growth of $1"),
           caption = paste0(source_string, "\n", note_string)) +
      theme(legend.position = "bottom")
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm") 
  }
}

for (i in 1:30){
  plot_period(i, 0, "", "", "")
  plot_period(i, 1, c("date", "SP500", "Trend"), "trend_vs_sp500", "Trend vs. S&P 500")
  plot_period(i, 1, c("date", "Portfolio_60_40", "Trend"), "trend_vs_6040", "Trend vs. 60/40")
}

create_gif(out_path, "period_plot_*.jpeg", 20, 0, "all_sp500.gif")
create_gif(out_path, "trend_vs_sp500_*.jpeg", 20, 0, "all_trend_sp500.gif")
create_gif(out_path, "trend_vs_6040_*.jpeg", 20, 0, "all_trend_sixty_forty.gif")


# Use the stacked data to make a big image

file_path <- paste0(out_path, "/all_years.jpeg")

source_string <- paste0("Source:  DFA (OfDollarsAndData.com)")
note_string <- paste0("Note:  Returns include dividends, but not adjusted for inflation.")

plot <- ggplot(stacked, aes(x=date, y=`SP500`, col =  as.factor(number_of_years))) + 
  geom_point(alpha = 0.5) +
  scale_color_discrete(guide=FALSE) +
  scale_y_continuous(label = dollar, trans = log2_trans(), limits = c(0.25, 64)) +
  scale_x_date(date_labels = "%Y", breaks = c(
    as.Date("1920-01-01"),
    as.Date("1940-01-01"),
    as.Date("1960-01-01"),
    as.Date("1980-01-01"),
    as.Date("2000-01-01"),
    as.Date("2020-01-01")
  ),
  limits = c(as.Date("1920-01-01"), as.Date("2020-01-01"))) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Growth of $1")) +
  labs(x="Year", y=paste0("Growth of $1"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm") 

# ############################  End  ################################## #