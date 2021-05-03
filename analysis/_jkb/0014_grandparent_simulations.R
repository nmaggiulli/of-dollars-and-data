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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "_jkb/0014_grandparent_simulations"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")

birth_year <- 1938
start_year <- 1993
end_date <- as.Date("2019-05-01")
ss_age <- 62
savings_rate <- 0.50
payment1 <- 1000*savings_rate
payment2 <- 1200*savings_rate

raw <- read.csv(paste0(importdir, "_jkb/0014_grandparent_simulation/SPXTR_data.csv")) %>%
          rename(index_sp500 = `S.P.500.Total.Return.Level`) %>%
          mutate(date = as.Date(Period)) %>%
          select(date, index_sp500) %>%
        arrange(date) %>%
        mutate(ret_sp500 = index_sp500/lag(index_sp500, 1) - 1) %>%
        filter(year(date) >= start_year, date < end_date)

df <- raw

for(i in 1:nrow(df)){
  ret <- df[i, "ret_sp500"]
  yr <- year(df[i, "date"])
  mt <- month(df[i, "date"])
  
  if(i == 1){
    df[i, "value_portfolio"] <- payment1
  } else{
    lag_mt <- month(df[(i-1), "date"])
    
    if(mt != lag_mt & yr >= (ss_age + birth_year)){
      new_payment <- payment1 + payment2
    } else if(mt != lag_mt){
      new_payment <- payment1
    }else{
      new_payment <- 0
    }
    
    df[i, "value_portfolio"] <- df[(i-1), "value_portfolio"] * (1 + ret) + new_payment
  }
}

# Do grandparents opportnity cost return on home
sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
          filter(date >= "1972-01-01", date <= "2001-12-31") %>%
          select(date, price_plus_div) %>%
          mutate(ret = price_plus_div/lag(price_plus_div, 1) - 1,
                 payment = case_when(
                   date <= as.Date("2001-12-31") ~ 280,
                   TRUE ~ 0
                 ))

for(i in 1:nrow(sp500)){
  ret <- sp500[i, "ret"]
  monthly_payment <- sp500[i, "payment"]
  
  if(i == 1){
    sp500[i, "portfolio"] <- monthly_payment
  }else{
    sp500[i, "portfolio"] <- (sp500[(i-1), "portfolio"] * (1 + ret)) + monthly_payment
  }
}

shiller_housing <- read_excel(paste0(importdir, "_jkb/0014_grandparent_simulation/shiller_house_data.xls"),
                              sheet = "Data",
                              skip = 6) %>%
                    select(1, 2) 

colnames(shiller_housing) <- c("date", "real_housing_index")

to_plot <- shiller_housing

file_path <- paste0(out_path, "/shiller_hpi.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=real_housing_index)) +
  geom_line() +
  scale_y_continuous(label =  comma) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Housing Index Since 1890")) +
  labs(x="Year", y="Real Housing Index")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #