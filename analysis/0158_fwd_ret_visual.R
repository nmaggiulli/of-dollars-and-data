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
library(tidyverse)

folder_name <- "0158_fwd_ret_visual"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0158_dfa_sp500/DFA_PeriodicReturns_20191223103141.csv"), skip = 7,
                col.names = c("date", "ret_sp500", "blank_col")) %>%
  select(-blank_col) %>%
  filter(!is.na(ret_sp500)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  select(date, ret_sp500)

df <- raw

for(i in 1:nrow(df)){
  ret <- df[i, "ret_sp500"]
  
  if(i == 1){
    df[i, "index"] <- 1
  } else{
    df[i, "index"] <- df[(i-1), "index"] * (1 + ret)
  }
}

years_seq <- seq(1, 20, 1)

for(y in years_seq){
  n_months <- y * 12
  
tmp <- df %>%
  mutate(lag_ret_12m = index/lag(index, 12) - 1,
         lead_ret = (lead(index, n_months)/index)^(1/y) - 1,
         n_years_ret = y) %>%
  filter(!is.na(lag_ret_12m), !is.na(lead_ret)) 

  if(y == min(years_seq)){
    final_results <- tmp
  } else{
    final_results <- bind_rows(final_results, tmp)
  }
}

to_plot <- final_results %>%
              select(lag_ret_12m, lead_ret, n_years_ret)

plot <- ggplot(to_plot, aes(x=lag_ret_12m, y=lead_ret)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle("{closest_state}-Year Forward Return\nBased on 1-Year Prior Return") +
  labs(x= "1-Year Prior Return", y ="Annualized Forward Return") +
  transition_states(n_years_ret) +
  ease_aes('linear')

anim <- animate(plot, fps = 7)

anim_save(filename = paste0("annual_fwd_ret_scatter.gif"), animation = anim, path = out_path)

# ############################  End  ################################## #