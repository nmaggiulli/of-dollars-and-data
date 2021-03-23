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

folder_name <- "_jkb/0013_leverage_simulation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")
max_leverage <- 3

dow_2020 <- read.csv(paste0(importdir, "_jkb/0013_leverage_simulation/DJI_data.csv"), 
                     col.names = c("date", "index_dow")) %>%
            mutate(date = as.Date(date)) %>%
            filter(date <= "2020-12-31") %>%
            arrange(date) %>%
            mutate(ret = index_dow/lag(index_dow, 1) - 1) %>%
            drop_na

pre_2020_dow <- read_excel(paste0(importdir, "_jkb/0013_leverage_simulation/Dow daily 2020.xlsx"),
                           col_names = c("date", "index_dow")) %>%
                  mutate(date = as.Date(date)) %>%
                  drop_na %>%
                  mutate(ret = index_dow/lag(index_dow, 1) - 1) %>%
                  filter(date < "2020-01-01")

df <- pre_2020_dow %>%
        bind_rows(dow_2020) %>%
        mutate(year = year(date))
  
build_leverage_data <- function(lookforward){
  
  for(i in 1:(nrow(df)-lookforward)){
    current_index <- df[i, "index_dow"]
    
    df[i, "max_loss"] <- min(df[i:(lookforward+i-1), "index_dow"])/current_index - 1
    df[i, "fwd_ret"] <- df[lookforward+i-1, "index_dow"]/current_index - 1
  }
  
  leverage_ratios <- seq(1.5, max_leverage, 0.5)
  final_results <- data.frame()
  
  for(i in 1:length(leverage_ratios)){
    leverage_ratio <- leverage_ratios[i]
    
    tmp <- df %>%
              mutate(wiped_out = ifelse(max_loss < -(1/leverage_ratio), 1, 0))
    
    final_results[i, "leverage_ratio"] <- leverage_ratio
    final_results[i, "lookforward"] <- lookforward
    final_results[i, "pct_broke"] <- mean(tmp$wiped_out, na.rm = TRUE)
  }
  return(final_results)
}

lev_250 <- build_leverage_data(250)
lev_1250 <- build_leverage_data(250*5)

to_plot <- lev_250 

file_path <- paste0(out_path, "/leverage_vs_broke_250.jpeg")

plot <- ggplot(to_plot, aes(x = leverage_ratio, y = pct_broke)) +
  geom_line(col = bw_colors[1]) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1, max_leverage, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Probability of Going Broke Investing in Dow\nBy Leverage Ratio")) +
  labs(x = "Leverage Ratio" , y = "How Often You Go Broke")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- lev_250 %>%
            bind_rows(lev_1250) %>%
            select(leverage_ratio, pct_broke, lookforward)

file_path <- paste0(out_path, "/leverage_vs_broke_full.jpeg")

plot <- ggplot(to_plot, aes(x = leverage_ratio, y = pct_broke, col = as.factor(lookforward))) +
  geom_line() +
  scale_color_manual(values = c(bw_colors), guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1, max_leverage, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Probability of Going Broke Investing in Dow\nBy Leverage Ratio")) +
  labs(x = "Leverage Ratio" , y = "How Often You Go Broke")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #