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

folder_name <- "xxxx_ind_stock_heatmap"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

tickers_2018 <- read_excel(paste0(importdir, "0155_ind_stocks_rebalance/SP500_tickers.xlsx"), skip = 6) %>%
                filter(!is.na(`12/31/2018`), `12/31/2018` != 0) %>%
                rename(symbol = TICKER) %>%
                select(symbol)

# Import YCharts Data
raw <- readRDS(paste0(localdir, "0155_ind_stocks_ycharts.Rds")) %>%
          filter(year(date) == 2018) %>%
          inner_join(tickers_2018) %>%
          select(date, symbol, ret)

date_coverage <- raw %>%
                  group_by(date) %>%
                  summarize(n_obs = n()) %>%
                  ungroup()
                    
df <- raw %>%
        inner_join(filter(date_coverage, n_obs == quantile(date_coverage$n_obs, probs = 0.5))) %>%
        select(-n_obs)
  
to_plot <- df %>%
            arrange(date, ret) %>%
            group_by(date) %>% 
            mutate(rank = rank(ret, ties.method = "first"),
                   pos = ifelse(ret >0, 1, 0)) %>%
            ungroup() %>%
            filter(month(date) >= 11)


source_string <- str_wrap(paste0("Source: YCharts (OfDollarsAndData.com)"), 
                          width = 80)
note_string <-  str_wrap(paste0("Individual stocks are constituents of the S&P 500 in 2018."), 
                         width = 80)

plot <- ggplot(data = to_plot, aes(x=1, y = rank, fill = ret)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green", guide = FALSE) +
  of_dollars_and_data_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank()) +
  ggtitle(paste0("S&P 500 Individual Stock Heatmap\n{closest_state}")) +
  labs(x = paste0("Date"), y = paste0("Rank"),
       caption = paste0(source_string, "\n", note_string)) +
  transition_states(date) +
  ease_aes('linear')

anim <- animate(plot, fps = 7)

anim_save(filename = paste0("sp500_heatmap_2018.gif"), animation = anim, path = out_path)


# ############################  End  ################################## #