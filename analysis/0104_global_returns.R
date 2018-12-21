cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "0104_global_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

gr <- read.csv(paste0(importdir, "/0104_world_indices_total_return/world_tr_data_ycharts.csv")) %>%
        clean_cols() %>%
        mutate(date = as.Date(gsub("(.*)\\s.*", "\\1", date), format = "%m/%d/%y")) %>%
        filter(date < "2018-12-01")
        
colnames(gr) <- c("date", "France", "Germany", "Greece", "Italy", "Japan",
                  "SouthAfrica", "Spain", "UK", "US")

gr_long <- gr %>%
        mutate(year = year(date),
               month = month(date)) %>%
        gather(key=key, value=value, -date, -month, -year)

gr_yr_mo <- gr_long %>%
          group_by(year, month, key) %>%
          summarize(avg_index = mean(value, na.rm = TRUE)) %>%
          ungroup() %>%
          mutate(date = as.Date(paste0(year,"-", month, "-01"))) %>%
          filter(!is.na(avg_index))

first_yr_mo <- gr_yr_mo %>%
          group_by(date) %>%
          summarize(n_countries = n()) %>%
          filter(n_countries == (ncol(gr) - 1)) %>%
          head(1)

gr_yr_mo <- gr_yr_mo %>%
              filter(date >= pull(first_yr_mo[1, "date"]))

gr_yr_mo_1 <- gr_yr_mo %>%
                inner_join(first_yr_mo) %>%
                rename(first_index = avg_index) %>%
                select(key, first_index)

to_plot <- gr_yr_mo %>%
              left_join(gr_yr_mo_1) %>%
              mutate(value = avg_index/first_index) %>%
              select(date, key, value)

last_month <- filter(to_plot, date == max(to_plot$date)) 

ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  geom_text_repel(data=last_month, aes(x=date, y=value, col=key),
                  label = paste0(last_month$key, " $", round(last_month$value, 2)),
                  max.iter = 1000) +
  of_dollars_and_data_theme +
  labs(x="Date", y="Growth of $1") +
  ggtitle("Global Markets")

# ############################  End  ################################## #
