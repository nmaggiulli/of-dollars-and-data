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
library(zoo)
library(PerformanceAnalytics)
library(tidyverse)

folder_name <- "xxxx_show_repeater"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_days <- 366
show_pattern <- c(0, 1, 10, 30, 60, 101, 152, 214, 286)

df <- data.frame(day = seq(1, n_days*2, 1), filled = rep(0, n_days*2))

broken <- 0
show_number <- 1
while(broken == 0){
  print(show_number)
  show_tmp <- data.frame(day = show_pattern + show_number, show = show_number)
  
  open <- df %>%
            inner_join(show_tmp) %>%
            summarize(filled = sum(filled, na.rm = TRUE)) %>%
            pull(filled)
  
  if(open == 0){
    print("filled")
    df <- df %>%
            left_join(show_tmp) %>%
            mutate(filled = ifelse(is.na(show) & filled == 0, 0, 
                                   ifelse(filled != 0, filled, show))) %>%
            select(-show)
    
    show_number <- show_number + 1
  } else{
    show_number <- show_number + 1
  }
  
  if(show_number > n_days){
    broken <- 1
  }
}

all_shows <- sort(unique(df$filled))
all_shows <- all_shows[2:length(all_shows)]
final_show <- seq(1, length(all_shows))

to_merge <- data.frame(filled = all_shows, idea = final_show)

final_results <- df %>%
                  left_join(to_merge) %>%
                  select(day, idea)

by_show <- final_results %>%
            gather(-idea, key=key, value=value) %>%
            arrange(idea)

empty_days <- by_show %>%
                filter(is.na(idea)) %>%
                select(-idea) %>%
                arrange(value)

by_show <- by_show %>%
            filter(!is.na(idea))

show_num <- 1
for(i in 1:nrow(by_show)){
  if(i == 1){
    by_show[i, "show_num"] <- paste0("date_", show_num)
  } else{
    if(by_show[i, "idea"] == by_show[(i-1), "idea"]){
      show_num <- show_num + 1
      by_show[i, "show_num"] <- paste0("date_", show_num) 
    } else{
      show_num <- 1
      by_show[i, "show_num"] <- paste0("date_", show_num)
    }
  }
}

final_guide <- by_show %>%
                  select(-key) %>%
                  spread(key=show_num, value=value)

# ############################  End  ################################## #