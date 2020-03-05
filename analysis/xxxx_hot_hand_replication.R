cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(stringr)
library(tidyverse)

folder_name <- "xxxx_hot_hand_replication"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

initial_strings <- c("H", "T")

df <- data.frame(streak = c(1, 1),
                flip_string = initial_strings,
                 flip_num = seq(1, length(initial_strings)))

streaks <- seq(2, 4)

for(s in streaks){
  tmp <- df %>%
          filter(streak == (s - 1)) 
  
  tmp <- tmp %>%
            bind_rows(tmp)
  
  for(i in 1:nrow(tmp)){
    
    if(i <= nrow(tmp)/2){
      new_flip <- "H"
    } else{
      new_flip <- "T"
    }
    
    tmp[i, "streak"] <- s
    tmp[i, "flip_string"] <- paste0(tmp[i, "flip_string"], new_flip)
    tmp[i, "flip_num"] <- i
  }
  
  df <- df %>%
          bind_rows(tmp)
}

count_all_occurences <- function(input_string, find_string){
  count <- 0
  for(i in 1:nchar(input_string)-1){
    if(substr(input_string, i, i+1) == find_string){
      count <- count + 1
    }
  }
  return(count)
}

df <- df %>%
                  mutate(n_heads_after_first_flip = str_count(substr(flip_string, 2, streak), "H"),
                         n_heads_after_heads = sapply(X = flip_string, FUN = count_all_occurences,
                                                        find_string = "HH"),
                         final_pct = n_heads_after_heads/n_heads_after_first_flip)

final_results <- df %>%
                  filter(!is.na(final_pct)) %>%
                  group_by(streak) %>%
                  summarize(prob_h = sum(final_pct, na.rm = TRUE)/n())

# ############################  End  ################################## #
