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

flips_df <- data.frame(streak = c(1, 1),
                flip_string = initial_strings,
                 flip_num = seq(1, length(initial_strings)))

streaks <- seq(2, 10)

for(s in streaks){
  tmp <- flips_df %>%
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
  
  flips_df <- flips_df %>%
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

flips_df <- flips_df %>%
      mutate(n_flips_after_heads = str_count(substr(flip_string, 1, streak-1), "H"),
             n_heads_after_heads = sapply(X = flip_string, FUN = count_all_occurences,
                                            find_string = "HH"),
             final_pct = n_heads_after_heads/n_flips_after_heads)

flip_results <- flips_df %>%
                  filter(!is.na(final_pct)) %>%
                  group_by(streak) %>%
                  summarize(prob_h = sum(final_pct, na.rm = TRUE)/n())

#Create fake data
set.seed(12345)

rands <- data.frame(rand = runif(10^6, 0, 1))

mat <- rands %>%
          mutate(pos = ifelse(rand > 0.5, 1, 0)) %>%
          select(pos) %>%
          as.matrix()

streaks <- seq(2, 10)
final_results <- data.frame(streak = streaks)

results_counter <- 1
for(s in streaks){
  print(s)
  bag_pos <- c()
  counter_pos <- 1
  
  bag_neg <- c()
  counter_neg <- 1
  for(i in 1:nrow(mat)-1){
    if(i >= s){
      streak_sum <- sum(mat[(i-s+1):i, 1])
      if(streak_sum == s){
        bag_pos[counter_pos] <- mat[i+1, 1]
        counter_pos <- counter_pos + 1
      } else if(streak_sum == 0){
        bag_neg[counter_neg] <- mat[i+1, 1]
        counter_neg <- counter_neg + 1
      }
    }
  }
  final_results[results_counter, "p_next_pos"] <- mean(bag_pos)
  final_results[results_counter, "p_next_neg"] <- mean(bag_neg)
  results_counter <- results_counter + 1
}

# ############################  End  ################################## #
