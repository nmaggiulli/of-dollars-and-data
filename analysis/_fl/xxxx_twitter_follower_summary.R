cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(rtweet)
library(httpuv)
library(readxl)
library(tidyverse)

folder_name <- "/_fl/xxxx_twitter_follower_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Dummy to pull data if needed
pull_data <- 0

# My app name
appname <- "TweetScraper22"

creds <- read.csv(paste0(importdir, "/0000_credentials/twitter_creds.csv"))

# Login token
twitter_token <- create_token(
  app = appname,
  consumer_key = creds$consumer_key,
  consumer_secret = creds$consumer_secret)

# List of handles
handles <- c("dollarsanddata")

if (pull_data == 1){
  for (i in 1:length(handles)){
    user <- handles[i]
    print(user)
    followers <- get_followers(user, n = 1.5*10^6, retryonratelimit = TRUE) 
    followers$handle <- user
    followers$date <- Sys.Date()
  }
  
  user_data <- lookup_users(followers$user_id)
  
  user_final <- user_data %>%
    select(screen_name, name, created_at, location, description)
  
  export_to_excel(user_final, outfile = paste0(out_path, "/", handles[1], "_twitter_data.xlsx"), sheetname = "twtr", 1, 0)
} else{
  df <- read_excel(paste0(out_path, "/", handles[1], "_twitter_data.xlsx")) %>%
          mutate(flag_bitcoin = grepl("bitcoin", description, ignore.case = TRUE),
                 flag_founder = grepl("founder", description, ignore.case = TRUE),
                 flag_investor = grepl("investor", description, ignore.case = TRUE),
                 flag_nyc = grepl("nyc", location, ignore.case = TRUE),
                 )
  
  summary <- df %>%
              summarise(flag_bitcoin = mean(flag_bitcoin),
                        flag_founder = mean(flag_founder),
                        flag_investor = mean(flag_investor),
                        flag_nyc = mean(flag_nyc)
              )
                        
}


# ############################  End  ################################## #