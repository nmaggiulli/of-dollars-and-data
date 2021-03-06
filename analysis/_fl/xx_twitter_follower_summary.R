cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(rtweet)
library(httpuv)
library(tidyverse)

folder_name <- "/_fl/xx_twitter_follower_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# My app name
appname <- "TweetScraper22"

# API key
key <- 'TMRoCTZ2Eis1O7ELovhs8ni9X'

# API secret
secret <- 'trdbfmbVNG5BOiJyvX4pUDmtccN0KV5Y5AWNcz2zKUbzhwus27'

# Login token
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# List of handles
handles <- c("dollarsanddata")

# Dummy to pull data if needed
pull_data <- 1

if (pull_data == 1){
  for (i in 1:length(handles)){
    user <- handles[i]
    print(user)
    followers <- get_followers(user, n = 1.5*10^6, retryonratelimit = TRUE) 
    followers$handle <- user
    followers$date <- Sys.Date()
  }
  
  user_data <- lookup_users(followers$user_id) %>%
                  select(user_id, screen_name, name, location, description)
}

user_final <- user_data

export_to_excel(user_final, outfile = paste0(out_path, "/", handles[1], "_twitter_data.xlsx"), sheetname = "twtr", 1, 0)

# ############################  End  ################################## #