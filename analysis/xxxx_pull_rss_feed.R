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
library(tidyRSS)
library(tidyverse)

folder_name <- "xxxx_pull_rss_feed"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Change site feed to allow for more posts to be grabbed before running

df <- tidyfeed(
  "https://ofdollarsanddata.com/feed"
) 

to_export <- df %>%
              select(item_title, item_link, item_description, item_pub_date, item_category) %>%
              mutate(pub_date = as.Date(item_pub_date)) %>%
              arrange(pub_date) %>%
              mutate(post_num = row_number()) %>%
              rename(title = item_title,
                     link = item_link,
                     description = item_description,
                     category = item_category) %>%
              select(post_num, pub_date, title, link, description)

export_to_excel(to_export,
                outfile = paste0(out_path, "/all_posts.xlsx"),
                sheetname = "raw",
                new_file = 1,
                fancy_formatting = 0)

# ############################  End  ################################## #