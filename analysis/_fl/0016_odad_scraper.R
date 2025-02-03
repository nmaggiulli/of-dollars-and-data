cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(tidyverse)
library(lubridate)
suppressPackageStartupMessages(library(gmailr))

folder_name <- "_fl/0016_odad_scraper"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

client_path <- file.path(importdir, "0000_credentials", "nick_odad_gmail_creds.json")
client_path <- normalizePath(client_path, winslash = "/", mustWork = TRUE)

gm_auth_configure(path = client_path)

# Then authenticate
gm_auth(email = "nick@ofdollarsanddata.com")

ms <- gm_messages(
  search = 'in:inbox',  # or whatever folder you want to search
  include_spam_trash = FALSE, 
  num_results = 10000
)

# Then run your loop but add a filter for the emojis
for (i in 1:length(ms)){
  n_messages <- length(ms[[i]][[1]]) 
  print(paste0("In loop ", i, " of ", length(ms)))
  for (j in 1:n_messages){
    tryCatch({
      id <- ms[[i]][[1]][[j]]$id
      email <- gm_message(id, user_id = "me", format = c("full"))
      
      # Check subject first - with error handling
      headers <- email$payload$headers
      subject_idx <- which(sapply(headers, function(x) x$name == "Subject"))
      from_idx <- which(sapply(headers, function(x) x$name == "From"))
      
      # Skip if we can't find required headers
      if(length(subject_idx) == 0 || length(from_idx) == 0) {
        print(paste("Skipping message", i, j, "- missing required headers"))
        next
      }
      
      subject_line <- headers[[subject_idx]]$value
      from_header <- headers[[from_idx]]$value
      
      # Skip if subject doesn't contain the emojis
      if(!grepl("💰💻", subject_line)) {
        next
      }
      
      # Extract timestamp
      epoch <- as.numeric(email$internalDate)
      dt <- as.Date(as.POSIXct(epoch/1000, origin="1970-01-01"))
      
      # Extract sender email with safer regex
      sender_email <- if(grepl("<.*>", from_header)) {
        gsub(".*<(.+)>.*", "\\1", from_header)
      } else {
        from_header  # If no angle brackets, use the whole string
      }
      
      # Message content extraction remains the same
      message_content <- tryCatch({
        # ... your existing message content code ...
      }, error = function(e) {
        "Error extracting content"
      })
      
      # Create temp dataframe with explicit checking
      temp_list <- list(
        id = as.character(id),
        date = as.Date(dt),
        subject = as.character(subject_line),
        sender = as.character(sender_email),
        content = as.character(message_content),
        snippet = as.character(email$snippet)
      )
      
      # Print debug info
      print(paste("Creating temp df for message", i, j))
      print(str(temp_list))
      
      temp <- as.data.frame(temp_list, stringsAsFactors = FALSE)
      
      # Initialize or append to df with explicit error checking
      if (!exists("df")) {
        print("Creating initial df")
        df <- temp
      } else {
        print("Attempting to bind rows")
        print(paste("temp columns:", paste(names(temp), collapse = ", ")))
        print(paste("df columns:", paste(names(df), collapse = ", ")))
        
        tryCatch({
          df <- bind_rows(df, temp)
        }, error = function(e) {
          print(paste("Error binding rows for message", i, j))
          print("temp structure:")
          print(str(temp))
          print("df structure:")
          print(str(df))
          stop(e)
        })
      }
      
    }, error = function(e) {
      print(paste("Error processing message", i, j, ":", e$message))
    })
  }
}

# ############################  End  ################################## #