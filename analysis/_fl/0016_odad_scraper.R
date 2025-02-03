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
  search = 'subject:"💰💻" exact',  # Using exact match
  include_spam_trash = FALSE, 
  num_results = 10000
)

for (i in 1:length(ms)){
  n_messages <- length(ms[[i]][[1]]) 
  print(paste0("In loop ", i, " of ", length(ms)))
  for (j in 1:n_messages){
    tryCatch({
      id <- ms[[i]][[1]][[j]]$id
      email <- gm_message(id, user_id = "me", format = c("full"))
      
      # Extract timestamp
      epoch <- as.numeric(email$internalDate)
      dt <- as.Date(as.POSIXct(epoch/1000, origin="1970-01-01"))
      
      # Extract sender email
      headers <- email$payload$headers
      from_header <- headers[[which(sapply(headers, function(x) x$name == "From"))]]
      sender_email <- gsub(".*<(.+)>.*", "\\1", from_header$value)
      
      # Extract message content with better error handling
      message_content <- tryCatch({
        if (!is.null(email$payload$parts)) {
          # Try to find text/plain part
          text_parts <- which(sapply(email$payload$parts, function(x) x$mimeType == "text/plain"))
          if (length(text_parts) > 0) {
            part_data <- email$payload$parts[[text_parts[1]]]$body$data
            if (!is.null(part_data)) {
              rawToChar(base64decode(gsub("-", "+", gsub("_", "/", part_data))))
            } else {
              "No readable content"
            }
          } else {
            # If no text/plain, try html part
            html_parts <- which(sapply(email$payload$parts, function(x) x$mimeType == "text/html"))
            if (length(html_parts) > 0) {
              part_data <- email$payload$parts[[html_parts[1]]]$body$data
              if (!is.null(part_data)) {
                rawToChar(base64decode(gsub("-", "+", gsub("_", "/", part_data))))
              } else {
                "No readable content"
              }
            } else {
              "No readable content"
            }
          }
        } else if (!is.null(email$payload$body$data)) {
          # Simple email with just body
          rawToChar(base64decode(gsub("-", "+", gsub("_", "/", email$payload$body$data))))
        } else {
          "No readable content"
        }
      }, error = function(e) {
        paste("Error extracting content:", e$message)
      })
      
      temp <- data.frame(
        id = id, 
        date = dt, 
        sender = sender_email,
        content = message_content,
        snippet = email$snippet,
        stringsAsFactors = FALSE
      )
      
      if (i == 1 & j == 1){
        df <- temp
      } else{
        df <- bind_rows(df, temp)
      } 
      
    }, error = function(e) {
      print(paste("Error processing message", i, j, ":", e$message))
    })
  }
}

# ############################  End  ################################## #