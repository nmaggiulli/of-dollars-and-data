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

pull_data <- 0
client_path <- file.path(importdir, "0000_credentials", "nick_odad_gmail_creds.json")
client_path <- normalizePath(client_path, winslash = "/", mustWork = TRUE)

gm_auth_configure(path = client_path)

# Then authenticate
gm_auth(email = "nick@ofdollarsanddata.com")

if(pull_data == 1){
  # Function to get all messages recursively
  ms <- gm_messages(
    search = 'in:inbox',
    include_spam_trash = FALSE, 
    num_results = 10000
  )
  
  # Create empty dataframe with proper structure
  df <- data.frame(
    id = character(),
    date = as.Date(character()),
    subject = character(),
    sender = character(),
    content = character(),
    snippet = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each element of ms
  for(outer_idx in 1:length(ms)) {
    # Get number of messages in this element
    n_messages <- length(ms[[outer_idx]]$messages)
    
    print(paste("Processing batch", outer_idx, "of", length(ms), "-", n_messages, "messages"))
    
    # Loop through each message in this element
    for(inner_idx in 1:n_messages) {
      print(paste0("Processing message ", inner_idx, " of ", n_messages, " (batch ", outer_idx, ")"))
      
      tryCatch({
        # Extract and verify ID
        message_id <- ms[[outer_idx]]$messages[[inner_idx]]$id
        
        # Get message details
        email <- gm_message(message_id, user_id = "me", format = "full")
        
        # Extract headers
        headers <- email$payload$headers
        subject_idx <- which(sapply(headers, function(x) x$name == "Subject"))
        from_idx <- which(sapply(headers, function(x) x$name == "From"))
        
        # Skip if missing required headers
        if(length(subject_idx) == 0 || length(from_idx) == 0) {
          print(paste("Skipping message", inner_idx, "- missing required headers"))
          next
        }
        
        # Extract fields
        subject_line <- headers[[subject_idx]]$value
        from_header <- headers[[from_idx]]$value
        
        # Convert timestamp
        epoch <- as.numeric(email$internalDate)
        dt <- as.Date(as.POSIXct(epoch/1000, origin="1970-01-01"))
        
        # Extract sender email
        sender_email <- if(grepl("<.*>", from_header)) {
          gsub(".*<(.+)>.*", "\\1", from_header)
        } else {
          from_header
        }
        
        # Create new row
        new_row <- data.frame(
          id = message_id,
          date = dt,
          subject = subject_line,
          sender = sender_email,
          content = ifelse(!is.null(email$snippet), email$snippet, ""),
          snippet = ifelse(!is.null(email$snippet), email$snippet, ""),
          stringsAsFactors = FALSE
        )
        
        # Append to dataframe
        df <- bind_rows(df, new_row)
        
        # Add small delay to avoid rate limiting
        Sys.sleep(0.1)
        
      }, error = function(e) {
        print(paste("Error processing message", inner_idx, "in batch", outer_idx, ":", e$message))
      })
    }
  }
  saveRDS(df, paste0(localdir, "odad_gmail_messages.Rds"))
}

all_emails <- readRDS(paste0(localdir, "odad_gmail_messages.Rds"))

replies_only <- all_emails %>%
                  filter(grepl("💰💻", subject))

count_by_email <- replies_only %>%
                    group_by(sender) %>%
                    summarize(n_emails = n()) %>%
                    ungroup() %>%
                    arrange(desc(n_emails))

export_to_excel(count_by_email,
                outfile = paste0(out_path, "/", date_to_string(Sys.Date()), "_odad_responses.xlsx"),
                sheetname = "emails",
                new_file = 1,
                fancy_formatting = 0)


# ############################  End  ################################## #