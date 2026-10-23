cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# ---------------------------------------------------------------------------
# ONE script for all three S&P 500 calculators:
#   S&P 500 calculator, S&P 500 DCA calculator, U.S. stock/bond calculator
#
# Writes ONE file, sp500_data.js. Each month:
#   1. Run this script (it downloads Shiller's ie_data.xls and the FRED
#      CPIAUCNS and GS10 files automatically)
#   2. Upload sp500_data.js to wp-content/themes/odad/calculators/ (replace the old one)
#   3. Purge the Cloudflare cache
# The calculator code, year lists, default dates and "Data through" labels all
# come from the theme, so nothing else needs to change.
#
# Every run writes a log to the output folder:
#   sp500_data_log.txt - all messages, warnings, errors and printed output
# ---------------------------------------------------------------------------

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(jsonlite)
library(zoo)
library(readxl)
library(lubridate)
library(quantmod)
library(tidyverse)

folder_name <- "_calculators/0000_sp500_calculator_data"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
log_path  <- paste0(out_path, "/sp500_data_log.txt")
log_con   <- file(log_path, open = "wt")

# Write one timestamped line to the log file
# (Hides the FRED API key, because download warnings include the full URL.)
log_line <- function(...) {
  txt <- paste0(...)
  key <- Sys.getenv("FRED_API_KEY")
  if (nchar(key) > 0) txt <- gsub(key, "<key>", txt, fixed = TRUE)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", txt, "\n",
      file = log_con, sep = "")
  flush(log_con)
}

# Describe where a warning or error came from in one short line.
# Calls raised at the top level of the script would otherwise print the whole script.
short_call <- function(cond) {
  cl <- conditionCall(cond)
  if (is.null(cl)) return("")
  fn <- tryCatch(deparse(cl[[1]])[1], error = function(e) "")
  if (fn %in% c("withCallingHandlers", "tryCatch", "doTryCatch", "eval")) return("")
  txt <- paste(deparse(cl), collapse = " ")
  paste0(" | in: ", if (nchar(txt) > 150) paste0(substr(txt, 1, 150), "...") else txt)
}

log_line("Run started")
log_line("R version: ", R.version.string, " | OS: ", Sys.info()[["sysname"]])
log_line("Working dir: ", getwd())

# Send printed output to the console AND the log
sink(log_con, split = TRUE)

tryCatch(withCallingHandlers({
  ########################## Start Program Here ######################### #
  
  today <- Sys.Date()
  filter_date <- "1871-01-01"
  data_dir <- paste0(importdir, "0009_sp500_returns_pe/")
  log_line("Data dir: ", data_dir)
  
  # ---------------------------------------------------------------------------
  # Download source data
  # ---------------------------------------------------------------------------
  
  # Two download methods, because each source needs a different one:
  #   Shiller: curl package with a browser user agent (the GoDaddy host needs it)
  #   FRED:    base download.file() with R's default user agent (FRED's CDN rejects
  #            a fake browser user agent, and the curl package hung on FRED)
  browser_agent <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                         "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36")
  download_timeout <- 60  # seconds per download; a stuck download fails instead of hanging
  
  # Make a curl handle for the Shiller requests.
  make_shiller_handle <- function() {
    h <- curl::new_handle(timeout = download_timeout, connecttimeout = 20)
    curl::handle_setheaders(h, "User-Agent" = browser_agent)
    h
  }
  log_line("curl package version: ", as.character(packageVersion("curl")))
  
  # Download one file to dest.
  # method = "curl": curl package with browser user agent (Shiller)
  # method = "base": download.file() with R's default user agent (FRED)
  # fallback = TRUE: if the download fails, keep the old local copy and warn.
  # fallback = FALSE: if the download fails, stop.
  fetch_file <- function(url, dest, method = c("base", "curl"), fallback = TRUE) {
    method <- match.arg(method)
    log_line("Downloading (", method, "): ", url)
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(dest)))
    err <- NULL
    if (method == "curl") {
      tryCatch(curl::curl_download(url, tmp, handle = make_shiller_handle(), quiet = TRUE),
               error = function(e) err <<- conditionMessage(e))
    } else {
      old_timeout <- options(timeout = download_timeout)
      tryCatch(download.file(url, tmp, mode = "wb", quiet = TRUE),
               error = function(e) err <<- conditionMessage(e))
      options(old_timeout)
    }
    size <- if (file.exists(tmp)) file.size(tmp) else 0
    log_line("  Bytes received: ", size, if (!is.null(err)) paste0(" | Error: ", err) else "")
    ok <- is.null(err) && size > 1000
    
    if (ok) {
      file.copy(tmp, dest, overwrite = TRUE)
      message("Downloaded: ", basename(dest))
    } else if (!fallback) {
      stop("Download failed: ", url)
    } else if (file.exists(dest)) {
      warning("Download failed. Using the old local file: ", dest)
    } else {
      stop("Download failed and no local file exists: ", dest)
    }
  }
  
  # Shiller's link changes with each update, so read the current link from his page.
  get_shiller_url <- function() {
    resp <- curl::curl_fetch_memory("https://shillerdata.com/", handle = make_shiller_handle())
    log_line("shillerdata.com HTTP status: ", resp$status_code)
    html <- rawToChar(resp$content)
    Encoding(html) <- "UTF-8"
    html <- gsub("[\r\n]+", " ", html)
    log_line("shillerdata.com page: ", nchar(html), " characters")
    log_line("  Mentions of 'ie_data' on page: ", str_count(html, "ie_data"))
    
    # Undo escaped slashes (\u002F and \/) that appear in embedded JSON
    html <- gsub("\\\\u002F", "/", html, ignore.case = TRUE)
    html <- gsub("\\\\/", "/", html)
    # Match with or without "https:" (the page can use //img1.wsimg.com/...)
    path <- str_extract(html,
                        "img1\\.wsimg\\.com/blobby/go/[^\"'\\s<>]+?/ie_data\\.xls(\\?ver=\\d+)?")
    if (is.na(path)) {
      # Log the text around the first mention of ie_data, if there is one
      pos <- str_locate(html, "ie_data")[1, "start"]
      if (!is.na(pos)) {
        log_line("  Text near 'ie_data': ",
                 substr(html, max(1, pos - 300), pos + 300))
      }
      stop("Could not find the ie_data.xls link on shillerdata.com. ",
           "Page length read: ", nchar(html), " characters.")
    }
    paste0("https://", path)
  }
  
  # Shiller is required: stop on any failure instead of silently using the old file.
  shiller_url <- get_shiller_url()
  message("Shiller URL: ", shiller_url)
  ver <- str_match(shiller_url, "ver=(\\d+)")[, 2]
  if (!is.na(ver)) {
    log_line("Shiller file upload time (from ver=): ",
             format(as.POSIXct(as.numeric(ver) / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M"))
  }
  fetch_file(shiller_url, paste0(data_dir, "ie_data.xls"), method = "curl", fallback = FALSE)
  
  # ---------------------------------------------------------------------------
  # FRED downloads
  #
  # Preferred: the official FRED API (api.stlouisfed.org), using the key in
  # import/0000_credentials/fred_api_key.csv.
  # Without a key, the script uses the fredgraph.csv download instead.
  # Either way it saves a CSV with columns observation_date and the series ID,
  # the same format the rest of the script reads.
  # ---------------------------------------------------------------------------
  fred_key_path <- paste0(importdir, "/0000_credentials/fred_api_key.csv")
  
  # Read the key from the credentials file. FRED keys are 32 lowercase letters/digits,
  # so take the first cell that matches, whether or not the file has a header row.
  read_fred_key <- function(path) {
    if (!file.exists(path)) {
      log_line("FRED key file not found: ", path)
      return("")
    }
    cells <- unlist(strsplit(readLines(path, warn = FALSE), ","))
    cells <- trimws(gsub('"', "", cells))
    key <- cells[grepl("^[a-z0-9]{32}$", cells)][1]
    if (is.na(key)) {
      log_line("FRED key file found, but no 32-character key in it: ", path)
      return("")
    }
    key
  }
  
  fred_key <- read_fred_key(fred_key_path)
  # Store it as an environment variable too, so log_line() can hide it in the log
  if (nchar(fred_key) > 0) Sys.setenv(FRED_API_KEY = fred_key)
  log_line("FRED API key loaded: ", nchar(fred_key) > 0)
  
  fetch_fred <- function(series_id) {
    dest <- paste0(data_dir, series_id, ".csv")
    
    if (nchar(fred_key) == 0) {
      fetch_file(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id), dest)
      return(invisible(NULL))
    }
    
    api_url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                      series_id, "&api_key=", fred_key, "&file_type=json")
    log_line("Downloading (FRED API): ", series_id)  # URL not logged: it contains the key
    tmp <- tempfile(fileext = ".json")
    err <- NULL
    old_timeout <- options(timeout = download_timeout)
    tryCatch(download.file(api_url, tmp, mode = "wb", quiet = TRUE),
             error = function(e) err <<- sub(fred_key, "<key>", conditionMessage(e), fixed = TRUE))
    options(old_timeout)
    
    obs <- NULL
    if (is.null(err)) {
      obs <- tryCatch(jsonlite::fromJSON(tmp)$observations,
                      error = function(e) { err <<- conditionMessage(e); NULL })
    }
    n_obs <- if (is.null(obs)) 0 else nrow(obs)
    log_line("  Observations received: ", n_obs, if (!is.null(err)) paste0(" | Error: ", err) else "")
    
    if (n_obs > 0) {
      out <- data.frame(observation_date = obs$date,
                        value = suppressWarnings(as.numeric(obs$value)))  # FRED uses "." for missing
      names(out)[2] <- series_id
      write.csv(out, dest, row.names = FALSE)
      message("Downloaded: ", basename(dest))
    } else if (file.exists(dest)) {
      warning("FRED API download failed. Using the old local file: ", dest)
    } else {
      stop("FRED API download failed and no local file exists: ", dest)
    }
  }
  
  #https://fred.stlouisfed.org/series/CPIAUCNS
  fetch_fred("CPIAUCNS")
  #https://fred.stlouisfed.org/series/GS10
  fetch_fred("GS10")
  
  for (f in c("ie_data.xls", "CPIAUCNS.csv", "GS10.csv")) {
    log_line("Local file ", f, ": modified ", format(file.mtime(paste0(data_dir, f))),
             " | ", file.size(paste0(data_dir, f)), " bytes")
  }
  
  # ---------------------------------------------------------------------------
  # Read Shiller data
  # ---------------------------------------------------------------------------
  # .name_repair and progress settings stop the "New names" message and the spinner
  sp500_raw <- read_excel(paste0(data_dir, "ie_data.xls"),
                          sheet = "Data", .name_repair = "minimal", progress = FALSE)
  
  colnames(sp500_raw) <- c("date", "price", "dividend", "earnings", "cpi_shiller", "date_frac",
                           "long_irate", "real_price", "real_div", "real_tr",
                           "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2",
                           "excess_cape", "orig_nom_bond_ret", "real_bond_index")
  
  #Remove first 6 rows
  sp500_raw <- sp500_raw[7:nrow(sp500_raw),]
  
  # Convert vars to numeric (text cells in Shiller's file become NA; the warning is expected)
  suppressWarnings({
    sp500_raw$price <- as.numeric(sp500_raw$price)
    sp500_raw$dividend <- as.numeric(sp500_raw$dividend)
    sp500_raw$cpi_shiller <- as.numeric(sp500_raw$cpi_shiller)
    sp500_raw$real_div <- as.numeric(sp500_raw$real_div)
    sp500_raw$date <- as.numeric(sp500_raw$date)
    sp500_raw$orig_nom_bond_ret <- as.numeric(sp500_raw$orig_nom_bond_ret)
  })
  
  # Create a numeric end date based on the closest start of month to today's date
  end_date <- year(today) + month(today)/100
  
  # Select specific columns
  sp500_subset <- sp500_raw %>%
    select(date, price, dividend, cpi_shiller, real_div, orig_nom_bond_ret) %>%
    filter(!is.na(date), date < end_date) %>%
    mutate(orig_nom_bond_ret = orig_nom_bond_ret - 1)
  
  # Change the Date to a Date type
  sp500_subset <- sp500_subset %>%
    mutate(date = as.Date(paste0(
      substring(as.character(date), 1, 4),
      "-",
      ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
      "-01",
      "%Y-%m-%d"))) %>%
    rename(month = date,
           realDividend = real_div,
           shiller_bond_ret = orig_nom_bond_ret)
  
  yahoo_start <- max(sp500_subset$month)
  # getSymbols excludes the "to" date, so use the first day of the current month.
  # (The old "- days(1)" dropped the last trading day of the prior month.)
  yahoo_end <- as.Date(paste0(year(today), "-", month(today), "-01"))
  log_line("Shiller data ends: ", format(yahoo_start, "%Y-%m"),
           " | Yahoo fills ", format(yahoo_start, "%Y-%m-%d"), " up to (not incl.) ",
           format(yahoo_end, "%Y-%m-%d"))
  
  #Bring in Yahoo data
  getSymbols("^SPX", from = yahoo_start, to = yahoo_end,
             src="yahoo", periodicity = "daily")
  
  yahoo_daily <- data.frame(date=index(get("SPX")), coredata(get("SPX"))) %>%
    rename(close = `SPX.Adjusted`) %>%
    select(date, close) %>%
    mutate(month = as.Date(paste0(year(date), "-", month(date), "-01")))
  log_line("Yahoo rows: ", nrow(yahoo_daily), " | last date: ", format(max(yahoo_daily$date)))
  
  yahoo_monthly <- yahoo_daily %>%
    group_by(month) %>%
    summarise(price = mean(close, na.rm = TRUE)) %>%
    ungroup()
  
  # Get CPI
  cpi_monthly <- read.csv(paste0(data_dir, "CPIAUCNS.csv")) %>%
    rename(cpi_fred = `CPIAUCNS`) %>%
    mutate(month = as.Date(observation_date)) %>%
    select(month, cpi_fred)
  log_line("CPI (FRED) last month: ", format(max(cpi_monthly$month), "%Y-%m"))
  
  # Get GS10
  gs10_monthly <- read.csv(paste0(data_dir, "GS10.csv")) %>%
    rename(gs10 = `GS10`) %>%
    mutate(month = as.Date(observation_date)) %>%
    select(month, gs10)
  log_line("GS10 (FRED) last month: ", format(max(gs10_monthly$month), "%Y-%m"))
  
  # Join Shiller, Yahoo, and FRED
  sp500_ret_pe <- sp500_subset %>%
    filter(month < yahoo_start) %>%
    bind_rows(yahoo_monthly) %>%
    left_join(cpi_monthly, by = "month") %>%
    left_join(gs10_monthly, by = "month") %>%
    mutate(cpi = case_when(
      !is.na(cpi_fred) ~ cpi_fred,
      !is.na(cpi_shiller) ~ cpi_shiller,
      TRUE ~ NA
    )) %>%
    select(-cpi_shiller, -cpi_fred) %>%
    arrange(month)
  
  sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)
  sp500_ret_pe$realDividend <- na.locf(sp500_ret_pe$realDividend)
  
  #Estimate CPI data for any months Shiller/FRED is missing (use his formula)
  for(i in 1:nrow(sp500_ret_pe)){
    if(is.na(sp500_ret_pe[i, "cpi"])){
      log_line("CPI estimated for ", format(sp500_ret_pe$month[i], "%Y-%m"))
      sp500_ret_pe[i, "cpi"] <- 1.5*sp500_ret_pe[(i-1), "cpi"] - 0.5*sp500_ret_pe[(i-2), "cpi"]
    }
  }
  
  final_cpi <- sp500_ret_pe[nrow(sp500_ret_pe), "cpi"]
  
  # Calculate stock returns (same as before)
  for (i in 1:nrow(sp500_ret_pe)){
    if (i == 1){
      sp500_ret_pe[i, "n_shares"]       <- 1
      sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
      sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
      sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
      sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[i, "realPrice"]
    } else{
      sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
      sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
      sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
      sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
      sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[(i-1), "realPricePlusDividend"]*((sp500_ret_pe[i, "realPrice"] + (sp500_ret_pe[i, "realDividend"]/12))/sp500_ret_pe[(i-1), "realPrice"])
    }
  }
  
  # ---------------------------------------------------------------------------
  # Bond returns (for the stock/bond calculator)
  #
  # From 1953 on, Shiller's monthly bond return sits in the month the bond is
  # bought (the return from month t to t+1). Shift it back one row so every month
  # holds the return earned DURING that month, like the stock returns. (Shiller's
  # own real bond index already uses this timing, so the real returns below match
  # his index.)
  #
  # Months after Shiller's data ends use the 10-year Treasury yield (GS10): buy a
  # 10-year bond at par paying last month's yield, value it at this month's yield,
  # plus one month of interest. Same formula as the old stock/bond script.
  # ---------------------------------------------------------------------------
  bond_switch <- as.Date("1953-03-01")
  
  sp500_ret_pe <- sp500_ret_pe %>%
    mutate(nom_bond_ret = case_when(
      month <  bond_switch ~ shiller_bond_ret,
      month <= yahoo_start ~ lag(shiller_bond_ret),
      TRUE ~ NA_real_
    ))
  
  for (i in which(is.na(sp500_ret_pe$nom_bond_ret) & sp500_ret_pe$month >= bond_switch)) {
    y0 <- sp500_ret_pe$gs10[i - 1] / 100
    y1 <- sp500_ret_pe$gs10[i] / 100
    if (is.na(y0) || is.na(y1)) {
      stop(paste0("GS10 is missing for ", format(sp500_ret_pe$month[i], "%B %Y"),
                  " (or the month before). Check that the FRED GS10 download worked and run again."))
    }
    bond_price <- (y0 * 100) * (1 - (1 + y1)^-10) / y1 + 100 / (1 + y1)^10
    sp500_ret_pe$nom_bond_ret[i] <- (bond_price / 100 - 1) + (1 + y0)^(1/12) - 1
  }
  
  # Real bond return = nominal bond return after that month's inflation.
  # Index levels start at 1; the calculators work out monthly returns from them.
  sp500_ret_pe <- sp500_ret_pe %>%
    mutate(nom_bond_ret  = ifelse(row_number() == 1, 0, nom_bond_ret),
           real_bond_ret = ifelse(row_number() == 1, 0, (1 + nom_bond_ret) / (cpi / lag(cpi)) - 1),
           bondNominal   = cumprod(1 + nom_bond_ret),
           bondReal      = cumprod(1 + real_bond_ret))
  
  # ---------------------------------------------------------------------------
  # Output
  # ---------------------------------------------------------------------------
  to_calc <- sp500_ret_pe %>%
    filter(month >= filter_date) %>%
    select(month, price, nominalPricePlusDividend, realPrice, realPricePlusDividend,
           cpi, bondNominal, bondReal) %>%
    arrange(month)
  
  # Make sure months are in order with no gaps (the calculators find months by position)
  month_num <- year(to_calc$month) * 12 + month(to_calc$month)
  if (any(diff(month_num) != 1)) {
    bad <- which(diff(month_num) != 1)
    print(to_calc$month[sort(unique(c(bad, bad + 1)))])
    stop("Months are missing or duplicated around the dates printed above.")
  }
  
  # No missing values allowed
  missing <- names(to_calc)[colSums(is.na(to_calc)) > 0]
  if (length(missing) > 0) {
    stop(paste0("Missing values in: ", paste(missing, collapse = ", ")))
  }
  
  data_list <- list(
    start                    = format(min(to_calc$month), "%Y-%m"),
    end                      = format(max(to_calc$month), "%Y-%m"),
    # 4 decimals, same as the old S&P 500 calculator file, so its results are unchanged
    price                    = round(to_calc$price, 4),
    nominalPricePlusDividend = round(to_calc$nominalPricePlusDividend, 4),
    realPrice                = round(to_calc$realPrice, 4),
    realPricePlusDividend    = round(to_calc$realPricePlusDividend, 4),
    cpi                      = signif(to_calc$cpi, 8),
    bondNominal              = signif(to_calc$bondNominal, 10),
    bondReal                 = signif(to_calc$bondReal, 10)
  )
  
  json_data <- toJSON(data_list, auto_unbox = TRUE, digits = NA)
  
  writeLines(paste0("window.SP500_DATA = ", json_data, ";"),
             paste0(out_path, "/sp500_data.js"))
  log_line("Wrote sp500_data.js: ", data_list$start, " to ", data_list$end,
           " | ", nrow(to_calc), " months")
  
  # Quick look at the last few months
  print(tail(sp500_ret_pe %>% select(month, price, cpi, gs10, nom_bond_ret, real_bond_ret), 4))
  print(paste0("Data end month = ", data_list$end))
  print(paste0("Shiller end month = ", format.Date(yahoo_start, "%m/%Y")))
  
  log_line("Run finished OK")
  
  # ############################  End  ################################## #
},
# Copy every message, warning and error to the log (they still show in the console)
message = function(m) log_line("MESSAGE: ", trimws(conditionMessage(m))),
warning = function(w) log_line("WARNING: ", conditionMessage(w), short_call(w)),
error   = function(e) log_line("ERROR: ", conditionMessage(e), short_call(e))
),
finally = {
  # Always close the log, even if the script stops with an error
  sink()
  log_line("Log closed")
  close(log_con)
  cat("Log written to:", log_path, "\n")
})