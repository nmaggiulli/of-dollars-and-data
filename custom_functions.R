# Function for variable frequency
freq_table <- function(df, varname, na_rm){
  if (na_rm == 1){
    df <- df[!is.na(df[, varname]), ]
  }
  round(table(df[, varname])/nrow(df), 2)
}

freq_plot <- function(df, varname, outfile, title, source_text, note_text){
  require(ggplot2)
  source_string <- str_wrap(source_text, width = 85)
  note_string <- str_wrap(note_text, width = 85)
  
  if(typeof(df[, varname]) == "character" | is.factor(df[, varname])){
    plot <- ggplot(df, aes_string(varname)) +
      geom_bar(fill = chart_standard_color) +
      rwm_theme +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
      ggtitle(title) +
      labs(x = varname, y = "Frequency",
           caption = paste0("\n", source_string, "\n", note_string))
  } else {
    plot <- ggplot(df, aes_string(varname)) +
      geom_density(fill = chart_standard_color) +
      rwm_theme +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
      ggtitle(title) +
      labs(x = varname, y = "Frequency",
           caption = paste0("\n", source_string, "\n", note_string))
  }
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save plot
  ggsave(outfile, my_gtable, width = 15, height = 12, units = "cm")
}

# Function to get the percentage of NAs for each column in a data frame
pct_missings <- function(df){
  sapply(df, function(x) round(sum(is.na(x))/nrow(df), 2))
}

# Function to export Excel tables (with formatting if needed)
export_to_excel <- function(df, outfile, sheetname, new_file, fancy_formatting){
  require(openxlsx)
  
  # Remove file if new_file is set to 1, otherwise, add a new sheet
  if (new_file == 1){
    if(file.exists(outfile)) file.remove(outfile)
    wb <- createWorkbook()
  } else {
    wb <- loadWorkbook(outfile)
  }
  
  addWorksheet(wb, sheetname)
  
  # Add highlighting and column formatting if needed
  if (fancy_formatting == 1){
    # Set the style for each individual cell
    for (j in 1:nrow(df)){
      if (j %% 2 != 0){
        highlight_fill <- highlight_standard_color
      } else {
        highlight_fill <- "white"
      }
      
      if (j == nrow(df)){
        border_string <- "bottom"
      } else {
        border_string <- NULL
      }
      
      for (i in 1:ncol(df)){
        col_type <- sapply(df, class)[i]
        col_name <- colnames(df)[i]
        if (grepl("pct_|_pct|Percentage|%", col_name)){
          s1 <- createStyle(numFmt = "PERCENTAGE", 
                            halign = "center", 
                            fgFill = highlight_fill, 
                            border = border_string)
        }
        else if (col_type == "integer" | col_type == "numeric"){
          s1 <- createStyle(numFmt = "#,##0", 
                            halign = "center", 
                            fgFill = highlight_fill,
                            border = border_string)
        }
        else if (col_type == "character"){
          s1 <- createStyle(numFmt = "TEXT", 
                            halign = "left", 
                            fgFill = highlight_fill,
                            border = border_string)
        }
        else if (col_type == "date"){
          s1 <- createStyle(numFmt = "DATE", 
                            halign = "center", 
                            fgFill = highlight_fill,
                            border = border_string)
        } else{
          s1 <- createStyle(numFmt = "TEXT", 
                            halign = "left", 
                            fgFill = highlight_fill,
                            border = border_string)
        }
        
        addStyle(wb, sheetname, style = s1, rows=j+1, cols = i)
      }
    }
  }
  
  # freeze the top pane and set automatic column widths
  freezePane(wb, sheet = sheetname, firstRow = TRUE)
  setColWidths(wb, sheet = sheetname, cols = 1:ncol(df), widths = "auto")
  
  # Add header style and filter
  hs1 <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "bottom")
  addFilter(wb, sheetname, row = 1, cols = 1:ncol(df))
  
  # Write out data
  writeData(wb, sheetname, df, startRow =1, startCol = 1, headerStyle = hs1)
  saveWorkbook(wb, outfile, TRUE)
}

# Function to clean columns to a standard underscore lowercase format (used for Salesforce and Orion data)
clean_cols <- function(df){
  # Get the varnames
  var_names <- colnames(df)
  
  # Clean the varnames
  for (i in 1:length(var_names)){
    var_names[i] <- str_replace_all(tolower(var_names[i]), "\\.|\\s|\\/", "_")
  }
  
  # Replace the names with the clean names
  colnames(df) <- var_names
  
  return(df)
}

# Function to fix dates from Salesforce
fix_salesforce_date <- function(x){
  mdy(paste0(gsub("(\\d\\d?)/\\d\\d?/.*", "\\1", x), "-",
             gsub("\\d\\d?/(\\d\\d?)/.*", "\\1", x), "-",
             gsub("\\d\\d?/\\d\\d?/(.*)", "\\1", x)))
}

convert_date_to_monthly <- function(dt__1){
  dt__2 <- as.Date(paste0(year(dt__1), "-", month(dt__1), "-01"))
  return(dt__2)
}

# Create GIF function accepts path, filename, and speed argument
# Before using on Mac you will need to install homebrew and imagemagick
# homebrew: /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# then: echo "eval $(/opt/homebrew/bin/brew shellenv)" >> ~/.zshrc
# then: brew install imagemagick
create_gif <- function(path, file_stub, speed_milliseconds, n_loops = 0, out_name = "all_plots.gif"){
  path <- gsub(" ", "\\\ ", path, fixed = TRUE)
  system(paste0("/opt/homebrew/bin/convert -delay ", speed_milliseconds, " -loop ", n_loops, " ", path, "/", file_stub ," ",  path, "/", out_name))
}

# Create function to calculate the drawdowns over time
drawdown_path <- function(vp, dd_counts = 0){
  
  vp <- as.data.frame(vp)
  
  dd_dates <- vp[, 1]
  dd_index <- as.vector(vp[, 2])
  
  dd_pct <- c()
  
  loc_max <- 0
  for (i in 1:length(dd_index)){
    if (dd_index[i] < loc_max & i != 1){
      dd_pct[i] <- dd_index[i]/loc_max - 1
    } else{
      dd_pct[i] <- 0
      loc_max  <- dd_index[i]
    }
  }
  
  dd <- data.frame(date = dd_dates, pct = dd_pct)
  if(dd_counts == 0){
    return(dd)
  } else{
    dd_counter_ <- 0
    for(d in 1:nrow(dd)){
      if(dd[d, "pct"] == 0){
        dd_counter_ <- dd_counter_ + 1
      }
      dd[d, "dd_count"] <- dd_counter_
    }
    return(dd)
  }
}

add_dd_counter <- function(df){
  dd_counter <- 1
  for(i in 1:nrow(df)){
    if(i == 1){
      df[i, "dd_counter"] <- dd_counter    
    } else{
      if(df[i, "pct"] == 0){
        dd_counter <- dd_counter + 1
        df[i, "dd_counter"] <- dd_counter
      } else{
        df[i, "dd_counter"] <- dd_counter
      }
    }
  }
  return(df)
}

date_to_string <- function(x){
  require(stringr)
  str_replace_all(paste0(x), "-", "_")
}

round_to_nearest <- function(num, round_direction, unit){
  final <- round(num/unit)*unit
  
  if(round_direction == "up" & final < num){
    final <- final + unit
  } else if (round_direction == "down" & final > num){
    final <- final - unit
  }
  
  return(final)
} 

roman_numerals <- c("I", "II", "III", "IV", "V")

# Capitalize first letter of a word unless it's a Roman numeral
sent_case <- function(word) {
  if (toupper(word) %in% roman_numerals) {
    return(toupper(word))
  } else {
    return(paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2))))
  }
}

# Apply sent_case to each part of a hyphenated word
properize_word <- function(word) {
  parts <- strsplit(word, "-", fixed = TRUE)[[1]]
  parts_proper <- sapply(parts, sent_case)
  paste(parts_proper, collapse = "-")
}

# Apply properize_word to each space-separated word
proper_case <- function(input_string) {
  words <- strsplit(input_string, " ", fixed = TRUE)[[1]]
  words_proper <- sapply(words, properize_word)
  paste(words_proper, collapse = " ")
}

format_as_dollar <- function(NumberInput, Digits=0){
  if(NumberInput >= 0){
    return(paste0("$", formatC(NumberInput, digits = Digits, format = "f", big.mark = ",")))
  } else{
    return(paste0("-$", formatC(-1*NumberInput, digits = Digits, format = "f", big.mark = ",")))
  }
}

import_ycharts_timeseries <- function(path){
  tmp <- read.csv(paste0(path),
           skip = 6) %>%
    select(-Metric, -Name) %>%
    rename(symbol = Symbol) %>%
    gather(-symbol, key=key, value=value) %>%
    mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
           month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
           day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
           date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
    arrange(date)
  
  return(tmp)
}

date_to_month <- function(date){
  month_dt <- as.Date(paste0(year(date), "-", month(date), "-01"))
  
  return(month_dt)
}

dollar_to_numeric <- function(dollar_str) {
  # Remove $ and , then convert to numeric
  as.numeric(gsub("[$,]", "", dollar_str))
}
