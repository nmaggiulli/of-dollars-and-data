cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
# Set dataroot location
dataroot <- "/Volumes/GoogleDrive/My Drive/of_dollars_and_data/"

# Set dataset libraries
localdir <- paste0(dataroot, "datasets/")

# Set the import/export directories
importdir <- paste0(dataroot, "import/")
exportdir <- paste0(dataroot, "export/")

of_dollars_and_data_theme <- theme(
  plot.title       = element_text(family = "my_font", size = 14, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 10, 0)),
  axis.title.y     = element_text(face = "bold", size = 10, family = "my_font", margin = ggplot2::margin(0, 10, 0, 0)),
  axis.text.y      = element_text(color = "black"), 
  axis.ticks.y     = element_line(color = "black"),
  axis.text.x      = element_text(color = "black"),
  axis.ticks.x     = element_line(color = "black"),
  axis.title.x     = element_text(face = "bold", size = 10, family = "my_font", margin = ggplot2::margin(10, 0, 0, 0)),
  axis.line.x      = element_line(color = "black"),
  axis.line.y      = element_line(color = "black"),
  legend.key       = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border     = element_blank(),
  panel.background = element_blank(),
  plot.caption     = element_text(hjust = 0, family = "my_font", size = 8))

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
        if (grepl("pct_|_pct", col_name)){
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

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(lubridate)
library(quantmod)
library(tidyverse)

folder_name <- "xxxx_yahoo_cef_pull"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

pull_and_chart_cef <- function(start_dt, end_dt, ticker){
  getSymbols(ticker, from = start_dt, to = end_dt, 
             src="yahoo", periodicity = "daily") 
  
  xticker <- paste0("X", ticker, "X")
  
  getSymbols(xticker, from = start_dt, to = end_dt, 
             src="yahoo", periodicity = "daily") 
  
  x_raw <- data.frame(date=index(get(xticker)), coredata(get(xticker))) %>%
    rename_(.dots = setNames(paste0(xticker, ".Close"), "x_close")) %>%
    select(date, x_close)
  
  raw <- data.frame(date=index(get(ticker)), coredata(get(ticker))) %>%
    rename_(.dots = setNames(paste0(ticker, ".Close"), "close")) %>%
    rename_(.dots = setNames(paste0(ticker, ".Adjusted"), "adj")) %>%
          left_join(x_raw) %>%
          select(date, close, adj, x_close) 
  
  df <- raw %>%
        mutate(premium_discount = close/x_close - 1,
               bucket = case_when(
                 premium_discount < -0.05 ~ "<-5%",
                 premium_discount < 0 ~ "-5% to 0%",
                 premium_discount < 0.05 ~ "0% to 5%",
                 TRUE ~ ">5%"
               ),
               bucket_order = case_when(bucket == "<-5%" ~ 1,
                                        bucket == "-5% to 0%" ~ 2,
                                        bucket == "0% to 5%" ~ 3,
                                        TRUE ~ 4),
               next_day_ret = lead(adj)/adj)
  
  export_to_excel(df, paste0(out_path, "/", ticker, "_daily_data.xlsx"), sheetname="daily", 1, 0)
  
  all <- df %>%
          summarize(count = n(),
                    sumproduct = prod(next_day_ret, na.rm = TRUE),
                    ret_ann = sumproduct^(252/count)-1) %>%
          mutate(bucket = "All")
  
  final_results <- df %>%
                    group_by(bucket, bucket_order) %>%
                    summarize(count = n(),
                              sumproduct = prod(next_day_ret, na.rm = TRUE),
                              ret_ann = sumproduct^(252/count)-1) %>%
                    ungroup() %>%
                    arrange(bucket_order) %>%
                    select(bucket, ret_ann, sumproduct, count) %>%
                    bind_rows(all)
  
  export_to_excel(df, paste0(out_path, "/", ticker, "_daily_data.xlsx"), sheetname="table_results", 0, 0)
  
  to_plot <- df
  
  file_path <- paste0(out_path, "/", ticker, "_premium_discount.jpeg")
  
  plot <- ggplot(to_plot, aes(x=date, y=premium_discount)) +
    geom_line(col = "blue") +
    scale_y_continuous(label = percent, limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, 0.05)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Premium / Discount to NAV")) +
    labs(x = "Date", y="Premium / Discount")
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  file_path <- paste0(out_path, "/", ticker, "_annualized_ret.jpeg") 
  
  plot <- ggplot(final_results, aes(x=factor(bucket, levels = c("<-5%", "-5% to 0%", "0% to 5%", ">5%", "All")), y=ret_ann)) +
    geom_bar(stat="identity", fill = "blue") +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_y_continuous(label = percent, limits = c(-0.4, 0.2), breaks = seq(-0.4, 0.2, 0.05)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Annualized Return")) +
    labs(x = "Bucket", y="Annualized Return")
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

pull_and_chart_cef(start_dt = as.Date("1999-03-04"),
                   end_dt = as.Date("2018-12-10"),
                   "GIM")

# ############################  End  ################################## #
