cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(readxl)
library(tidyverse)

folder_name <- "0182_dow_forward_rets"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow <- read_excel(paste0(importdir, "0172_daily_dow/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index"))

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            rename(index = price_plus_div) %>%
            select(date, index) 

#Bring in Japan data
nkk_yahoo <- read.csv(paste0(importdir, "0182_japan_nikkei/nikkei_225_yahoo.csv")) %>%
  rename(index = `Adj.Close`) %>%             
   mutate(date = as.Date(Date)) %>%
                select(date, index)

nkk <- read.csv(paste0(importdir, "0182_japan_nikkei/nikk.csv"), skip = 1) %>%
        rename(index = Close) %>%
        mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
        select(date, index) %>%
        bind_rows(nkk_yahoo)

fwd_df <- data.frame(fwd_days =  c(1, 5, 20, 125, 250, 1250, 2500, 5000),
                     fwd_months =  c(0, 0, 1, 6, 12, 60, 120, 240),
                     label =  c("1D", "5D", "1M", "6M", "1Y", "5Y", "10Y", "20Y")
)

plot_fwd_rets <- function(df, outname, sourcename){
  
  first_year <- year(min(df$date))
  last_year <- year(max(df$date))
  
  file_out <- substitute(df)
  
  for(f in 1:nrow(fwd_df)){
    lbl <- fwd_df[f, "label"]
    
  if(outname == "Dow"){
    fwd <- fwd_df[f, "fwd_days"]
    if(fwd < 250){
      exp <- 1
    } else{
      exp <- 1/(fwd/250)
    }
  lower_y <- -0.2
  } else{
    fwd <- fwd_df[f, "fwd_months"]
    if(fwd < 12){
      exp <- 1
    } else{
      exp <- 1/(fwd/12)
    }
  lower_y <- -0.25  
  }

  upper_y <- -1*lower_y  
    
  tmp <- df %>%
          mutate(fwd_ret = (lead(index, fwd)/index)^(exp) - 1,
                 fwd_pos = ifelse(fwd_ret > 0, 1, 0),
                 label = lbl)
    
    pos <- tmp %>%
            group_by(label) %>%
            summarise(pct_pos = mean(fwd_pos, na.rm = TRUE)) %>%
            ungroup()
    
    pos_neg <- tmp %>%
      filter(!is.na(fwd_pos)) %>%
      group_by(label, fwd_pos) %>%
      summarise(ret = mean(fwd_ret, na.rm = TRUE)) %>%
      ungroup()
            
    if(f == 1){
      final_results <- pos
      final_pos_neg <- pos_neg
      final_df <- tmp
    } else{
      final_results <- bind_rows(final_results, pos)
      final_pos_neg <- bind_rows(final_pos_neg, pos_neg)
      final_df <- bind_rows(final_df, tmp)
    }
  }
  
  final_results$label <- factor(final_results$label, levels = fwd_df$label)
  final_pos_neg$label <- factor(final_pos_neg$label, levels = fwd_df$label)
  final_df$label <- factor(final_df$label, levels = fwd_df$label)
  
  # Print useful stats for post
  print(final_results)
  print(final_pos_neg)
  
  if(outname == "U.S. Stocks"){
    note_upper <- paste0("Note:  Return data includes dividends and is adjusted for inflation.  ")
  } else{
    note_upper <- paste0("Note: ", outname, " data does not include dividends.  ")
  }
  
  # Plot pct positive
  to_plot <- final_results

  file_path <- paste0(out_path, "/pct_pos_", file_out, ".jpeg")
  source_string <- str_wrap(paste0("Source:  ", sourcename, ", ", first_year, "-", last_year," (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0(note_upper),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=label, y=pct_pos)) +
    geom_bar(stat="identity", fill = chart_standard_color) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Percentage of Periods Where ", outname, "\nHad a Positive Return")) +
    labs(x = "Holding Period" , y = "Percentage of Time ", outname, " is Higher",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Plot return pos_neg
  to_plot <- final_pos_neg %>%
              mutate(text_label = paste0(100*round(ret, 2), "%"),
                     hjust_values = case_when(
                       fwd_pos == 1 & nchar(text_label) > 2 ~ -0.2,
                       fwd_pos == 1 & nchar(text_label) <= 2 ~ -0.45,
                       fwd_pos == 0 & nchar(text_label) > 3 ~ 1.2,
                       fwd_pos == 0 & nchar(text_label) <= 3 ~ 1.3,
                       TRUE ~ 0
                     ))
  
  file_path <- paste0(out_path, "/ret_pos_neg_", file_out, ".jpeg")
  note_string <- str_wrap(paste0(note_upper,
                                 "Forward returns for all periods greater than 1 year are annualized."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=label, y=ret, fill = as.factor(fwd_pos))) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(data=to_plot, aes(x=label, y=ret, label = text_label),
              col = "white",
              size = 2.3,
              vjust = to_plot$fwd_pos,
              hjust = to_plot$hjust_values) +
    scale_fill_manual(values = c("black", "red"), guide = FALSE) +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(lower_y, upper_y), breaks = seq(lower_y, upper_y, 0.05)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(outname, " Average Period Return When\nReturn is Positive or Negative")) +
    labs(x = "Holding Period" , y = "Period Return",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Plot the boxplot
  to_plot <- final_df
  
  file_path <- paste0(out_path, "/", file_out, "_box_plot.jpeg")
  note_string <- str_wrap(paste0(note_upper,
                                 "Forward returns for all periods greater than 1 year are annualized."),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=label, y=fwd_ret)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.05, 0.2)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(outname, " Forward Returns by Holding Period")) +
    labs(x = "Holding Period" , y = "Forward Return",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_fwd_rets(dow, "Dow", "Bloomberg")
plot_fwd_rets(shiller, "U.S. Stocks", "Shiller Data, http://www.econ.yale.edu/~shiller/data.htm")
plot_fwd_rets(nkk, "Nikkei", "Bloomberg, Yahoo Finance")




# ############################  End  ################################## #