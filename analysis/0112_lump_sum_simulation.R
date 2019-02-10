cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "0112_lump_sum_simulation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Bring in DFA data
raw <- read.csv(paste0(importdir, "/0112_dfa_sp500_treasury_5yr/DFA_PeriodicReturns_20190210111032.csv"),
               skip = 7) %>%
        select(-4)

colnames(raw) <- c("date", "ret_sp500", "ret_treasury_5yr")

# Filter out bad rows from import
raw <- raw %>%
          filter(!is.na(ret_sp500)) %>%
          mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Get growth of $1 from returns
for (i in 1:nrow(raw)){
  if(i == 1){
    raw[i, "index_sp500"]        <- 1 * (1 + raw[i, "ret_sp500"])
    raw[i, "index_treasury_5yr"] <- 1 * (1 + raw[i, "ret_treasury_5yr"])
  } else{
    raw[i, "index_sp500"] <- raw[(i-1), "index_sp500"] * (1 + raw[i, "ret_sp500"])
    raw[i, "index_treasury_5yr"] <- raw[(i-1), "index_treasury_5yr"] * (1 + raw[i, "ret_treasury_5yr"])
  }
}

# Grab only the index columns for the matrix
raw_matrix <- as.matrix(raw[, grepl("index_", colnames(raw))])

sp_col_num <- grepl("sp500", colnames(raw_matrix))
treasury_col_num <- grepl("treasury", colnames(raw_matrix))

run_lump_sum_simulation <- function(n_month_dca, weight_sp500, y_unit){
  
  if(n_month_dca < 10){
    n_month_string <- paste0("0", n_month_dca)
  } else{
    n_month_string <- paste0(n_month_dca)
  }
  
  weight_treasury <- 1 - weight_sp500
  
  if(weight_sp500 > 1 | weight_sp500 < 0){
    stop("Weights do not equal 1!")
  }
  
  final_results <- data.frame(date = raw[1:(nrow(raw) - n_month_dca), "date"])

  for(i in 1:(length(raw_matrix[,1]) - n_month_dca)){
    
    end_row_num <- i+n_month_dca
    
    beg_sp500 <- raw_matrix[i, sp_col_num]
    beg_treasury <- raw_matrix[i, treasury_col_num]
    
    end_sp500 <- raw_matrix[end_row_num, sp_col_num]
    end_treasury <- raw_matrix[end_row_num, treasury_col_num]
    
    end_ls <- (end_sp500/beg_sp500 * weight_sp500) + (end_treasury/beg_treasury * weight_treasury)
    
    dca_sp500 <- sum(end_sp500/raw_matrix[i:(end_row_num-1), sp_col_num] * (1/n_month_dca) * weight_sp500)
    dca_treasury <- sum(end_treasury/raw_matrix[i:(end_row_num-1), treasury_col_num] * (1/n_month_dca) * weight_treasury)
    
    end_dca <- dca_sp500 + dca_treasury
    
    dca_col <- paste0("dca_outperformance_", n_month_dca, "m")
    
    final_results[i, dca_col] <- end_dca/end_ls - 1
  }
  
  # Plot the LS outperformance
  file_path <- paste0(out_path, "/dca_outperformance_over_time_sw_", 100*weight_sp500, "_", n_month_string, "_m.jpeg")
  
  source_string <- paste0("Source:  DFA (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: The 60/40 Portfolio is 60% the S&P 500 and 40% 5-Year Treasuries. ",
                                 "The S&P 500 return includes dividends, but it not adjusted for inflation."), 
                          width = 80)
  
  to_plot <- final_results
  
  avg_outperformance <- mean(to_plot[which(to_plot[, dca_col] > 0), dca_col], na.rm = TRUE)
  avg_performance <- mean(to_plot[, dca_col], na.rm = TRUE)
  avg_underperformance <- mean(to_plot[which(to_plot[, dca_col] < 0), dca_col], na.rm = TRUE)
  
  plot <- ggplot(to_plot, aes_string(x="date", y=dca_col)) +
    geom_hline(yintercept = 0, col = "black") +
    geom_line() +
    geom_hline(yintercept = avg_outperformance, col = "green", linetype= "dashed") +
    geom_hline(yintercept = avg_performance, col = "blue", linetype= "dashed") +
    geom_hline(yintercept = avg_underperformance, col = "red", linetype= "dashed") +
    scale_y_continuous(label = percent, limits = c(-y_unit, y_unit),
                       breaks = seq(-y_unit, y_unit, 0.1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("DCA Outperformance over ", n_month_dca, " Months\nvs. Lump Sum Investment")) +
    labs(x = "Date", y="DCA Outperformance (%)")
    
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  return(final_results)
}

months_to_run <- seq(2, 24, 1)
final_results_60_40 <- data.frame()
final_results_all_stock <- data.frame()

for(m in months_to_run){
  tmp_60_40 <- run_lump_sum_simulation(m, 0.6, 0.7)
  tmp_all_stock <- run_lump_sum_simulation(m, 1, 1)
  
  if(m == months_to_run[1]){
    final_results_60_40 <- tmp_60_40
    final_results_all_stock <- tmp_all_stock
  } else{
    final_results_60_40 <- final_results_60_40 %>%
                      left_join(tmp_60_40)
    final_results_all_stock <- final_results_all_stock %>%
      left_join(tmp_all_stock)
  }
}

create_gif(out_path, 
           paste0("dca_outperformance_over_time_sw_60_*.jpeg"), 
           40, 
           0, 
           paste0("_gif_dca_outperformance_sw_60.gif"))

create_gif(out_path, 
           paste0("dca_outperformance_over_time_sw_100_*.jpeg"), 
           40, 
           0, 
           paste0("_gif_dca_outperformance_sw_100.gif"))



# ############################  End  ################################## #