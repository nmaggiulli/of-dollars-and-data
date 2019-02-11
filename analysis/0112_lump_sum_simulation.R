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
          mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
          filter(date >= "1960-01-01")

first_yr <- min(year(raw$date))
last_yr <- max(year(raw$date))

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

dd <- drawdown_path(select(raw, date, index_sp500)) %>%
        mutate(dd_cat = as.factor(case_when(
          pct > -0.05 ~ 0,
          pct < -0.2 ~ 2,
          TRUE ~ 1
          ))
        )

# Grab only the index columns for the matrix
raw_matrix <- as.matrix(raw[, grepl("index_", colnames(raw))])

sp_col_num <- grepl("sp500", colnames(raw_matrix))
treasury_col_num <- grepl("treasury", colnames(raw_matrix))

run_lump_sum_simulation <- function(n_month_dca, pct_sp500, y_unit){
  
  if(n_month_dca < 10){
    n_month_string <- paste0("0", n_month_dca)
  } else{
    n_month_string <- paste0(n_month_dca)
  }
  
  pct_treasury    <- 100 - pct_sp500
  weight_sp500    <- pct_sp500/100
  weight_treasury <- 1 - weight_sp500
  
  if(weight_sp500 > 1 | weight_sp500 < 0){
    stop("Weights do not equal 1!")
  }
  
  final_results <- data.frame(date = raw[1:(nrow(raw) - n_month_dca), "date"])

  for(i in 1:(length(raw_matrix[,1]) - n_month_dca)){
    
    end_row_num <- i + n_month_dca
    
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
  
  # Define an out folder to delete and re-create
  out_folder <- paste0(out_path,"/time_sw_", pct_sp500)
  
  # Plot the LS outperformance
  file_path <- paste0(out_folder,"/dca_perf_time_sw_", pct_sp500, "_", n_month_string, "_m.jpeg")
  
  to_plot <- final_results %>%
              rename_(.dots = setNames(paste0(dca_col), "dca_col")) %>%
              left_join(dd)
  
  avg_performance <- mean(to_plot$dca_col, na.rm = TRUE)
  pct_underperformance <- (to_plot %>% filter(dca_col < 0) %>% nrow())/nrow(to_plot)
  
  if(avg_performance > 0){
    perf_string <- "outperforms"
  } else{
    perf_string <- "underperforms"
    avg_performance <- abs(avg_performance)
  }
  
  source_string <- paste0("Source:  DFA, ", first_yr, "-", last_yr, " (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: 'Stocks' are represented by the S&P 500 and 'Bonds' are represented by 5-Year U.S. Treasuries.  ",
                                 "The S&P 500 return includes dividends, but is not adjusted for inflation.  ",
                                 "On average, DCA (over ", n_month_dca, " months) ", perf_string, " Lump Sum by ", round(100*avg_performance, 1), "% and ",
                                 "underperforms Lump Sum in ", round(100*pct_underperformance, 1), "% of all months shown."), 
                          width = 80)
  
  if(y_unit == 1){
    y_break <- 0.25
  } else{
    y_break <- 0.1
  }
  
  # Set the title
  if(pct_sp500 < 100){
    title_portfolio_string <- paste0(pct_sp500, "/", pct_treasury, " Portfolio")
  } else if (pct_sp500 == 100){
    title_portfolio_string <- paste0("All Stock Portfolio")
  } else if (pct_sp500 == 0){
    title_portfolio_string <- paste0("All Bond Portfolio")
  }
  
  text_labels <- data.frame()
  text_labels[1, "dca_col"] <- y_unit * 0.95
  text_labels[1, "label"] <- "DCA Outperforms Lump Sum"
  text_labels[1, "date"] <- as.Date("1990-01-01")
  text_labels[1, "dd_cat"] <- 1
  text_labels[2, "dca_col"] <- -y_unit * 0.95
  text_labels[2, "label"] <- "DCA Underperforms Lump Sum"
  text_labels[2, "date"] <- as.Date("1990-01-01")
  text_labels[2, "dd_cat"] <- 1
  
  plot <- ggplot(to_plot, aes(x=date, y=dca_col)) +
    geom_hline(yintercept = 0, col = "black") +
    geom_line() +
    geom_text_repel(data=text_labels, aes(x=date, y=dca_col),
                    color = "black",
                    label = text_labels$label,
                    max.iter = 1) +
    scale_y_continuous(label = percent, limits = c(-y_unit, y_unit),
                       breaks = seq(-y_unit, y_unit, y_break)) +
    scale_x_date(date_labels = "%Y", breaks = c(
      as.Date("1960-01-01"),
      as.Date("1970-01-01"),
      as.Date("1980-01-01"),
      as.Date("1990-01-01"),
      as.Date("2000-01-01"),
      as.Date("2010-01-01")
    )) +
    of_dollars_and_data_theme +
    ggtitle(paste0("DCA Performance Over ", n_month_dca, " Months\nvs. Lump Sum Investment\n",
                   title_portfolio_string)) +
    labs(x = "Date", y="DCA Outperformance (%)",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  out_folder <- paste0(out_path,"/dist_sw_", pct_sp500)
  
  # Plot distribution of returns as well
  file_path <- paste0(out_folder, "/dca_dist_sw_", pct_sp500, "_", n_month_string, "_m.jpeg")
  
  plot <- ggplot(to_plot, aes(x=dca_col)) +
    geom_density(fill = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    scale_fill_discrete(guide = FALSE) +
    scale_x_continuous(label = percent, limits = c(-y_unit, y_unit),
                       breaks = seq(-y_unit, y_unit, y_break)) +
    of_dollars_and_data_theme +
    theme(axis.ticks.y     = element_blank(),
          axis.text.y     = element_blank()) +
    ggtitle(paste0("DCA Performance Over ", n_month_dca, " Months\nvs. Lump Sum Investment\n",
                   title_portfolio_string)) +
    labs(x = "DCA Outperformance (%)", y="Frequency",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  return(final_results)
}

remove_and_recreate_folder <- function(path){
  unlink(path)
  dir.create(file.path(paste0(path)), showWarnings = FALSE)
}

months_to_run <- seq(2, 60, 1)
results <- data.frame()

for (j in c(60, 100)){
  
  y_unit_max <- 0.5
  
  folder_time <- paste0(out_path, "/time_sw_", j)
  folder_dist <- paste0(out_path, "/dist_sw_", j)
  
  remove_and_recreate_folder(folder_time)
  remove_and_recreate_folder(folder_dist)
  
  for(m in months_to_run){
    tmp <- run_lump_sum_simulation(m, j, y_unit_max)
    
    if(m == months_to_run[1]){
      results <- tmp
      
    } else{
      results <- results %>%
                          left_join(tmp)
    }
  }
  
  assign(paste0("final_results_", j, "_sw"), results, envir = .GlobalEnv)
  
  create_gif(folder_time,
             paste0("dca_perf_time_sw_", j,"_*.jpeg"),
             40,
             0,
             paste0("_gif_dca_outperformance_sw_", j, ".gif"))

  create_gif(folder_dist,
             paste0("dca_dist_sw_", j,"_*.jpeg"),
             40,
             0,
             paste0("_gif_dca_dist_sw_", j,".gif"))
}


# ############################  End  ################################## #