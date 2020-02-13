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
library(zoo)
library(tidyverse)

folder_name <- "0164_ls_v_dca_multi_asset"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

remove_and_recreate_folder <- function(path){
  unlink(path)
  dir.create(file.path(paste0(path)), showWarnings = FALSE)
}

SentCase <- function(InputString){
  InputString <-
    paste(toupper(substring(InputString,1,1)),tolower(substring(InputString,2)),
          sep="")
}

ProperCase <- function(InputString){
  sapply(lapply(strsplit(InputString," "), SentCase), paste, collapse=" ")
}

plot_ls_v_dca <- function(asset, f_out, in_df, var, var_note){
  
  file_path <- paste0(f_out,"/ls_v_dca_", var, "_", n_month_dca, "m.jpeg")
  
  if(var != "outperformance"){
  to_plot <- in_df %>%
    rename_(.dots = setNames(paste0("ls_", var, "_", n_month_dca, "m"), "Lump Sum")) %>%
    rename_(.dots = setNames(paste0("dca_", var, "_", n_month_dca, "m"), "DCA")) %>%
    select(date, `Lump Sum`, `DCA`) %>%
    gather(-date, key=key, value=value)
  
    avg_ls <- to_plot %>% filter(key == "Lump Sum") %>% summarize(value = mean(value)) %>% pull(value)
    avg_dca <- to_plot %>% filter(key == "DCA") %>% summarize(value = mean(value)) %>% pull(value)
    
    note_string <- str_wrap(paste0("Note: On average, the DCA strategy has a ", var_note, " of ", 100*round(avg_dca, 3), 
                                   "%  while Lump Sum has a ", var_note, " of ", 100*round(avg_ls, 3), "% over the time period shown."), 
                            width = 80)
  } else if (var == "outperformance"){
    to_plot <- in_df %>%
      rename_(.dots = setNames(paste0("dca_outperformance_", n_month_dca, "m"), "perf_col"))
    
    avg_performance <- mean(to_plot$perf_col, na.rm = TRUE)
    pct_underperformance <- (to_plot %>% filter(perf_col < 0) %>% nrow())/nrow(to_plot)
    
    if(avg_performance > 0){
      perf_string <- "outperforms"
    } else{
      perf_string <- "underperforms"
      avg_performance <- abs(avg_performance)
    }
    
    note_string <- str_wrap(paste0("On average, DCA (over ", n_month_dca, " months) ", perf_string, " Lump Sum by ", round(100*avg_performance, 1), "% and ",
                                   "underperforms Lump Sum in ", round(100*pct_underperformance, 1), "% of all months shown."), 
                            width = 80)
  }
  
  first_yr <- min(year(to_plot$date))
  last_yr <- max(year(to_plot$date))
  
  source_string <- paste0("Source:  YCharts, ", first_yr, "-", last_yr, " (OfDollarsAndData.com)")
  
  if(var != "outperformance"){
    plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
      geom_line() +
      scale_color_manual(values = c("black", "blue")) +
      scale_y_continuous(label = percent_format(accuracy = 0.1)) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0(ProperCase(var_note), " For ", n_month_dca, "-Month DCA\nvs. Lump Sum Investment\n", asset)) +
      labs(x = "Date", y=ProperCase(var_note),
           caption = paste0(source_string, "\n", note_string))
  } else if (var == "outperformance"){
    
    n_row <- nrow(in_df)
    mid_date <- in_df[ceiling(n_row/2), "date"]
    y_max <- max(to_plot[, "perf_col"])
    y_min <- min(to_plot[, "perf_col"])
    
    text_labels <- data.frame()
    text_labels[1, "perf_col"] <- y_max * 1.25
    text_labels[1, "label"] <- "DCA Outperforms Lump Sum"
    text_labels[1, "date"] <- mid_date
    text_labels[2, "perf_col"] <- y_min * 1.25
    text_labels[2, "label"] <- "DCA Underperforms Lump Sum"
    text_labels[2, "date"] <- mid_date
    
    text_labels <- text_labels %>%
      mutate(date = as.Date(date))
    
    plot <- ggplot(to_plot, aes(x=date, y=perf_col)) +
              geom_hline(yintercept = 0, col = "black") +
              geom_line() +
              geom_text_repel(data=text_labels, aes(x=date, y=perf_col),
                              color = "black",
                              label = text_labels$label,
                              family = "my_font",
                              max.iter = 1) +
              scale_y_continuous(label = percent_format(accuracy = 1), limits = c(y_min*1.4, y_max*1.4)) +
              of_dollars_and_data_theme +
              ggtitle(paste0("DCA Performance Over ", n_month_dca, " Months\nvs. Lump Sum Investment\n", asset)) +
              labs(x = "Date", y="DCA Outperformance (%)",
                   caption = paste0(source_string, "\n", note_string))
  }
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

raw <- read.csv(paste0(importdir, "0164_ycharts_multi_asset/timeseries_1-22-2020.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  filter(symbol != "", !is.na(value)) %>%
  arrange(name, date) %>%
  select(date, name, value) %>%
  mutate(ret = ifelse(name == lag(name), value/lag(value) - 1, NA)) %>%
  filter(!is.na(ret))

port_6040 <- raw %>%
              filter(name %in% c("S&P 500 Total Return", "Bloomberg Barclays US Treasury")) %>%
              mutate(ret = ifelse(name == "S&P 500 Total Return", 0.6*ret, 0.4*ret)) %>%
              group_by(date) %>%
              summarize(ret = sum(ret)) %>%
              ungroup() %>%
              mutate(name = "Portfolio 60-40")

for(i in 1:nrow(port_6040)){
  if(i == 1){
    port_6040[i, "value"] <- 1
  } else{
    port_6040[i, "value"] <- port_6040[(i-1), "value"] * (1 + port_6040[i, "ret"])
  }
}

df <- raw %>%
        bind_rows(port_6040)

all_assets <- unique(df$name)

for(a in all_assets){
  print(a)
  
  filtered <- df %>%
                  filter(name == a)
                  
  raw_matrix <- as.matrix(filtered[, grepl("value|ret", colnames(filtered))])
  
  n_month_dca <- 24
  
  asset_result <- data.frame(date = filtered[1:(nrow(filtered) - n_month_dca), "date"])
  
  for(i in 1:(length(raw_matrix[,1]) - n_month_dca)){
    
    end_row_num <- i + n_month_dca
    
    beg_asset    <- raw_matrix[i, 1]
    
    end_asset   <- raw_matrix[end_row_num, 1]
    
    end_ls <- end_asset/beg_asset
    
    # Calculate the monthly lump sum returns
    ls_ret <- raw_matrix[i:(end_row_num-1), 2]
    
    # Now calculate DCA
    asset_growth  <- end_asset/raw_matrix[i:(end_row_num-1), 1]
    
    # Get final DCA sum
    end_dca <- sum(asset_growth * (1/n_month_dca))
    
    # Calculate the monthly DCA returns
    dca_ret <- raw_matrix[i:(end_row_num-1), 2] * (seq(1, n_month_dca)/n_month_dca)
    
    ls_col  <- paste0("ls_", n_month_dca, "m")
    dca_col <- paste0("dca_", n_month_dca, "m")
    out_col <- paste0("dca_outperformance_", n_month_dca, "m")
    under_col <- paste0("dca_underperformed_", n_month_dca, "m")
    
    ls_r <- paste0("ls_ret_", n_month_dca, "m")
    dca_r <- paste0("dca_ret_", n_month_dca, "m")
    
    ls_sd <- paste0("ls_sd_", n_month_dca, "m")
    dca_sd <- paste0("dca_sd_", n_month_dca, "m")
    
    ls_sharpe <- paste0("ls_sharpe_", n_month_dca, "m")
    dca_sharpe <- paste0("dca_sharpe_", n_month_dca, "m")
    
    asset_result[i, "asset"] <- a
    asset_result[i, ls_col] <- end_ls - 1
    asset_result[i, dca_col] <- end_dca - 1
    asset_result[i, out_col] <- end_dca/end_ls - 1
    asset_result[i, under_col] <- ifelse(end_dca < end_ls, 1, 0)

    asset_result[i, ls_r] <- (prod(1 + ls_ret)^(1/n_month_dca)) - 1
    asset_result[i, dca_r] <- (prod(1 + dca_ret)^(1/n_month_dca)) - 1
    
    asset_result[i, ls_sd] <- sd(ls_ret)
    asset_result[i, dca_sd] <- sd(dca_ret)
    
    asset_result[i, ls_sharpe] <- asset_result[i, ls_r]/sd(ls_ret)
    asset_result[i, dca_sharpe] <- asset_result[i, dca_r]/sd(dca_ret)
  }
  
  if(a == all_assets[1]){
   final_results <- asset_result 
  } else{
    final_results <- bind_rows(final_results, asset_result) 
  }
  
  # Define an out folder to delete and re-create
  out_folder <- paste0(out_path,"/", a)
  
  remove_and_recreate_folder(out_folder)
  
  plot_ls_v_dca(a, out_folder, asset_result, "ret", "monthly return")
  plot_ls_v_dca(a, out_folder, asset_result, "sd", "standard deviation")
  plot_ls_v_dca(a, out_folder, asset_result, "sharpe", "Sharpe Ratio")
  plot_ls_v_dca(a, out_folder, asset_result, "outperformance", "DCA outperformance")
}

#Plot S&P 500 downside vs. 60/40 Downside
to_plot <- final_results %>%
            filter(asset %in% c("S&P 500 Total Return", "Portfolio 60-40")) %>%
            select(date, asset, contains("_sd_")) %>%
            mutate(value = ifelse(asset == "Portfolio 60-40", ls_sd_24m, dca_sd_24m)) %>%
            select(date, asset, value)

file_path <- paste0(out_path,"/sp500_dca_vs_6040_ls_sd_", n_month_dca, "m.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = asset)) +
  geom_line() +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Standard Deviation For\n", n_month_dca, "-Month DCA into S&P 500 vs.\nLump Sum into 60/40")) +
  labs(x = "Date", y=("Standard Deviation"),
       caption = paste0("Source:  YCharts (OfDollarsAndData.com)"))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- final_results %>%
  filter(asset %in% c("S&P 500 Total Return", "Portfolio 60-40")) %>%
  select(date, asset, contains("ret")) %>%
  mutate(value = ifelse(asset == "Portfolio 60-40", ls_ret_24m, dca_ret_24m)) %>%
  select(date, asset, value)

file_path <- paste0(out_path,"/sp500_dca_vs_6040_ls_ret_", n_month_dca, "m.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = asset)) +
  geom_line() +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Monthly Return For\n", n_month_dca, "-Month DCA into S&P 500 vs.\nLump Sum into 60/40")) +
  labs(x = "Date", y=("Monthly Return"),
       caption = paste0("Source:  YCharts (OfDollarsAndData.com)"))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")


final_summary <- final_results %>%
                    group_by(asset) %>%
                    summarize(dca_underperformance = -1*mean(dca_outperformance_24m),
                              dca_underperformed_months = mean(dca_underperformed_24m)) %>%
                    ungroup()

export_to_excel(final_summary, 
                paste0(out_path, "/_final_dca_underperformance_summary.xlsx"),
                sheetname = "final_summary",
                1,
                0)

# Plot sample LS vs. DCA
amount <- 24000

to_plot <- data.frame(period = rep(seq(1, n_month_dca), 2),
                      value = c(amount, rep(0, n_month_dca-1),
                                     rep(amount/n_month_dca, n_month_dca)),
                      key = c(rep("Lump Sum", n_month_dca),
                              rep("Dollar Cost Averaging", n_month_dca))
)

file_path <- paste0(out_path,"/_ls_vs_dca_example.jpeg")

plot <- ggplot(to_plot, aes(x=period, y=value)) +
  geom_bar(stat="identity", fill = "blue") +
  facet_grid(key ~ .) +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 24, 3)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("The Difference Between LS and DCA")) +
  labs(x = "Period", y=("Amount Invested per Period"),
       caption = paste0("Source:  Simulated data (OfDollarsAndData.com)"))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #