cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(ggrepel)
library(lubridate)
library(Quandl)
library(tidyverse)

folder_name <- "0093_best_stock_predictor"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

type <- "aaii"

# Delete summary file
if (file.exists(paste0(out_path, "/all_model_summaries_", type, ".xlsx"))){
  file.remove(paste0(out_path, "/all_model_summaries_", type, ".xlsx"))
}

starting_value <- 1

aaii_raw <- read_excel(paste0(importdir, "0093_best_stock_predictor/aaii_allocation_survey.xls"))

# Create a date list to use for subsetting
end_month <- floor_date(Sys.Date(), "month")
all_dates <- seq.Date(as.Date("1987-12-01"), end_month, "month")-1

# Subset based on known properties of file
aaii <- aaii_raw[3:(length(all_dates)+2), 2:6]

colnames(aaii) <- c("stock_funds", "stocks", "bond_funds", "bonds", "cash")  

aaii <- aaii %>%
  mutate(date = as.POSIXct(all_dates),
         stock_allocation = as.numeric(stocks) + as.numeric(stock_funds),
         bond_allocation = as.numeric(bonds) + as.numeric(bond_funds),
         cash_allocation = as.numeric(cash)) %>%
  select(date, stock_allocation, bond_allocation, cash_allocation)

# Merge and subset
raw <- read_excel(paste0(importdir, "0093_best_stock_predictor/sp500_5yr_treasury_data.xlsx"), sheet = "DFA_PeriodicReturns_20180913112") %>%
          inner_join(aaii)

n_years_lookahead <- 10
lookahead_ret_string <- paste0("ret_forward_", n_years_lookahead, "yr")


for (i in 1:(nrow(raw) - n_years_lookahead*12)){
  raw[i, lookahead_ret_string] <- prod(1 + raw[i:(i + n_years_lookahead*12-1), "sp500"])^(1/n_years_lookahead) - 1
}

l_reg <- lm(ret_forward_10yr ~ stock_allocation, data = raw)
summary(l_reg)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/equity_allocation_and_forward_returns.jpeg")

# Set note and source string
source_string <- str_wrap("Source: AAII, DFA, 1987-2018 (OfDollarsAndData.com)",
                          width = 85)

# Plot the allocation vs. future returns
plot <- ggplot(raw, aes(x = stock_allocation, y = ret_forward_10yr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Higher Investor Allocation to Equities\nCorresponds to Lower Future Returns")) +
  labs(x = "Average Investor Allocation to Equities" , y = "Future 10-Year Returns (Annualized)",
       caption = paste0("\n", source_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# Model to test trend in AAII
trend_aaii <- function(upper_cutoff, lower_cutoff, model_name, start_date, end_date){
  
  df <- filter(raw, date >= start_date, date <= end_date)
  
  # Create subfolder for the model name
  dir.create(file.path(paste0(out_path, "/", model_name)), showWarnings = FALSE)
  
  # Loop through data to run model
  for (i in 1:nrow(df)){
    if (i == 1){
      df[i, "in_out"] <- 1
      df[i, "value_trend"] <- starting_value
      df[i, "value_bh"] <- starting_value
      df[i, "ret_trend"]   <- NA
      df[i, "ret_bh"]   <- NA
      df[i, "buy_sell"] <- 0
    } else{
      if (df[i, "stock_allocation"] > upper_cutoff & df[(i-1), "in_out"] == 1){
        df[i, "in_out"] <- 0
      } else if (df[i, "stock_allocation"] < lower_cutoff & df[(i-1), "in_out"] == 0){
        df[i, "in_out"] <- 1
      } else {
        df[i, "in_out"] <- df[(i-1), "in_out"]
      }
      
      if (df[i, "in_out"] != df[(i-1), "in_out"]){
        df[i, "buy_sell"] <- df[(i-1), "buy_sell"] + 1
      } else {
        df[i, "buy_sell"] <- df[(i-1), "buy_sell"]
      }
      
      df[i, "value_trend"] <- (df[(i-1), "value_trend"] * (1 + df[i, "sp500"]) * df[(i-1), "in_out"]) -
        (df[(i-1), "value_trend"] * (1 + df[i, "treasury_5yr"])  * (df[(i-1), "in_out"] - 1))
      df[i, "value_bh"] <- df[(i-1), "value_bh"] * (1 + df[i, "sp500"])
      
      df[i, "ret_trend"] <- df[i, "value_trend"]/df[(i-1), "value_trend"] - 1
      df[i, "ret_bh"] <- df[i, "value_bh"]/df[(i-1), "value_bh"] - 1
    }
  }
  
  final_trend <- df[nrow(df), "value_trend"]
  final_bh <- df[nrow(df), "value_bh"]
  
  final_df <- df
  
  # Export full data
  export_to_excel(final_df, 
                  paste0(out_path, "/", model_name, "/all_raw_data_", model_name, ".xlsx"), 
                  "raw_data",
                  1,
                  0)
  
  final_df <- final_df %>%
    mutate(trend_3m = value_trend/lag(value_trend, 3) - 1,
           bh_3m = value_bh/lag(value_bh, 3) - 1,
           trend_6m = value_trend/lag(value_trend, 6) - 1,
           bh_6m = value_bh/lag(value_bh, 6) - 1)
  
  # Calculate summary stats to be used later
  # Calculate ret and sd
  ret_monthly    <- mean(final_df$ret_trend, na.rm =  TRUE)
  sd_monthly     <- sd(final_df$ret_trend, na.rm =  TRUE)
  ret_monthly_bh <- mean(final_df$ret_bh, na.rm =  TRUE)
  sd_monthly_bh  <- sd(final_df$ret_bh, na.rm =  TRUE)
  
  worst_trend_3m <- min(final_df$trend_3m, na.rm = TRUE)
  worst_bh_3m <- min(final_df$bh_3m, na.rm = TRUE)
  worst_trend_6m <- min(final_df$trend_6m, na.rm = TRUE)
  worst_bh_6m <- min(final_df$bh_6m, na.rm = TRUE)
  
  # Calculate the number of  buys and sells
  n_buys_sells <- max(final_df$buy_sell)
  total_months <- nrow(final_df)
  
  # Create perf/vol measure
  perf_trend <- ((final_df[nrow(final_df), "value_trend"]/final_df[1, "value_trend"])^(1/nrow(final_df)))^12 - 1
  vol_trend  <- sd(final_df$ret_trend, na.rm = TRUE)
  
  perf_bh <- ((final_df[nrow(final_df), "value_bh"]/final_df[1, "value_bh"])^(1/nrow(final_df)))^12 - 1
  vol_bh  <- sd(final_df$ret_bh, na.rm = TRUE)
  
  #Calculate upside downside capture
  upside_trend <- final_df %>%
    filter(ret_bh > 0) %>%
    summarize(up_down_ratio = sum(ret_trend, na.rm = TRUE)/sum(ret_bh, na.rm = TRUE)) %>%
    pull()
  
  downside_trend <- final_df %>%
    filter(ret_bh < 0) %>%
    summarize(up_down_ratio = sum(ret_trend, na.rm = TRUE)/sum(ret_bh, na.rm = TRUE)) %>%
    pull()
  
  # Plot worst monthly returns for Trend and Buy and Hold
  worst_10_trend <- final_df %>%
    select(date, ret_trend) %>%
    arrange(ret_trend) %>%
    mutate(model = "AvgEquityShare") %>%
    rename(monthly_return_pct = ret_trend) %>%
    head(10)
  
  worst_10_bh <- final_df %>%
    select(date, ret_bh) %>%
    arrange(ret_bh) %>%
    mutate(model = "Buy and Hold") %>%
    rename(monthly_return_pct = ret_bh) %>%
    head(10)
  
  models <- c("AvgEquityShare", "Buy and Hold")
  
  for (m in models){
    to_plot <- bind_rows(worst_10_trend, worst_10_bh) %>%
      mutate(yr_mo = paste0(month(date), "/", substring(year(date), 3, 4))) %>%
      select(-date) %>%
      filter_(paste0("model == '", m, "'"))
    
    # Set the file_path based on the function input 
    file_path <- paste0(out_path, "/", model_name, "/worst_returns_", m, "_", model_name, ".jpeg")
    
    # Set note and source string
    source_string <- str_wrap("Source: AAII, DFA (OfDollarsAndData.com)",
                              width = 85)
    
    # Create distribution of GT vs. Buy and Hold
    plot <- ggplot(to_plot, aes(x=reorder(yr_mo, monthly_return_pct), y=monthly_return_pct)) +
      geom_bar(stat = "identity", fill = chart_standard_color) +
      scale_y_continuous(label = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(data = to_plot, 
                aes(reorder(yr_mo, monthly_return_pct), monthly_return_pct + (0.005 * sign(monthly_return_pct)), label = paste0(round(100*monthly_return_pct, 0), "%")),
                col = "black", 
                size = 4) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Worst 10 Monthly Returns\n", m)) +
      labs(x = "Month/Year" , y = "Monthly Return",
           caption = paste0("\n", source_string))
    
    # Turn plot into a gtable for adding text grobs
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    
    # Save the plot
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  }
  
  print("Worst returns plotted")
  
  # Plot the distributions of 1y, 3y, 5y returns and heatmaps
  for (i in c(1, 3, 5)){
    n_months_lookback <- i * 12
    to_plot <- final_df %>%
      mutate(ret_trend_annual = (value_trend/lag(value_trend, n_months_lookback))^(1/i) - 1,
             ret_bh_annual = (value_bh/lag(value_bh, n_months_lookback))^(1/i) - 1,
             month = month(date),
             year = year(date)) %>%
      select(date, month, year, ret_trend_annual, ret_bh_annual) %>%
      filter(!is.na(ret_trend_annual), !is.na(ret_bh_annual)) %>%
      rename(`Buy and Hold` = ret_bh_annual,
             `AvgEquityShare` = ret_trend_annual) %>%
      gather(key=key, value=value, -date, -month, -year)
    
    pct_neg_trend <- nrow(filter(to_plot, key=="AvgEquityShare", value < 0))/nrow(filter(to_plot, key=="AvgEquityShare"))
    pct_neg_bh <- nrow(filter(to_plot, key=="Buy and Hold", value < 0))/nrow(filter(to_plot, key=="Buy and Hold"))
    
    # Set the file_path based on the function input 
    file_path <- paste0(out_path, "/", model_name, "/dists_", i, "y_", model_name, ".jpeg")
    
    # Set note
    note_string   <- str_wrap(paste0("Note:  ", 100*round(pct_neg_trend, 3), "% of AvgEquityShare's ", i, "-year returns are negative compared to ", 
                                     100*round(pct_neg_bh, 3), "% for Buy and Hold."),
                              width = 85)
    
    # Create distribution of GT vs. Buy and Hold
    plot <- ggplot(to_plot, aes(value, fill = key)) +
      geom_density(alpha = 0.4) +
      scale_fill_discrete(guide = guide_legend()) +
      theme(legend.position="bottom") +
      scale_x_continuous(label = percent, limits = c(-0.5, 0.5)) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Distribution of ", i, "-Year Annualized Returns")) +
      labs(x = "Annualized Return" , y = "Frequency",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Turn plot into a gtable for adding text grobs
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    
    # Save the plot
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    
    print(paste0(i, "-Year Dist plotted!"))
    
    # Set the file_path based on the function input 
    file_path <- paste0(out_path, "/", model_name, "/heatmap_", i, "y_", model_name, ".jpeg")
    
    to_heatmap <- filter(to_plot, key == "AvgEquityShare") %>%
      select(month, year, value) %>%
      rename(`Annualized Return` = value)
    
    # Create heatmap
    plot <- ggplot(to_heatmap, aes(x=month, y=factor(year, levels=rev(sort(unique(to_heatmap$year)))))) +
      geom_tile(aes(fill=`Annualized Return`)) +
      geom_text(aes(label = paste0(100*round(`Annualized Return`, 2), "%")), size = 2) +
      scale_fill_gradient(low = "red", high = "green", guide = FALSE) +
      scale_x_continuous(breaks = seq(1, 12)) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Heatmap of AvgEquityShare ", i, "-Year\nAnnualized Returns")) +
      labs(x = "Month" , y = "Year",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Turn plot into a gtable for adding text grobs
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    
    # Save the plot
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    
    print(paste0(i, "-Year Heatmap created!"))
    
    # Plot monthly returns in the same way
    if (i == 1){
      # Plot the rolling 1-yr return as well
      file_path <- paste0(out_path, "/", model_name, "/rolling_1yr_", model_name, ".jpeg")
      
      to_plot2 <- filter(to_plot, key == "AvgEquityShare") %>%
        select(date, value) %>%
        rename(`Annualized Return` = value)
      
      plot <- ggplot(to_plot2, aes(x=date, y=`Annualized Return`)) +
        geom_line() +
        of_dollars_and_data_theme +
        scale_y_continuous(label = percent, limits = c(-0.5, 0.5)) +
        ggtitle(paste0("AvgEquityShare Rolling ", i, "-Year\nAnnualized Returns")) +
        labs(x = "Date" , y = "1-Year Annualized Return",
             caption = paste0("\n", source_string, "\n", note_string))
      
      # Turn plot into a gtable for adding text grobs
      my_gtable   <- ggplot_gtable(ggplot_build(plot))
      
      # Save the plot
      ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
      
      print("1 year rolling returns plotted")
      
      # Set the file_path based on the function input 
      file_path <- paste0(out_path, "/", model_name, "/heatmap_monthly_", model_name, ".jpeg")
      
      to_heatmap <- final_df %>%
        filter(!is.na(ret_trend)) %>%
        mutate(month=month(date), 
               year=year(date)) %>%
        select(month, year, ret_trend) %>%
        rename(`Monthly Return` = ret_trend)
      
      pct_neg_trend <- nrow(filter(to_heatmap, `Monthly Return` < 0))/nrow(to_heatmap)
      
      note_string   <- str_wrap(paste0("Note:  ", 100*round(pct_neg_trend, 3), "% of AvgEquityShare's monthly returns are negative."),
                                width = 85)
      
      # Create heatmap
      plot <- ggplot(to_heatmap, aes(x=month, y=factor(year, levels=rev(sort(unique(to_heatmap$year)))))) +
        geom_tile(aes(fill=`Monthly Return`)) +
        geom_text(aes(label = paste0(100*round(`Monthly Return`, 2), "%")), size = 2) +
        scale_fill_gradient(low = "red", high = "green", guide = FALSE) +
        scale_x_continuous(breaks = seq(1, 12)) +
        of_dollars_and_data_theme +
        ggtitle(paste0("Heatmap of AvgEquityShare Monthly Returns")) +
        labs(x = "Month" , y = "Year",
             caption = paste0("\n", source_string, "\n", note_string))
      
      # Turn plot into a gtable for adding text grobs
      my_gtable   <- ggplot_gtable(ggplot_build(plot))
      
      # Save the plot
      ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    } else if (i == 3){
      to_plot <- final_df
      
      for (i in 1:nrow(to_plot)){
        if (i >= n_months_lookback){
          to_plot[i, "sd_3yr"] <- sd(pull(to_plot[(i-n_months_lookback+1):i, "ret_trend"]), na.rm = TRUE)
        }
      }
      
      to_plot <- filter(to_plot, !is.na(sd_3yr))
      
      # Set the file_path based on the function input 
      file_path <- paste0(out_path, "/", model_name, "/sd_3yr_", model_name, ".jpeg")
      
      # Create heatmap
      plot <- ggplot(to_plot, aes(x=date, y=sd_3yr)) +
        geom_line() +
        scale_y_continuous(label = percent) +
        of_dollars_and_data_theme +
        ggtitle(paste0("3-Year Rolling Standard Deviation\nOf Monthly AvgEquityShare Returns")) +
        labs(x = "Date" , y = "Standard Deviation Over Prior 3 Years",
             caption = paste0("\n", source_string))
      
      # Turn plot into a gtable for adding text grobs
      my_gtable   <- ggplot_gtable(ggplot_build(plot))
      
      # Save the plot
      ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
    }
  }
  
  print("Return distributions plotted")
  
  # Get drawdown datasets
  vp_trend <- drawdown_path(select(final_df, date, value_trend)) %>%
    mutate(key="AvgEquityShare")
  vp_bh <- drawdown_path(select(final_df, date, value_bh)) %>%
    mutate(key="Buy and Hold")
  
  # Plot drawdowns
  to_plot <- bind_rows(vp_trend, vp_bh)
  
  dd <- to_plot[with(to_plot, order(key, rev(date))), ]
  
  for (i in 1:nrow(dd)){
    if (i == 1){
      dd[i, "group"] <- 1
    } else {
      key <- dd[i, "key"]
      p_key <- dd[(i-1), "key"] 
      pct <- dd[i, "pct"]
      
      if(key == p_key & pct != 0){
        dd[i, "group"] <- dd[(i-1), "group"]
      } else {
        dd[i, "group"] <- dd[(i-1), "group"] + 1
      }
    }
  }
  
  trend_low_dd <- dd %>%
    filter(key == "AvgEquityShare") %>%
    group_by(key, group) %>%
    summarize(min_dd = min(pct)) %>%
    ungroup() %>%
    arrange(key, min_dd) %>%
    head(3)
  
  bh_low_dd <- dd %>%
    filter(key == "Buy and Hold") %>%
    group_by(key, group) %>%
    summarize(min_dd = min(pct)) %>%
    ungroup() %>%
    arrange(key, min_dd) %>%
    head(3)
  
  max_dd_trend <- min(vp_trend$pct, na.rm = TRUE)
  max_dd_bh <- min(vp_bh$pct, na.rm = TRUE)
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/", model_name, "/dd_", model_name, ".jpeg")
  
  # Set source
  source_string <- str_wrap(paste0("Source:  AAII, DFA (OfDollarsAndData.com)"),
                            width = 85)
  
  # Set note
  note_string   <- str_wrap(paste0("Note:  The largest drawdown for AvgEquityShare is ", 100*round(max_dd_trend, 3), "% compared to 
                                   ", 100*round(max_dd_bh, 3), "% for Buy and Hold."),
                            width = 85)
  
  plot <- ggplot(to_plot, aes(x = date, y = pct, fill = key)) +
    geom_area(position = "identity", alpha = 0.4) +
    scale_y_continuous(label = percent, limits = c(-0.75, 0)) +
    of_dollars_and_data_theme +
    ggtitle("Drawdowns for AvgEquityShare vs.\nBuy and Hold") +
    labs(x = "Date" , y = "Percentage of Value Lost",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  print("Drawdowns plotted")
  
  # Plot model vs. buy and hold
  to_plot <- final_df %>%
    select(date, value_trend, value_bh)
  
  to_plot[, "AvgEquityShare"]     <- to_plot[, "value_trend"]
  to_plot[, "Buy and Hold"] <- to_plot[, "value_bh"]
  
  to_plot <- to_plot %>%
    select(-value_trend, -value_bh) %>%
    gather(key=key, value=value, -date)
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/", model_name, "/trend_v_bh_", model_name, ".jpeg")
  
  note_string <- str_wrap(paste0("Note:  AvgEquityShare had ",  n_buys_sells, " transactions across ", total_months, " months."))
  
  # Create plot of GT vs. Buy and Hold
  plot <- ggplot(to_plot, aes(x=date, y = value, col = key)) +
    geom_line() +
    scale_color_discrete(guide = guide_legend()) +
    theme(legend.position="bottom") +
    geom_text_repel(data = filter(to_plot, date == max(to_plot$date)),
                    aes(x = date,
                        y = value,
                        col = key,
                        label = dollar(value),
                        family = "my_font"
                    ), size = 4,
                    max.iter = 4000) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle("AvgEquityShare vs. Buy and Hold") +
    labs(x = "Date" , y = "Growth of $1",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  print("Value paths plotted")
  print("All plotting complete!")
  
  model_summary <- data.frame(
    Statistic = c("Growth of $1",
                  "Perf/Vol",
                  "Worst Drawdown",
                  "2nd Worst Drawdown",
                  "3rd Worst Drawdown",
                  "Avg Monthly Return",
                  "Standard Deviation",
                  "Upside Capture",
                  "Downside Capture",
                  "Min 1-Month Return",
                  "25th Percentile Monthly Return",
                  "50th Percentile Monthly Return",
                  "75th Percentile Monthly Return",
                  "Max Month Return",
                  "Worst 3-Month Return",
                  "Worst 6-Month Return",
                  "Number of Buys and Sells",
                  "Total Buys and Sells / Total Months"),
    AvgEquityShare = c(paste0("$", round(as.numeric(final_df[nrow(final_df), "value_trend"]), 2)),
                   round(as.numeric(perf_trend/vol_trend), 1),
                   paste0(100*round(trend_low_dd[1, "min_dd"], 3), "%"),
                   paste0(100*round(trend_low_dd[2, "min_dd"], 3), "%"),
                   paste0(100*round(trend_low_dd[3, "min_dd"], 3), "%"),
                   paste0(100*round(ret_monthly, 3), "%"),
                   paste0(100*round(sd_monthly, 3), "%"),
                   paste0(100*round(upside_trend, 3)),
                   paste0(100*round(downside_trend, 3)),
                   paste0(100*round(min(final_df$ret_trend, na.rm = TRUE), 3), "%"),
                   paste0(100*round(quantile(final_df$ret_trend, probs = 0.25, na.rm = TRUE), 3), "%"),
                   paste0(100*round(quantile(final_df$ret_trend, probs = 0.5, na.rm = TRUE), 3), "%"),
                   paste0(100*round(quantile(final_df$ret_trend, probs = 0.75, na.rm = TRUE), 3), "%"),
                   paste0(100*round(max(final_df$ret_trend, na.rm = TRUE), 3), "%"),
                   paste0(100*round(worst_trend_3m, 3), "%"),
                   paste0(100*round(worst_trend_6m, 3), "%"),
                   paste0(n_buys_sells),
                   paste0(100*round(n_buys_sells/total_months, 3), "%")
    ),
    `Buy and Hold` = c(paste0("$", round(as.numeric(final_df[nrow(final_df), "value_bh"]), 2)),
                       round(as.numeric(perf_bh/vol_bh), 1),
                       paste0(100*round(bh_low_dd[1, "min_dd"], 3), "%"),
                       paste0(100*round(bh_low_dd[2, "min_dd"], 3), "%"),
                       paste0(100*round(bh_low_dd[3, "min_dd"], 3), "%"),
                       paste0(100*round(ret_monthly_bh, 3), "%"),
                       paste0(100*round(sd_monthly_bh, 3), "%"),
                       paste("100"),
                       paste("100"),
                       paste0(100*round(min(final_df$ret_bh, na.rm = TRUE), 3), "%"),
                       paste0(100*round(quantile(final_df$ret_bh, probs = 0.25, na.rm = TRUE), 3), "%"),
                       paste0(100*round(quantile(final_df$ret_bh, probs = 0.5, na.rm = TRUE), 3), "%"),
                       paste0(100*round(quantile(final_df$ret_bh, probs = 0.75, na.rm = TRUE), 3), "%"),
                       paste0(100*round(max(final_df$ret_bh, na.rm = TRUE), 3), "%"),
                       paste0(100*round(worst_bh_3m, 3), "%"),
                       paste0(100*round(worst_bh_6m, 3), "%"),
                       paste("0"),
                       paste("0%")
    )
  ) 
  
  # Create overall summary to compare Trend sensitivities
  if(file.exists(paste0(out_path, "/all_model_summaries_", type, ".xlsx"))){
    new_file <- 0
  } else {
    new_file <- 1
  }
  
  # Export the model summary to Excel
  export_to_excel(model_summary,
                  paste0(out_path, "/all_model_summaries_", type, ".xlsx"),
                  sheetname = model_name,
                  new_file,
                  1)
}

sd_upper1 <- mean(aaii$stock_allocation) + sd(aaii$stock_allocation)

trend_aaii(0.7, 0.50, "70_upper_50_lower", "1900-01-01", "2100-01-01")
trend_aaii(0.7, 0.50, "70_upper_50_lower_dotcom", "1995-01-01", "2003-12-31")

# ############################  End  ################################## #