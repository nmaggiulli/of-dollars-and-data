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
library(FinCal)
library(tidyverse)

folder_name <- "0132_avoiding_volatility"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read.csv(paste0(importdir, "/0132_spx/SPX_data.csv"),
                col.names = c("date", "index_sp500")) %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  filter(year < 2019) %>%
  arrange(date)

all_years <- unique(spx$year)
first_yr <- all_years[1]
last_yr <- all_years[length(all_years)]

for(i in 1:length(all_years)){
  yr <- all_years[i]
  
  dd_yr <- drawdown_path(filter(spx, year(date) == yr))
  
  dd_min <- dd_yr %>% 
              filter(pct == min(dd_yr$pct)) %>% 
              mutate(year = year(date),
                     min_dd = pct) %>%
              select(year, min_dd)
  if(i == 1){
    dd_all <- dd_min
  } else{
    dd_all <- bind_rows(dd_all, dd_min)
  }
}

# Set file path
file_path <- paste0(out_path, "/dd_intrayear_max.jpeg")

to_plot <- dd_all
avg_intrayear_dd <- mean(to_plot$min_dd)
median_intrayear_dd <- quantile(to_plot$min_dd, probs = 0.5, na.rm = TRUE)

# Set source/note
source_string <- paste0("Source:  YCharts, ", first_yr, "-", last_yr, " (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Does not adjust for inflation or dividends.  ",
                                 "The average intrayear maximum drawdown is ", 
                                 100*round(avg_intrayear_dd, 3), 
                                 "% and the median intrayear maximum drawdown is ",
                                 100*round(median_intrayear_dd, 3),
                                 "%."), 
                          width = 85)

plot <- ggplot(to_plot, aes(x=year, y=min_dd)) +
  geom_bar(stat="identity", fill = "red") +
  scale_y_continuous(label = percent, limits = c(-0.6, 0)) +
  of_dollars_and_data_theme +
  ggtitle("Maximum Intrayear Drawdown by Year") +
  labs(x="Year", y="Percentage Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now bring in the total return stock and bond data
df <- read.csv(paste0(importdir, "0132_spx/dfa_sp500_5yr.csv"), skip = 7,
                col.names =  c("date", "ret_sp500", "ret_5yr", "drop")) %>%
      filter(!is.na(ret_sp500)) %>%
      mutate(date = as.Date(date, format = "%m/%d/%Y"),
             year = year(date)) %>%
      filter(year(date) < 2019, year(date) >= min(spx$year)) %>%
      select(-drop) %>%
      left_join(dd_all)

yearly_rets <- df %>%
                group_by(year, min_dd) %>%
                summarize(year_ret_sp500 = prod(1+ret_sp500) - 1,
                          year_ret_5yr = prod(1+ret_5yr) - 1) %>%
                ungroup() %>%
                left_join(df) %>%
                mutate(min_dd = min_dd*-1)

# Set file path
file_path <- paste0(out_path, "/dd_vs_annual_ret_stock.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts, DFA ", first_yr, "-", last_yr, " (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Drawdowns are determined by changes in the S&P 500 price level.  Performance shown is total return."),
                          width = 85)

# Plot Yearly returns versus Max intrayear drawdowns
plot<- ggplot(yearly_rets, aes(x=min_dd, y=year_ret_sp500)) + 
        geom_hline(yintercept = 0) +
        geom_point() + 
        geom_smooth(method = "lm", se=FALSE) +
        scale_x_continuous(label = percent, limits = c(0, 0.5)) +
        scale_y_continuous(label = percent, limits = c(-0.5, 0.5)) +
        of_dollars_and_data_theme +
        ggtitle(paste0("Maximum Drawdown vs. Annual Stock Returns\n", first_yr, "-", last_yr)) +
        labs(x="S&P 500 Maximum Intrayear Drawdown", y="Annual Stock Return",
             caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Set file path
file_path <- paste0(out_path, "/dd_vs_annual_ret_bond.jpeg")

# Plot Yearly returns versus Max intrayear drawdowns
plot<- ggplot(yearly_rets, aes(x=min_dd, y=year_ret_5yr)) + 
  geom_hline(yintercept = 0) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(label = percent, limits = c(0, 0.5)) +
  scale_y_continuous(label = percent, limits = c(-0.5, 0.5)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("When Stocks Fall, Bonds Tend to Compensate\n", first_yr, "-", last_yr)) +
  labs(x="S&P 500 Maximum Intrayear Drawdown", y="Annual Bond Return",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Function for calculating performance with the genie
calculate_genie_performance <- function(dd_pct){
  df_in_out <- df %>%
                  mutate(in_out = ifelse(min_dd < dd_pct, 0, 1))

  for(i in 1:nrow(df_in_out)){
    ret_sp500 <- df_in_out[i, "ret_sp500"]
    ret_5yr <- df_in_out[i, "ret_5yr"]
    in_out <- df_in_out[i, "in_out"]
    
    if(i == 1){
      df_in_out[i, "value_bh"] <- 1 * (1 + ret_sp500)
      df_in_out[i, "value_ad"] <- (1 * in_out * (1 +ret_sp500)) + 
                                    (1 * (1 - in_out) * (1 + ret_5yr))
    } else{
      df_in_out[i, "value_bh"] <- df_in_out[(i-1), "value_bh"] * (1 + ret_sp500)
      df_in_out[i, "value_ad"] <- (df_in_out[(i-1), "value_ad"] * in_out * (1 +ret_sp500)) + 
                                    (df_in_out[(i-1), "value_ad"] * (1 - in_out) * (1 + ret_5yr))
    }
  }
  return(df_in_out)
}

for(j in seq(-0.40, -0.05, 0.01)){
  raw_perf <- calculate_genie_performance(j)
  dd_pos <- -100*j
  
  print(paste0("Avoiding ", dd_pos, "% Drawdowns produces a final value of ", 
               round(raw_perf[nrow(raw_perf), "value_ad"]/raw_perf[nrow(raw_perf), "value_bh"], 2), 
               " times greater than Buy & Hold"))
  
    pct_in <- mean(raw_perf$in_out)
    years_in <-sum(raw_perf$in_out)/12
    years_total <- nrow(raw_perf)/12
    
    to_plot <- raw_perf %>%
                select(date, value_bh, value_ad) %>%
                rename(`Buy & Hold` = value_bh,
                       `Avoid Drawdowns` = value_ad) %>%
                gather(-date, key=key, value=value)
    
    # Set ymax manually
    ymax <- 18000
    
    dates <- filter(raw_perf) %>%
      select(date, in_out) %>%
      mutate(end_date = lead(date) - 1) %>%
      filter(in_out == 0) %>%
      left_join(to_plot) %>%
      filter(key == "Avoid Drawdowns") %>%
      mutate(key = "In Bonds") %>%
      select(date, end_date, key, value) 
    
    dd_string <- paste0(str_pad(dd_pos, width = 2, side = "left"), "_pct")
    
    # Set file path
    file_path <- paste0(out_path, "/bh_vs_ad_dd_", dd_string, ".jpeg")
    
    # Set source/note
    source_string <- paste0("Source:  YCharts, DFA, ", first_yr ,"-", last_yr," (OfDollarsAndData.com)")
    note_string   <- str_wrap(paste0("Note:  The 'Avoid Drawdowns' strategy invests in the S&P 500 (total return) during years with a drawdown smaller than ", dd_pos  , "% ", 
                                "and in 5-Year U.S. Treasuries during all other years.  ",
                                "The strategy was invested in the S&P 500 in ", round(years_in, digits=0), " of ", round(years_total, digits = 0), " years or ", 100*round(pct_in, 2), "% of the time."), 
                              width = 85)
    
    plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
            geom_line() +
            scale_y_continuous(label = dollar, limits = c(0.8, ymax), breaks = c(1, 10, 100, 1000, 10000),  trans = log10_trans()) +
            scale_color_manual(values = c("blue", "black")) +
            of_dollars_and_data_theme +
            theme(legend.title = element_blank(),
                  legend.position = "bottom") +
            ggtitle(paste0("Buy & Hold vs. Avoiding Drawdowns\nGreater Than ", dd_pos, "%")) +
            labs(x="Date", y="Growth of $1 (Log Scale)",
                   caption = paste0("\n", source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
    # Do another plot with the geom_rect for when the strategy is in bonds
    file_path <- paste0(out_path, "/ad_in_bonds_", dd_string, ".jpeg")
    
    plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
      geom_rect(data=dates, aes(xmin = date, ymin = 0, 
                                xmax = end_date, ymax = ymax), fill = "green", alpha = 0.2) +
      geom_line() +
      scale_y_continuous(label = dollar, limits = c(0.8, ymax), breaks = c(1, 10, 100, 1000, 10000),  trans = log10_trans()) +
      scale_color_manual(values = c("blue", "black", "transparent"), guide = FALSE) +
      scale_fill_discrete(guide = FALSE) +
      of_dollars_and_data_theme +
      ggtitle(paste0("When 'Avoid Drawdowns' is in Bonds\nFor Drawdowns Greater Than ", dd_pos, "%")) +
      labs(x="Date", y="Growth of $1 (Log Scale)",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_gif(out_path, "bh_vs_ad_dd_*.jpeg", 60, 0, "_gif_bh_vs_ad.gif")


# ############################  End  ################################## #