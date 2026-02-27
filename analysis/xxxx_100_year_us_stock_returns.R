cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "xxxx_100_year_us_stock_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

# ============================================================
# S&P 500 Return Distribution Analysis (1926-2025)
# ============================================================
# Assumes: df with columns "date" and "price_plus_div"
# where price_plus_div is inflation-adjusted total return index
# Data already filtered to 1926-01-01 through 2025-12-01
# ============================================================

df <- sp500_ret_pe %>%
          filter(date >= "1926-01-01", date<= "2025-12-31") %>%
          select(date, price_plus_div) %>%
          arrange(date)

# Define holding periods in months
periods <- c(1, 3, 12, 60, 120, 240, 360)
period_labels <- c("1 Month", "3 Months", "1 Year", "5 Years", "10 Years", "20 Years", "30 Years")

# --- Step 1: Calculate rolling returns for each period ---
calc_returns <- function(df, n_months) {
  df %>%
    arrange(date) %>%
    mutate(
      future_price = lead(price_plus_div, n = n_months),
      total_return = future_price / price_plus_div - 1,
      annualized_return = (future_price / price_plus_div)^(12 / n_months) - 1
    ) %>%
    filter(!is.na(total_return))
}

# Store all returns
all_returns <- map2(periods, period_labels, function(p, lab) {
  calc_returns(df, p) %>%
    mutate(period = lab, period_months = p)
}) %>%
  bind_rows()

# --- Step 2: Define reasonable return buckets based on the data ---
# For periods < 12 months, use total return
# For periods >= 12 months, use annualized return

create_buckets <- function(returns_vec, n_months) {
  if (n_months < 12) {
    # Even 5% bins for monthly/quarterly total returns
    breaks <- c(-Inf, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, Inf)
    labels <- c("< -15%", "-15% to -10%", "-10% to -5%", "-5% to 0%",
                "0% to 5%", "5% to 10%", "10% to 15%", "> 15%")
  } else if (n_months == 12) {
    # Even 10% bins for 1-year returns
    breaks <- c(-Inf, -0.30, -0.20, -0.10, 0, 0.10, 0.20, 0.30, Inf)
    labels <- c("< -30%", "-30% to -20%", "-20% to -10%", "-10% to 0%",
                "0% to 10%", "10% to 20%", "20% to 30%", "> 30%")
  } else if (n_months <= 120) {
    # Even 2% bins for 5-10 year annualized
    breaks <- c(-Inf, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, Inf)
    labels <- c("< -4%", "-4% to -2%", "-2% to 0%", "0% to 2%", "2% to 4%",
                "4% to 6%", "6% to 8%", "8% to 10%", "10% to 12%", "> 12%")
  } else {
    # Even 2% bins for 20-30 year annualized
    breaks <- c(-Inf, 0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, Inf)
    labels <- c("< 0%", "0% to 2%", "2% to 4%", "4% to 6%",
                "6% to 8%", "8% to 10%", "10% to 12%", "> 12%")
  }
  
  cut(returns_vec, breaks = breaks, labels = labels, right = TRUE)
}

# --- Step 3: Build distribution tables ---
results_list <- map2(periods, period_labels, function(p, lab) {
  
  subset <- all_returns %>% filter(period == lab)
  
  # Choose the right return metric
  if (p < 12) {
    ret_col <- subset$total_return
    ret_type <- "Total Return"
  } else {
    ret_col <- subset$annualized_return
    ret_type <- "Annualized Return"
  }
  
  buckets <- create_buckets(ret_col, p)
  
  tibble(
    return_range = buckets,
    return_value = ret_col
  ) %>%
    group_by(return_range) %>%
    summarise(
      count = n(),
      avg_return = mean(return_value),
      .groups = "drop"
    ) %>%
    mutate(
      probability = count / sum(count),
      contribution = probability * avg_return,
      period = lab,
      return_type = ret_type
    )
})

results <- bind_rows(results_list)

# --- Step 4: Summary table with expected returns ---
summary_table <- results %>%
  group_by(period, return_type) %>%
  summarise(
    expected_return = sum(contribution),
    n_observations = sum(count),
    .groups = "drop"
  )

# Fill in additional stats from the raw data
additional_stats <- map2_dfr(periods, period_labels, function(p, lab) {
  subset <- all_returns %>% filter(period == lab)
  ret_col <- if (p < 12) subset$total_return else subset$annualized_return
  
  tibble(
    period = lab,
    median_return = median(ret_col),
    min_return = min(ret_col),
    max_return = max(ret_col),
    pct_positive = mean(ret_col > 0)
  )
})

summary_table <- summary_table %>%
  left_join(additional_stats, by = "period")

# Order periods properly
summary_table$period <- factor(summary_table$period, levels = period_labels)

# --- Step 5: Print results ---
cat(strrep("=", 70), "\n")
cat("S&P 500 RETURN ANALYSIS (1926-2025, Real Total Returns)\n")
cat(strrep("=", 70), "\n\n")

# Print summary
cat("SUMMARY TABLE\n")
cat(strrep("-", 70), "\n")
summary_table %>%
  arrange(period) %>%
  mutate(across(c(expected_return, median_return, min_return, max_return),
                ~ sprintf("%.2f%%", . * 100)),
         pct_positive = sprintf("%.1f%%", pct_positive * 100)) %>%
  select(Period = period, Type = return_type, `Expected Return` = expected_return,
         Median = median_return, Min = min_return, Max = max_return,
         `% Positive` = pct_positive, N = n_observations) %>%
  print(n = Inf)

# Print detailed distribution for each period
cat("\n\n")
for (lab in period_labels) {
  cat(strrep("=", 70), "\n")
  cat(lab, "Return Distribution\n")
  cat(strrep("-", 70), "\n")
  
  period_results <- results %>%
    filter(period == lab)
  
  period_results %>%
    mutate(
      probability = sprintf("%.1f%%", probability * 100),
      avg_return = sprintf("%.2f%%", avg_return * 100),
      contribution = sprintf("%.4f%%", contribution * 100)
    ) %>%
    select(`Return Range` = return_range, Count = count,
           Probability = probability, `Avg Return` = avg_return,
           `Contribution to E[R]` = contribution) %>%
    print(n = Inf)
  
  expected_ret <- sum(period_results$contribution) * 100
  cat(sprintf("\n  Expected Return: %.2f%%\n\n", expected_ret))
}

all_periods <- unique(results$period)

bucket_labels <- list(
  "1 Month" = c("< -15%", "-15% to -10%", "-10% to -5%", "-5% to 0%",
                "0% to 5%", "5% to 10%", "10% to 15%", "> 15%"),
  "3 Months" = c("< -15%", "-15% to -10%", "-10% to -5%", "-5% to 0%",
                 "0% to 5%", "5% to 10%", "10% to 15%", "> 15%"),
  "1 Year" = c("< -30%", "-30% to -20%", "-20% to -10%", "-10% to 0%",
               "0% to 10%", "10% to 20%", "20% to 30%", "> 30%"),
  "5 Years" = c("< -4%", "-4% to -2%", "-2% to 0%", "0% to 2%", "2% to 4%",
                "4% to 6%", "6% to 8%", "8% to 10%", "10% to 12%", "> 12%"),
  "10 Years" = c("< -4%", "-4% to -2%", "-2% to 0%", "0% to 2%", "2% to 4%",
                 "4% to 6%", "6% to 8%", "8% to 10%", "10% to 12%", "> 12%"),
  "20 Years" = c("< 0%", "0% to 2%", "2% to 4%", "4% to 6%",
                 "6% to 8%", "8% to 10%", "10% to 12%", "> 12%"),
  "30 Years" = c("< 0%", "0% to 2%", "2% to 4%", "4% to 6%",
                 "6% to 8%", "8% to 10%", "10% to 12%", "> 12%")
)

for(p in all_periods){
  
  period_results <- all_returns %>%
    filter(period == p)
  
  if(grepl("Month", p)){
    x_lab <- "Total Real Return"
    annualized_string <- ""
    exp_ret<- mean(period_results$total_return)
    label_accuracy <- 0.1
  } else{
    x_lab <- "Total Real Return (Annualized)"
    annualized_string <- " (annualized)"
    exp_ret<- mean(period_results$annualized_return)
    label_accuracy <- 1
  }
  
  to_plot <- results %>%
              filter(period == p) %>%
              mutate(return_range = factor(return_range, levels = bucket_labels[[p]]))
  
  file_path <- paste0(out_path, "/us_stock_", p, "_return_dist_1926_2025.jpeg")
  source_string <- paste0("Source: Shiller data, 1926-2025 (OfDollarsAndData.com)")
  note_string   <- str_wrap(paste0("Note: Performance includes reinvested dividends and is adjusted for inflation. The expected total real return over ", p, " is ", round(100*exp_ret, 2), "%", annualized_string, "."),
                            width = 85)
  
  plot <- ggplot(to_plot, aes(x = return_range, y = probability)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    geom_text(aes(label = percent(probability, accuracy = label_accuracy)),
              vjust = -0.5, color = chart_standard_color, size = 3) +
    of_dollars_and_data_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("U.S. Stock Market Return Distribution\n", p)) +
    labs(x = x_lab, y = "Probability",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# ============================================================
# Worst Entry Points Analysis
# ============================================================

worst_starts <- c("1929-09-01", "1973-01-01", "2000-03-01", "2007-10-01")
worst_labels <- c("September 1929", "January 1973", "March 2000", "October 2007")

# For each worst entry point, track growth of $1 over time
worst_results <- map2_dfr(worst_starts, worst_labels, function(start, lab) {
  
  start_date <- as.Date(start)
  
  subset <- df %>%
    filter(date >= start_date) %>%
    mutate(
      months_elapsed = as.numeric(difftime(date, start_date, units = "days")) / 30.44,
      years_elapsed = months_elapsed / 12,
      growth = price_plus_div / first(price_plus_div)
    ) %>%
    mutate(entry_point = lab)
  
  return(subset)
})

worst_results$entry_point <- factor(worst_results$entry_point, levels = worst_labels)

# --- Chart 1: Growth of $1 for each entry point (first 20 years) ---
plot_data <- worst_results %>%
  filter(years_elapsed <= 20)

plot_growth <- ggplot(plot_data, aes(x = years_elapsed, y = growth, color = entry_point)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  scale_y_continuous(label = dollar_format()) +
  scale_color_manual(values = c("September 1929" = "#1a252f",
                                "January 1973" = "#4292c6",
                                "March 2000" = "#e08b36",
                                "October 2007" = "#c94040")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Growth of $1 Invested at the Worst Times\nU.S. Stocks, Real Total Return") +
  labs(x = "Years After Investment",
       y = "Growth of $1 (Real)",
       caption = paste0("Source: Shiller data, 1926-2025 (OfDollarsAndData.com)\n",
                        str_wrap("Note: Performance includes reinvested dividends and is adjusted for inflation.", width = 85)))

ggsave(paste0(out_path, "/worst_entry_points_growth_20yr_line.jpeg"), plot_growth,
       width = 15, height = 12, units = "cm")

# --- Chart 2: Annualized returns at key milestones for each entry point ---
milestones <- c(1, 5, 10, 20)

milestone_returns <- map2_dfr(worst_starts, worst_labels, function(start, lab) {
  
  start_date <- as.Date(start)
  start_price <- df %>% filter(date == start_date) %>% pull(price_plus_div)
  
  map_dfr(milestones, function(yrs) {
    
    end_date <- start_date %m+% months(yrs * 12)
    
    end_row <- df %>%
      filter(date >= end_date) %>%
      slice(1)
    
    if (nrow(end_row) == 0) return(NULL)
    
    end_price <- end_row$price_plus_div
    total_return <- end_price / start_price - 1
    ann_return <- (end_price / start_price)^(1 / yrs) - 1
    
    tibble(
      entry_point = lab,
      horizon = paste0(yrs, " Year", ifelse(yrs > 1, "s", "")),
      horizon_years = yrs,
      total_return = total_return,
      annualized_return = ann_return
    )
  })
})

milestone_returns$horizon <- factor(milestone_returns$horizon,
                                    levels = c("1 Year", "5 Years", "10 Years", "20 Years"))
milestone_returns$entry_point <- factor(milestone_returns$entry_point, levels = worst_labels)

# Grouped bar chart
plot_milestones <- ggplot(milestone_returns,
                          aes(x = horizon, y = annualized_return, fill = entry_point)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", annualized_return * 100),
                y = annualized_return + ifelse(annualized_return >= 0, 0.005, -0.005)),
            position = position_dodge(width = 0.8),
            vjust = ifelse(milestone_returns$annualized_return >= 0, -0.3, 1.3),
            size = 2.2, color = "black") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("September 1929" = "#1a252f",
                                "January 1973" = "#4292c6",
                                "March 2000" = "#e08b36",
                                "October 2007" = "#c94040")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Annualized Real Returns After Investing\nat the Worst Possible Times") +
  labs(x = "Holding Period",
       y = "Annualized Real Return",
       caption = paste0("Source: Shiller data, 1926-2025 (OfDollarsAndData.com)\n",
                        str_wrap("Note: Performance includes reinvested dividends and is adjusted for inflation.", width = 85)))

ggsave(paste0(out_path, "/worst_entry_points_milestones.jpeg"), plot_milestones,
       width = 15, height = 12, units = "cm")

# --- Print summary table ---
cat(strrep("=", 70), "\n")
cat("WORST ENTRY POINTS: ANNUALIZED REAL RETURNS\n")
cat(strrep("=", 70), "\n\n")

milestone_returns %>%
  mutate(annualized_return = sprintf("%.2f%%", annualized_return * 100)) %>%
  pivot_wider(names_from = horizon, values_from = annualized_return) %>%
  select(-horizon_years, -total_return) %>%
  print()

# --- Time to recovery for each entry point ---
recovery <- map2_dfr(worst_starts, worst_labels, function(start, lab) {
  
  start_date <- as.Date(start)
  start_price <- df %>% filter(date == start_date) %>% pull(price_plus_div)
  
  recovery_row <- df %>%
    filter(date > start_date, price_plus_div >= start_price) %>%
    slice(1)
  
  if (nrow(recovery_row) == 0) {
    recovery_months <- NA
  } else {
    recovery_months <- interval(start_date, recovery_row$date) %/% months(1)
  }
  
  tibble(
    entry_point = lab,
    recovery_months = recovery_months,
    recovery_years = round(recovery_months / 12, 1)
  )
})

cat("\nTIME TO RECOVERY (Real Total Return)\n")
cat(strrep("-", 70), "\n")
print(recovery)

# ############################  End  ################################## #