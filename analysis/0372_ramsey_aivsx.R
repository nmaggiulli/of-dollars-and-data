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
library(tidyverse)

folder_name <- "0372_ramsey_aivsx"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_years <- 30
raw <- read_excel(paste0(importdir, "/0372_aivsx_ret/aivsx_rets.xlsx"))

for(i in 1:nrow(raw)){
  if(i == 1){
    raw[i, "real_index_aivsx"] <- 1
  } else{
    raw[i, "real_index_aivsx"] <-  raw[(i-1), "real_index_aivsx"]*(1+raw[(i-1), "ret"])/(1+raw[(i-1), "cpi"])
  }
}

get_survival_rate <- function(swr){

all_years <- unique(raw$year)

year_seq <- seq(min(all_years), max(all_years) - n_years + 1)

final_results <- data.frame()
counter <- 1
for(y in year_seq){
  end_y <- y + n_years - 1
  
  final_results[counter, "start_year"] <- y
  final_results[counter, "end_year"] <- end_y
  
  tmp <- raw %>%
            filter(year >= y, year <= end_y)
  
  for(i in 1:nrow(tmp)){
    ret <- tmp[i, "ret"]
    cpi <- tmp[i, "cpi"]
    
    if(i == 1){
      tmp[i, "withdrawal_amount"] <- 10^6*swr
      tmp[i, "value"] <- (10^6 - tmp[i, "withdrawal_amount"]) * (1 + ret)
    } else{
      tmp[i, "withdrawal_amount"] <- tmp[(i-1), "withdrawal_amount"] * (1 + cpi)
      tmp[i, "value"] <- (tmp[(i-1), "value"] - tmp[i, "withdrawal_amount"]) * (1 + ret)
      
      if(tmp[i, "value"] < 0 & tmp[(i-1), "value"] > 0){
        tmp[i, "value"] <- 0
        final_results[counter, "years_survived"] <- i
      } else if(tmp[i, "value"] < 0){
        tmp[i, "value"] <- 0
      } else if(i == n_years & tmp[i, "value"] > 0){
        final_results[counter, "years_survived"] <- n_years
      }
    }
  }
  
  final_results[counter, "final_value"] <- tmp[nrow(tmp), "value"]
  counter <- counter + 1
}

final_results <- final_results %>%
                    mutate(gt_zero = ifelse(final_value > 0, 1, 0))
  return(final_results)
}

sim_results <- data.frame()
counter <- 1

swrs <- seq(0.03, 0.1, 0.01)

for(s in swrs){
  print(s)
  fr <- get_survival_rate(s)
  sim_results[counter, "swr"] <- paste0(100*s, "%")
  sim_results[counter, "survival_rate"] <- mean(fr$gt_zero)
  
  counter <- counter + 1
}

# Plot swr results
to_plot <- sim_results

to_plot$swr <- factor(to_plot$swr, levels = paste0(100*swrs, "%"))

text_labels <- to_plot %>%
  mutate(label = paste0(100*round(survival_rate, 2), "%"))

file_path <- paste0(out_path, "/sim_results_by_swr_aivsx.jpeg")
source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Performance data starts in 1935 and ends in 2022. Assumes annual spending is adjusted by inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = swr, y = survival_rate)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data=text_labels, aes(x=swr, y=survival_rate, label = label),
            col = chart_standard_color,
            vjust = -0.2,
            size = 3) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Survival Rate of 100% AIVSX Portfolio\nOver ", n_years, "-Year Periods\nBy Withdrawal Rate")) +
  labs(x = "Withdrawal Rate" , y = "Survival Percentage",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

plot_swr_survival <- function(swr){

  # Plot SWR specific results
  to_plot <- get_survival_rate(swr)

  swr_string <- 100*swr
  
  assign(paste0("swr_", swr_string), to_plot, envir = .GlobalEnv)
  
  if(swr == 0.08){
    a_an <- "an"
  } else{
    a_an <- "a"
  }
  
  file_path <- paste0(out_path, "/swr_", swr_string, "_aivsx.jpeg")
  source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
  note_string <- str_wrap("Note: Assumes annual spending is adjusted by inflation.",
                          width = 80)
  
  plot <- ggplot(data = to_plot, aes(x = start_year, y = years_survived)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    scale_y_continuous(limits = c(0, n_years), breaks = seq(0, n_years, 5)) +
    scale_x_continuous(breaks = seq(min(to_plot$start_year), 2020, 5)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Years Survived for 100% AIVSX Portfolio\nBy Start Year\nUsing ", a_an," ", 100*swr, "% Withdrawal Rate")) +
    labs(x = "Start Year" , y = "Years Survived",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_swr_survival(0.08)
plot_swr_survival(0.05)

#Plot AIVSX CAGR
cagrs <- raw %>%
  mutate(fwd_real_cagr_10yr = (lead(real_index_aivsx, 10)/real_index_aivsx)^(1/10) - 1,
    fwd_real_cagr_30yr = (lead(real_index_aivsx, 30)/real_index_aivsx)^(1/30) - 1)

to_plot <- cagrs %>%
            drop_na() %>%
            left_join(swr_8 %>% rename(year=start_year) %>% select(year, gt_zero))

file_path <- paste0(out_path, "/aivsx_fwd_30_year_cagr.jpeg")
source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = year, y = fwd_real_cagr_30yr)) +
  geom_line() +
  geom_hline(yintercept = 0.08, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), 2020, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("AIVSX 30-Year Inflation-Adjusted CAGR\nBy Year")) +
  labs(x = "Start Year" , y = "30-Year Real CAGR",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do with points
file_path <- paste0(out_path, "/aivsx_fwd_30_year_cagr_w_points.jpeg")
source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = year, y = fwd_real_cagr_30yr)) +
  geom_line() +
  geom_hline(yintercept = 0.08, linetype = "dashed") +
  geom_point(data = to_plot, aes(x = year, y = fwd_real_cagr_30yr, col = as.factor(gt_zero))) +
  scale_color_manual(values = c("red", "green"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), 2020, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("AIVSX 30-Year Inflation-Adjusted CAGR\nBy Year\nWith Survival Information")) +
  labs(x = "Start Year" , y = "30-Year Real CAGR",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Do 10-year CAGRs
to_plot <- cagrs %>%
  select(-fwd_real_cagr_30yr) %>%
  drop_na() %>%
  left_join(swr_8 %>% rename(year=start_year) %>% select(year, gt_zero))

file_path <- paste0(out_path, "/aivsx_fwd_10_year_cagr.jpeg")
source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = year, y = fwd_real_cagr_10yr)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-0.02, 0.16, 0.02), limits = c(-0.02, 0.16)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), 2020, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("AIVSX 10-Year Inflation-Adjusted CAGR\nBy Year")) +
  labs(x = "Start Year" , y = "10-Year Real CAGR",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/aivsx_fwd_10_year_cagr_w_points.jpeg")
source_string <- paste0("Source:  YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = year, y = fwd_real_cagr_10yr)) +
  geom_line() +
  geom_point(data = to_plot, aes(x = year, y = fwd_real_cagr_10yr, col = as.factor(gt_zero))) +
  scale_color_manual(values = c("red", "green"), guide = "none") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(-0.02, 0.16, 0.02), limits = c(-0.02, 0.16)) +
  scale_x_continuous(breaks = seq(min(to_plot$year), 2020, 10)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("AIVSX 10-Year Inflation-Adjusted CAGR\nBy Year\nWith Survival Information")) +
  labs(x = "Start Year" , y = "10-Year Real CAGR",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #