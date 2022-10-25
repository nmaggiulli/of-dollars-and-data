cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0318_port_6040"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0318_port_6040/GrowthOfWealth_20221022162658.csv"),
                skip = 7, 
                row.names = NULL,
                col.names = c("date", "index_sp500", "index_bond", "cpi"))  %>%
          filter(!is.na(index_sp500)) %>%
          mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
          mutate(date = as.Date(paste0(year(date), "-", month(date), "-01"), format = "%Y-%m-%d")) %>%
          filter(date >= "1970-01-01") %>%
          gather(-date, key=key, value=value) 

min_year <- min(year(raw$date))

irate <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
            select(date, long_irate)

first_values <- raw %>%
                    filter(date == min(raw$date)) %>%
                    select(-date) %>%
                    rename(first_value = value)
 
df <- raw %>%
          left_join(first_values) %>%
          mutate(value = value/first_value) %>%
          select(-first_value) %>%
          spread(key = key, value = value) %>%
          mutate(index_sp500_real = index_sp500/cpi,
                 index_bond_real = index_bond/cpi) %>%
          select(date, contains("_real"))

calc_dca <- function(start_dt, end_dt, weight_s, dca){

  monthly_payment <- 10000/12
  
  pay_s <- monthly_payment * weight_s
  pay_b <- monthly_payment - pay_s
  
  tmp <- df %>%
          filter(date >= start_dt, date <= end_dt)
  
  for(i in 1:nrow(tmp)){
    mt <- month(tmp[i, "date"])
    
    if(i == 1){
      tmp[i, "port_us_s"] <- pay_s
      tmp[i, "port_us_b"] <- pay_b
    } else{
      ret_s_us <- tmp[i, "index_sp500_real"]/tmp[(i-1), "index_sp500_real"] - 1
      ret_b <- tmp[i, "index_bond_real"]/tmp[(i-1), "index_bond_real"] - 1
      
      if(mt == 1){
        tmp[i, "port_us_s"] <- tmp[(i-1), "port_us"] * weight_s * (1 + ret_s_us) + pay_s
        tmp[i, "port_us_b"] <- tmp[(i-1), "port_us"] * (1-weight_s) * (1 + ret_b) + pay_b
      } else{
        tmp[i, "port_us_s"] <- tmp[(i-1), "port_us_s"] * (1 + ret_s_us) + pay_s
        tmp[i, "port_us_b"] <- tmp[(i-1), "port_us_b"] * (1 + ret_b) + pay_b
      }
    }
    tmp[i, "port_us"] <- tmp[i, "port_us_s"] + tmp[i, "port_us_b"]
  }
  
  return(tmp)
}

plot_global_dca <- function(n_years, s_weight){
  
  s_weight_label <- 100*s_weight
  b_weight_label <- (1-s_weight)*100
  
  n_months <- n_years*12
  total_months <- nrow(df)
  final_start_month <- total_months - n_months + 1
  all_dates <- seq.Date(min(df$date), df[final_start_month, "date"], "month")
  
  final_results <- data.frame()
  counter <- 1
  for(s in 1:length(all_dates)){
    start <- all_dates[s]
    end <- start + months(n_months - 1)
    
    tmp2 <- calc_dca(start,  end, s_weight)
    
    final_results[counter, "start_date"] <- start
    final_results[counter, "end_date"] <- end
    final_results[counter, "stock_weight"] <- s_weight
    final_results[counter, "port_us"] <- tmp2[nrow(tmp2), "port_us"]
    
    counter <- counter + 1
  }
  
  to_plot <- final_results %>%
                select(end_date, port_us) %>%
                gather(-end_date, key=key, value=value) %>%
                mutate(key = case_when(
                  key == "port_us" ~ paste0("U.S. ", s_weight_label, "/", b_weight_label),
                ))
  
  s_weight_fname <- ifelse(s_weight == 1, "100", paste0("0", s_weight_label))
  
  file_path <- paste0(out_path, "/us_", s_weight_fname, "_dca_", n_years, "_years_", min_year, ".jpg")
  source_string <- paste0("Source: Returns2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Includes dividends and adjusted for inflation. ", 
                                  "Bonds are represented by 5-Year U.S. Treasuries. ",
                                 "The portfolio is rebalanced each January."), 
                          width = 80)
  
  basis <- n_years*10000
  
  plot <- ggplot(data = to_plot, aes(x=end_date, y=value, col = key)) +
    geom_line() +
    geom_hline(yintercept = basis, linetype = "dashed") +
    scale_color_manual(values = c("black")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Real Value of ", format_as_dollar(basis), " DCA Investment\nOver ", n_years, " Years\nU.S. ", s_weight_label, "/", b_weight_label)) +
    labs(x = paste0(n_years, " Years Ending"), y = "Final Portfolio Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

years <- seq(10, 30, 10)

for(y in years){
  plot_global_dca(y, 0.6)
}

#Plot Irate vs bond ret
to_plot <- df %>%
            left_join(irate) %>%
            left_join(raw %>% filter(key == "cpi") %>% rename(cpi = value) %>% select(date, cpi)) %>%
            mutate(ret_bond_real = (lead(index_bond_real, 120)/index_bond_real)^(1/10) - 1,
                   ret_bond_1yr = (lead(index_bond_real, 12)/index_bond_real) - 1,
                   cpi_change = cpi/lag(cpi, 12) - 1,
                   fwd_cpi_change = lead(cpi, 12)/cpi - 1) %>%
            select(date, long_irate, cpi_change, fwd_cpi_change, ret_bond_real, ret_bond_1yr)

today_string <- date_to_string(Sys.Date())

file_path <- paste0(out_path, "/yield_vs_10yr_bond_", today_string, ".jpeg")
source_string <- paste0("Source:  Returns 2.0, Shiller Data (OfDollarsAndData.com)")
note_string <- paste0("Note: Bonds are represented by Five-Year U.S. Treasuries.")

# Plot the results
plot <- ggplot(to_plot, aes(x = long_irate, y = ret_bond_real)) +
  geom_point(col = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 0.042, linetype = "dashed") +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16), breaks = seq(0, 0.16, 0.04)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Starting 10-Year Yield vs.\nReal Bond Return Over Next Decade")) +
  labs(x = "Starting Bond Yield" , y = "Annualized Real Return Over Next Decade",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/yield_vs_10yr_bond_", today_string, "_no_vert_line.jpeg")
source_string <- paste0("Source:  Returns 2.0, Shiller Data (OfDollarsAndData.com)")
note_string <- paste0("Note: Bonds are represented by Five-Year U.S. Treasuries.")

# Plot the results
plot <- ggplot(to_plot, aes(x = long_irate, y = ret_bond_real)) +
  geom_point(col = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16), breaks = seq(0, 0.16, 0.04)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Starting 10-Year Yield vs.\nReal Bond Return Over Next Decade")) +
  labs(x = "Starting Bond Yield" , y = "Annualized Real Return Over Next Decade",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do CPI vs. Bond ret
file_path <- paste0(out_path, "/cpi_vs_10yr_bond_", today_string, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = cpi_change, y = ret_bond_real)) +
  geom_point(col = "black") +
  geom_vline(xintercept = 0.082, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("1-Year Change in CPI vs.\nReal Bond Return Over Next Decade")) +
  labs(x = "1-Year Change in CPI" , y = "Annualized Real Return Over Next Decade",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Future CPI vs. 1-Year Bond ret
file_path <- paste0(out_path, "/fwd_cpi_vs_1yr_bond_", today_string, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")

# Plot the results
plot <- ggplot(to_plot, aes(x = fwd_cpi_change, y = ret_bond_1yr)) +
  geom_point(col = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("1-Year Future Change in CPI vs.\nReal Bond Return Over Next Year")) +
  labs(x = "Future Change in CPI" , y = "Real Return Over Next Year",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #