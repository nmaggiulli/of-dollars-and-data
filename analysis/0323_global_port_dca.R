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

folder_name <- "0323_global_port_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_1970 <- read.csv(paste0(importdir, "/0323_global_port/GrowthOfWealth_20221021164443.csv"),
                skip = 7, 
                row.names = NULL,
                col.names = c("date", "index_sp500",	"index_world", "cpi", "index_bond"))  %>%
          filter(!is.na(index_sp500)) %>%
          mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
          mutate(date = as.Date(paste0(year(date), "-", month(date), "-01"), format = "%Y-%m-%d")) %>%
          gather(-date, key=key, value=value) 

first_values <- raw_1970 %>%
                    filter(date == min(raw_1970$date)) %>%
                    select(-date) %>%
                    rename(first_value = value)
 
df <- raw_1970 %>%
          left_join(first_values) %>%
          mutate(value = value/first_value) %>%
          select(-first_value) %>%
          spread(key = key, value = value) %>%
          mutate(index_sp500_real = index_sp500/cpi,
                 index_world_real = index_world/cpi,
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
      tmp[i, "port_world_s"] <- pay_s
      tmp[i, "port_world_b"] <- pay_b
      
      tmp[i, "port_us_s"] <- pay_s
      tmp[i, "port_us_b"] <- pay_b
    } else{
      ret_s_us <- tmp[i, "index_sp500_real"]/tmp[(i-1), "index_sp500_real"] - 1
      ret_s_world <- tmp[i, "index_world_real"]/tmp[(i-1), "index_world_real"] - 1
      ret_b <- tmp[i, "index_bond_real"]/tmp[(i-1), "index_bond_real"] - 1
      
      if(mt == 1){
        tmp[i, "port_world_s"] <- tmp[(i-1), "port_world"] * weight_s *  (1 + ret_s_world) + pay_s
        tmp[i, "port_world_b"] <- tmp[(i-1), "port_world"] * (1-weight_s) * (1 + ret_b) + pay_b
        
        tmp[i, "port_us_s"] <- tmp[(i-1), "port_us"] * weight_s * (1 + ret_s_us) + pay_s
        tmp[i, "port_us_b"] <- tmp[(i-1), "port_us"] * (1-weight_s) * (1 + ret_b) + pay_b
      } else{
        tmp[i, "port_world_s"] <- tmp[(i-1), "port_world_s"] * (1 + ret_s_world) + pay_s
        tmp[i, "port_world_b"] <- tmp[(i-1), "port_world_b"] * (1 + ret_b) + pay_b
        
        tmp[i, "port_us_s"] <- tmp[(i-1), "port_us_s"] * (1 + ret_s_us) + pay_s
        tmp[i, "port_us_b"] <- tmp[(i-1), "port_us_b"] * (1 + ret_b) + pay_b
      }
    }
    
    tmp[i, "port_world"] <- tmp[i, "port_world_s"] + tmp[i, "port_world_b"]
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
    final_results[counter, "port_world"] <- tmp2[nrow(tmp2), "port_world"]
    final_results[counter, "port_us"] <- tmp2[nrow(tmp2), "port_us"]
    
    counter <- counter + 1
  }
  
  final_results <- final_results %>%
                    mutate(us_outperform = ifelse(port_us > port_world, 1, 0))
  
  to_plot <- final_results %>%
                select(end_date, port_world, port_us) %>%
                gather(-end_date, key=key, value=value) %>%
                mutate(key = case_when(
                  key == "port_us" ~ paste0("U.S. ", s_weight_label, "/", b_weight_label),
                  key == "port_world" ~ paste0("Global ", s_weight_label, "/", b_weight_label),
                  TRUE ~ "Error"),
                  total_growth = value/(n_years*10000)
                )
  
  s_weight_fname <- ifelse(s_weight == 1, "100", paste0("0", s_weight_label))
  
  file_path <- paste0(out_path, "/global_v_us_", s_weight_fname, "_dca_", n_years, "_years.jpg")
  source_string <- paste0("Source: Returns2.0 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Includes dividends and adjusted for inflation. ", 
                                  "Bonds are represented by 5-Year U.S. Treasuries. ",
                                 "The portfolio is rebalanced every January."), 
                          width = 80)
  
  basis <- n_years*10000
  
  plot <- ggplot(data = to_plot, aes(x=end_date, y=value, col = key)) +
    geom_line() +
    geom_hline(yintercept = basis, linetype = "dashed") +
    scale_color_manual(values = c("black", "green")) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(paste0("Real Value of ", format_as_dollar(basis), " DCA Investment\nOver ", n_years, " Years\nU.S. vs. Global ", s_weight_label, "/", b_weight_label)) +
    labs(x = paste0(n_years, " Years Ending"), y = "Final Portfolio Value",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
  if(n_years == 10){
    new_f <- 1
  } else{
    new_f <- 0
  }
  
  assign(paste0("final_results_", n_years), to_plot %>% mutate(n_years = n_years), envir = .GlobalEnv)
  
  to_plot_summary <- to_plot %>%
                        group_by(key) %>%
                        summarise(min_growth = min(total_growth),
                                  median_growth = quantile(total_growth, probs = 0.5),
                                  max_growth = max(total_growth)
                        ) %>%
                        ungroup() %>%
                        mutate(n_years = n_years)
  

  
  export_to_excel(df = to_plot_summary,
                  outfile = paste0(out_path, "/dca_global_results.xlsx"),
                  sheetname = paste0("results_", n_years),
                  new_file = new_f,
                  fancy_formatting = 0)
}

years <- seq(10, 40, 10)

for(y in years){
  plot_global_dca(y, 0.8)
}

all_results <- final_results_10 %>%
                  bind_rows(final_results_20, final_results_30, final_results_40) %>%
                  filter(grepl("Global", key))

file_path <- paste0(out_path, "/all_global_results_8020_dca.jpg")
source_string <- paste0("Source: Returns2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Includes dividends and adjusted for inflation. ", 
                               "Bonds are represented by 5-Year U.S. Treasuries. ",
                               "The portfolio is rebalanced every January."), 
                        width = 80)

y_max <- round_to_nearest(max(all_results$total_growth), "up", 1)

plot <- ggplot(data = all_results, aes(x=as.factor(n_years), y=total_growth)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(label = comma, breaks = seq(1, y_max, 1), limits = c(0, y_max)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Total Growth of DCA into Global 80/20 Portfolio\nBy Time Horizon")) +
  labs(x = paste0("Years Invested"), y = "Total Growth (Value/Basis)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #