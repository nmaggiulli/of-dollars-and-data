cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "xxxx_rebalance_during_dd"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in raw data
raw <- read.csv(paste0(importdir, "/xxxx_bond_stock_rets/bond_stock_monthly_rets.csv"), 
                skip = 7) %>%
        rename(date = X,
               ret_sp500 = USD,
               ret_bond = USD.1) %>%
        select(-`X.1`) %>%
        filter(!is.na(ret_sp500)) %>%
        mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
        filter(date >= "1926-11-01")
 
# Do Growth of dollar
for(i in 1:nrow(raw)){
  if(i == 1){
    raw[i, "index_sp500"] <- 1
    raw[i, "index_bond"] <- 1
  } else{
    raw[i, "index_sp500"] <- raw[(i-1), "index_sp500"] * (1 + raw[i, "ret_sp500"])
    raw[i, "index_bond"] <- raw[(i-1), "index_bond"] * (1 + raw[i, "ret_bond"])
  }
}

# Get all years
all_years <- sort(unique(year(raw$date)))
final_results <- data.frame()

build_data <- function(n_years, dd_limit, w_sp500){
  n_sims <- length(all_years) - n_years
  
  tmp_all_years <- all_years[1:n_sims]
  
  counter <- 1
  for(y in tmp_all_years){
    print(y)
    tmp <- raw %>%
            filter(date >= paste0(y, "-11-01"),
                   date <= paste0(y+n_years, "-11-01"))
    
    dd_sp500 <- tmp %>% 
                  select(date, index_sp500) %>%
                  drawdown_path() %>%
                  rename(dd_sp500 = pct)
    
    tmp <- tmp %>%
            left_join(dd_sp500)
    
    dd_rebal <- 0
    for(i in 1:nrow(tmp)){
      mt <- month(tmp[i, "date"])
      curr_dd <- tmp[i, "dd_sp500"]
      
      if(i == 1){
        tmp[i, "rebal_sp500"] <- w_sp500
        tmp[i, "rebal_bond"] <- (1-w_sp500)
        tmp[i, "dd_rebal_sp500"] <- w_sp500
        tmp[i, "dd_rebal_bond"] <- (1-w_sp500)
        tmp[i, "dd_rebalance"] <- 0
      } else{
        if(curr_dd < dd_limit & dd_rebal == 0){
          tmp[i, "dd_rebalance"] <- 1
          tmp[i, "dd_rebal_sp500"] <- tmp[(i-1), "port_dd"] * w_sp500 * (1 + tmp[i, "ret_sp500"])
          tmp[i, "dd_rebal_bond"] <- tmp[(i-1), "port_dd"] * (1-w_sp500) * (1 + tmp[i, "ret_bond"])
          
          # Set dd rebalance timer to 1 until new ATH
          dd_rebal <- 1
        } else{
          tmp[i, "dd_rebalance"] <- 0
          tmp[i, "dd_rebal_sp500"] <- tmp[(i-1), "dd_rebal_sp500"] * (1 + tmp[i, "ret_sp500"])
          tmp[i, "dd_rebal_bond"] <-  tmp[(i-1), "dd_rebal_bond"] * (1 + tmp[i, "ret_bond"])
        }
        
        if(mt == 6){
          tmp[i, "rebal_sp500"] <- tmp[(i-1), "port_rebal"] * w_sp500 * (1 + tmp[i, "ret_sp500"])
          tmp[i, "rebal_bond"] <- tmp[(i-1), "port_rebal"] * (1-w_sp500) * (1 + tmp[i, "ret_bond"])
        } else{
          tmp[i, "rebal_sp500"] <-  tmp[(i-1), "rebal_sp500"] * (1 + tmp[i, "ret_sp500"])
          tmp[i, "rebal_bond"] <-  tmp[(i-1), "rebal_bond"] * (1 + tmp[i, "ret_bond"])
        }
      }
      
      # Wait for new ATH before resetting DD
      if(curr_dd == 0){
        dd_rebal <- 0
      }
      
      tmp[i, "port_rebal"] <- tmp[i, "rebal_sp500"] + tmp[i, "rebal_bond"]
      tmp[i, "port_dd"] <- tmp[i, "dd_rebal_sp500"] + tmp[i, "dd_rebal_bond"]
      tmp[i, "dd_premium"] <- tmp[i, "port_dd"]/tmp[i, "port_rebal"] - 1
    }
    
    final_results[counter, "start_year"] <- y
    final_results[counter, "n_years"] <- n_years
    final_results[counter, "dd_limit"] <- dd_limit
    final_results[counter, "w_sp500"] <- w_sp500
    final_results[counter, "final_dd_premium"] <- tmp[nrow(tmp), "dd_premium"]
    final_results[counter, "n_dd_rebals"] <- sum(tmp$dd_rebalance, na.rm = TRUE)
    
    counter <- counter + 1
  }
  return(final_results)
}

n_yr <- 10
final_results_10yr <- build_data(n_yr, -0.2, 0.8) %>%
                  bind_rows(build_data(n_yr, -0.3, 0.8))

n_yr <- 20
final_results_20yr <- build_data(n_yr, -0.2, 0.8) %>%
  bind_rows(build_data(n_yr, -0.3, 0.8))

to_plot <- final_results_10yr %>%
  bind_rows(final_results_20yr) %>%
  mutate(dd_limit = case_when(
    dd_limit == -0.2 ~ "20%+",
    dd_limit == -0.3 ~ "30%+",
    TRUE ~ "Error"
  ))

dd_limit_results <- to_plot %>%
                      group_by(n_years, dd_limit) %>%
                      summarise(n_obs = n(),
                                mean_dd_premium = mean(final_dd_premium),
                                mean_rebals = mean(n_dd_rebals)) %>%
                      ungroup()


file_path <- paste0(out_path, "/rebal_by_years.jpeg")
source_string <- paste0("Source: Returns 2.0, (OfDollarsAndData.com)")

note_string <- str_wrap(paste0("Note: Assumes annual rebalance is done each June."),
  width = 85)

plot <- ggplot(to_plot, aes(x = final_dd_premium, fill = as.factor(dd_limit))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~n_years) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Drawdown Rebalance Premium by Time")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "Premium After 10 Years" , y = paste0("Frequency"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #