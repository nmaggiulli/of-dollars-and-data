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

folder_name <- "xxxx_buying_dips_revisited"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
  select(date, price_plus_div)

dd <- drawdown_path(shiller)

dd_num <- 1
for(i in 1:nrow(dd)){
  dd_curr <- dd[i, "pct"]
  
  dd[i, "dd_num"] <- dd_num
  
  if(dd_curr == 0){
    dd_num <- dd_num + 1
  }
}

dd_final <- dd %>%
  left_join(shiller)

dd_tops <- dd_final %>%
            filter(pct == 0) %>%
            group_by(dd_num) %>%
            summarise(recovery_price = max(price_plus_div),
                      recovery_date = date) %>%
            ungroup()

dd_lengths <- dd %>%
                group_by(dd_num) %>%
                summarise(n_months = n(),
                          min_dd = min(pct)) %>%
                ungroup() %>%
                filter(min_dd < -0.2) %>%
                left_join(dd_tops) %>%
                select(-n_months)

plot_dd_pct <- function(dd_pct){
  
  dd_w_recovery <- dd_final %>%
    inner_join(dd_lengths) %>%
    mutate(n_years = (interval(date, recovery_date) %/% months(1))/12,
           annualized_recovery_ret = (recovery_price/price_plus_div)^(1/n_years) - 1,
           recovery_bucket = case_when(
             annualized_recovery_ret < 0.05 ~ "0%-5%",
             annualized_recovery_ret < 0.1 ~ "5%-10%",
             annualized_recovery_ret < 0.15 ~ "10%-15%",
             annualized_recovery_ret < 0.2 ~ "15%-20%",
             annualized_recovery_ret < 0.25 ~ "20%-25%",
             annualized_recovery_ret < 0.3 ~ "25%-30%",
             TRUE ~ ">30%"
           )) %>%
    filter(n_years != 0, date >= "1900-01-01", pct < dd_pct)
  
  tmp <- dd_w_recovery %>%
              group_by(recovery_bucket) %>%
              summarise(pct = n()/nrow(dd_w_recovery)) %>%
              ungroup()
  
  recovery_buckets <- c("0%-5%", "5%-10%","10%-15%", "15%-20%", "20%-25%", "25%-30%", ">30%")
  
  to_plot <- data.frame(recovery_bucket = recovery_buckets) %>%
            full_join(tmp) %>%
            mutate(pct = ifelse(is.na(pct), 0, pct))
  
  
  to_plot$recovery_bucket <- factor(to_plot$recovery_bucket, levels = recovery_buckets)
  
  dd_string <- -100*dd_pct
  
  file_path <- paste0(out_path, "/dd_", dd_string, "_pct_rets.jpeg")
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  
  # Plot the results
  plot <- ggplot(to_plot, aes(x = recovery_bucket, y = pct)) +
    geom_bar(stat="identity", fill = chart_standard_color) +
    scale_y_continuous(label = percent_format(accuracy = 1),
                       limits = c(0, 0.5)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Annualized Returns After Buying\nDuring ", dd_string, "%+ Drawdown")) +
    labs(x = "Annualized Return" , y = "Frequency",
         caption = paste0("\n", source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

dd_pcts <- seq(-0.2, -0.50, -0.05)

for(d in dd_pcts){
  plot_dd_pct(d)
}

create_gif(path = paste0(out_path),
           file_stub = "dd_*.jpeg" , 
             speed_milliseconds = 100,
           out_name = "_dd_pcts.gif")




# ############################  End  ################################## #