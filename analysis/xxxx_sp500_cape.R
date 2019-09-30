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

folder_name <- "xxxx_sp500_cape"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

plot_cape_time <- function(start_date, end_date, n_month_ret, file_out){
  
  n_year_ret <- n_month_ret/12
  
  #Bring in all data
  ret_yr <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    rename(index = price_plus_div) %>%
                    mutate(fwd_ret = (lead(index, n_month_ret)/index)^(12/n_month_ret) - 1) %>%
                    select(date, fwd_ret, cape) %>%
                    filter(!is.na(fwd_ret), !is.na(cape), 
                           date >= start_date,
                           date < end_date) 
  
  to_plot <- ret_yr
  
  start_year <- year(start_date)
  end_year <- year(max(to_plot$date))
  
  # Do stock beats by year
  file_path <- paste0(out_path, "/", file_out)
  
  source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
  note_string <- paste0("Note:  Real return includes reinvested dividends.")
  
  plot <- ggplot(data = to_plot, aes(x=cape, y = fwd_ret)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(label = percent, limits = c(-0.15, 0.3)) +
    scale_x_continuous(limits = c(0, 45)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Starting CAPE vs. ", n_year_ret, "-Year Forward Return\n", start_year, "-", end_year)) +
    labs(x = paste0("Starting CAPE"), y = paste0(n_year_ret, "-Year Annualized Forward Return"),
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
}

date_seq <- seq.Date(as.Date("1940-01-01"), as.Date("2008-01-01"), "year")

for(i in 1:length(date_seq)){
  n_month_fwd_ret <- 60
  
  start_dt <- date_seq[i]
  start_date_string <- date_to_string(start_dt)
  
  end_dt <- start_dt + years(20)
  
  file_name_out <- paste0("cape_forward_ret_", n_month_fwd_ret, "m_", start_date_string, ".jpeg")
  
  plot_cape_time(start_dt, end_dt, n_month_fwd_ret, file_name_out)
}

create_gif(out_path,
           paste0("cape_forward_ret_", n_month_fwd_ret, "m_*.jpeg"),
           40,
           0,
           paste0("_gif_cape_fwd_ret.gif"))

plot_cape_time(as.Date("1940-01-01"), as.Date("2014-01-01"), n_month_fwd_ret, "cape_all_years.jpeg")


# ############################  End  ################################## #