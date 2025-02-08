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
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0437_1m_purchasing_power"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Grab CPI data
cpi <- readRDS(paste0(localdir, "0021_FRED_cpi.Rds")) %>%
  filter(year>= 1998, year <= 2022) %>%
  select(year, index_cpi)

cpi_latest <- cpi %>%
  mutate(cpi_latest = index_cpi/cpi[nrow(cpi), "index_cpi"]) %>%
  select(year, cpi_latest)

# Bring in SCF data
scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
  filter(year >= 1998) %>%
  left_join(cpi_latest) %>%
  mutate(networth_nominal = networth * cpi_latest) %>%
  select(year, wgt, networth_nominal)

# Calculate net worth percentiles
find_percentile <- function(yr, amount, var, varname){
  
  if(amount < 10^6){
    p_change <- 0.001
    p_guess <- 0.5
    log_reduce <- 1
  } else if(amount < 10^7){
    p_change <- 0.001
    p_guess <- 0.8
    log_reduce <- 2
  } else if (amount < 10^8){
    p_change <- 0.0001
    p_guess <- 0.97
    log_reduce <- 2
  } else{
    p_change <- 0.0001
    p_guess <- 0.99
    log_reduce <- 2
  }
  
  solved <- 0
  while(solved == 0){
    guess <- scf_stack %>%
      filter(year == yr) %>%
      rename(summary_col = !!sym(var)) %>%
      summarise(nw_percentile = wtd.quantile(summary_col, weights = wgt, probs = p_guess)) %>%
      pull(nw_percentile)
    
    diff_allowed <- 10^(floor(log10(amount) - log_reduce))
    
    if(guess - amount > diff_allowed){
      p_guess <- p_guess - p_change
    } else if (amount - guess > diff_allowed){
      p_guess <- p_guess + p_change
    } else{
      solved <- 1
    }
  }
  
  print(paste0(varname, " Percentile for ", format_as_dollar(amount), " = ", 100*p_guess, "%"))
}

find_percentile(1998, 10^6, "networth_nominal", "Net Worth")
find_percentile(2022, 10^6, "networth_nominal", "Net Worth")

# Now find the 1998 percentile in 2022
to_plot <- scf_stack %>%
                    group_by(year) %>%
                    summarise(pct_95 = wtd.quantile(networth_nominal, weights = wgt, probs = 0.95)) %>%
                    ungroup() %>%
                    gather(-year, key=key, value=value)

start_year <- min(scf_stack$year)
end_year <- max(scf_stack$year)

file_path <- paste0(out_path, "/nw_95_over_time.jpeg")
source_string <- paste0("Source: Survey of Consumer Finances (", start_year, "-", end_year, ")")

plot <- ggplot(to_plot, aes(x= year, y = value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("black"), guide = "none") +
  scale_y_continuous(label = dollar, breaks = seq(0, 4*10^6, 2*10^5)) +
  scale_x_continuous(breaks = seq(start_year, end_year, 3)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("95th Percentile of Net Worth over Time")) +
  labs(x="Year", y="Net Worth (Nominal Dollars)",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #