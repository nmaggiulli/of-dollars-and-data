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
library(quantmod)
library(FinCal)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0454_10yr_dca_summary"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

today <- Sys.Date() 

sp500_raw <- read_excel(paste0(importdir, "0009_sp500_returns_pe/ie_data.xls"),
                        sheet = "Data") 

colnames(sp500_raw) <- c("date", "price", "dividend", "earnings", "cpi_shiller", "date_frac", 
                         "long_irate", "real_price", "real_div", "real_tr",
                         "real_earn", "real_earn_scaled", "cape", "blank", "cape_tr", "blank2",
                         "excess_cape", "orig_nom_bond_ret", "real_bond_index")

#Remove first 6 rows
sp500_raw <- sp500_raw[7:nrow(sp500_raw),]

# Convert vars to numeric
sp500_raw$price <- as.numeric(sp500_raw$price)
sp500_raw$dividend <- as.numeric(sp500_raw$dividend)
sp500_raw$real_div <- as.numeric(sp500_raw$real_div)
sp500_raw$real_price <- as.numeric(sp500_raw$real_price)
sp500_raw$real_tr <- as.numeric(sp500_raw$real_tr)
sp500_raw$cpi_shiller <- as.numeric(sp500_raw$cpi_shiller)
sp500_raw$date <- as.numeric(sp500_raw$date)
sp500_raw$orig_nom_bond_ret <- as.numeric(sp500_raw$orig_nom_bond_ret)
sp500_raw$real_bond_index <- as.numeric(sp500_raw$real_bond_index)

# Create a numeric end date based on the closest start of month to today's date
end_date <- year(Sys.Date()) + month(Sys.Date())/100

# Filter out missing dividends
sp500_subset <- sp500_raw %>%
  select(date, price, dividend, real_div, real_price, real_tr, cpi_shiller, orig_nom_bond_ret, real_bond_index) %>%
  filter(!is.na(date), date < end_date) %>%
  mutate(orig_nom_bond_ret = orig_nom_bond_ret - 1)

# Change the Date to a Date type for plotting the S&P data
sp500_subset <- sp500_subset %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d"))) %>%
  rename(month = date,
         realPrice = real_price,
         realPricePlusDividend = real_tr)

yahoo_start <- max(sp500_subset$month)
yahoo_end <- as.Date(paste0(year(today), "-", month(today), "-01")) - days(1)

#Bring in Yahoo data
getSymbols("^SPX", from = yahoo_start, to = yahoo_end, 
           src="yahoo", periodicity = "daily")

yahoo_daily <- data.frame(date=index(get("SPX")), coredata(get("SPX"))) %>%
  rename(close = `SPX.Adjusted`) %>%
  select(date, close) %>%
  mutate(month = as.Date(paste0(year(date), "-", month(date), "-01")))

yahoo_monthly <- yahoo_daily %>%
  group_by(month) %>%
  summarise(price = mean(close)) %>%
  ungroup()

# Get CPI
getSymbols('CPIAUCNS',src='FRED')

cpi_monthly <- data.frame(date=index(get("CPIAUCNS")), coredata(get("CPIAUCNS"))) %>%
  rename(cpi_fred = `CPIAUCNS`,
         month = date) 

# Get GS10
getSymbols('GS10',src='FRED')

gs10_monthly <- data.frame(date=index(get("GS10")), coredata(get("GS10"))) %>%
  rename(gs10 = `GS10`,
         month = date) 

# Join Shiller, Yahoo, and FRED
sp500_ret_pe <- sp500_subset %>%
  filter(month < yahoo_start) %>%
  bind_rows(yahoo_monthly) %>%
  left_join(cpi_monthly) %>%
  left_join(gs10_monthly) %>%
  mutate(cpi = case_when(
    !is.na(cpi_fred) ~ cpi_fred,
    !is.na(cpi_shiller) ~ cpi_shiller,
    TRUE ~ NA
  )) %>%
  select(-cpi_shiller, -cpi_fred)

sp500_ret_pe$dividend <- na.locf(sp500_ret_pe$dividend)
sp500_ret_pe$real_div <- na.locf(sp500_ret_pe$real_div)

#Estimate CPI data for any months Shiller/FRED is missing (use his formula)
for(i in 1:nrow(sp500_ret_pe)){
  if(is.na(sp500_ret_pe[i, "cpi"])){
    sp500_ret_pe[i, "cpi"] <- 1.5*sp500_ret_pe[(i-1), "cpi"] - 0.5*sp500_ret_pe[(i-2), "cpi"]
  }
}

final_cpi <- sp500_ret_pe[nrow(sp500_ret_pe), "cpi"]

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
    sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[i, "realPrice"]
    
    sp500_ret_pe[i, "nom_sp500_ret"] <- 0
    sp500_ret_pe[i, "real_sp500_ret"] <- 0
    
    sp500_ret_pe[i, "nom_bond_ret"] <- sp500_ret_pe[i, "orig_nom_bond_ret"]
    sp500_ret_pe[i, "real_bond_ret"] <- 0
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "dividend"]
    sp500_ret_pe[i, "nominalPricePlusDividend"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "price"]
    sp500_ret_pe[i, "realPrice"] <- sp500_ret_pe[i, "price"] * final_cpi/sp500_ret_pe[i, "cpi"]
    sp500_ret_pe[i, "realPricePlusDividend"] <- sp500_ret_pe[(i-1), "realPricePlusDividend"]*((sp500_ret_pe[i, "realPrice"] + (sp500_ret_pe[i, "real_div"]/12))/sp500_ret_pe[(i-1), "realPrice"])
    
    sp500_ret_pe[i, "nom_sp500_ret"] <- sp500_ret_pe[i, "nominalPricePlusDividend"]/sp500_ret_pe[(i-1), "nominalPricePlusDividend"] - 1
    sp500_ret_pe[i, "real_sp500_ret"] <- sp500_ret_pe[i, "realPricePlusDividend"]/sp500_ret_pe[(i-1), "realPricePlusDividend"] - 1
    if(is.na(sp500_ret_pe[i, "orig_nom_bond_ret"])){
      pv <- pv(r =  sp500_ret_pe[i, "gs10"]/100,
               n = 10,
               fv = 100,
               pmt = sp500_ret_pe[(i-1), "gs10"],
               type = 0)
      sp500_ret_pe[i, "nom_bond_ret"] <- (-pv/100 - 1) + (sp500_ret_pe[(i-1), "gs10"]/100 + 1)^(1/12) - 1
      
      sp500_ret_pe[i, "real_bond_index"] <- sp500_ret_pe[(i-1), "real_bond_index"] * (1 + sp500_ret_pe[(i-1), "nom_bond_ret"]) * (sp500_ret_pe[i, "cpi"]/sp500_ret_pe[(i-1), "cpi"])
    } else{
      sp500_ret_pe[i, "nom_bond_ret"] <- sp500_ret_pe[i, "orig_nom_bond_ret"]
    }
  }
}

# Now run DCA data
plot_dca <-function(nyrs, wstock){
  
  filter_date <- "1919-12-01"
  n_years <- nyrs
  w_stock <- wstock

  # Final df
  df <- sp500_ret_pe %>%
    filter(month >= filter_date) %>%
    mutate(real_bond_ret = case_when(
      row_number() == 1 ~ real_bond_ret,
      TRUE ~ real_bond_index/lag(real_bond_index, 1) - 1)
    ) %>%
    select(month, real_sp500_ret, real_bond_ret) %>%
    drop_na()
  
  for (i in 1:nrow(df)){
    mt <- month(pull(df[i, "month"]))
    if(i == 1){
      df[i, "value_sp500"] <- w_stock
      df[i, "value_bond"] <- 1 - w_stock
    } else{
      ret_stock <- df[i, "real_sp500_ret"]
      #Rebalance
      if(mt == 1){
        df[i, "value_sp500"] <- (df[(i-1), "value_port"] * w_stock) * (1 + df[i, "real_sp500_ret"])
        df[i, "value_bond"] <- (df[(i-1), "value_port"] * (1 - w_stock)) * (1 + df[i, "real_bond_ret"])
      } else{
        df[i, "value_sp500"] <- df[(i-1), "value_sp500"] * (1 + df[i, "real_sp500_ret"])
        df[i, "value_bond"] <- df[(i-1), "value_bond"] * (1 + df[i, "real_bond_ret"])
      }
    }
    df[i, "value_port"] <- df[i, "value_sp500"] + df[i, "value_bond"]
  }
  
  end_months <- df %>%
                  filter(month >= as.Date(filter_date) + years(n_years) + months(1)) %>%
                  pull(month)
  
  final <- data.frame()
  counter <- 1
  
  for(i in 1:length(end_months)){
    end_mt <- end_months[i]
    start_mt <- end_mt - years(n_years) + months(1)
    end_value <- df %>%
                    filter(month == end_mt) %>%
                    pull(value_port)
    
    tr <- df %>%
          filter(month < end_mt, month >= start_mt) %>%
          mutate(total_return = end_value/value_port) %>%
          pull(total_return)
    
    final[counter, "start_month"] <- start_mt
    final[counter, "end_month"] <- end_mt
    final[counter, "dca_return"] <- mean(tr)
    
    counter <- counter + 1
  }
  
  to_plot <- final %>%
              mutate(lost_money = case_when(
                dca_return < 1 ~ 1,
                TRUE ~ 0
              ))
  
  # Set the file_path based on the function input 
  file_path <- paste0(out_path, "/dca_", n_years, "_", 100*w_stock, "_", (100*(1-w_stock)), "_port.jpeg")
  source_string <- paste0("Source:  Shiller data, FRED (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Assumes an equal monthly contribution, reinvested dividends, and all returns are adjusted for inflation. The portfolio is rebalanced annually each January."),
                          width = 85)
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = end_month, y = dca_return)) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_y_continuous(label = dollar) +
    ggtitle(paste0("Average Real Dollar Growth for\n", n_years, "-Year DCA\n",  100*w_stock, "/", (100*(1-w_stock)), " Portfolio")) +
    of_dollars_and_data_theme +
    labs(x = "Ending Period", y = "Average Real Growth",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm") 
  
  assign("to_plot", to_plot, envir = .GlobalEnv)
}

plot_dca(10, 0.8)
plot_dca(20, 0.8)
plot_dca(10, 0.6)
plot_dca(20, 0.6)

# ############################  End  ################################## #