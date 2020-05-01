cat("\014") # Clear your console
rm(list = ls()) #clear your environment

setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(stringr)
library(readxl)
library(lubridate)
library(zoo)
library(scales)
library(tidyverse)

folder_name <- "0113_life_cycle_simulation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dfa_data <- read_excel(paste0(importdir, "0113_life_cycle_simulations/dfa_tbill_sp500_5yr.xlsx"), skip = 4) %>%
                filter(!is.na(Date)) %>%
                mutate(Date = as.Date(Date))

colnames(dfa_data) <- c("date", "cpi", "ret_tbill", "ret_sp500", "ret_treasury_5yr")

# Convert S&P 500 rate of return into a nominal rate of return
dfa_data <- dfa_data %>%
              mutate(ret_sp500 = ret_sp500 + cpi)

max_date <- max(dfa_data$date)

# Set arbitrary starting salary
starting_salary <- 50000
salary_string   <- formatC(starting_salary, format="f", big.mark = ",", digits = 0)

run_life_cycle <- function(working_years, spending_years, 
                           savings_rate, 
                           starting_date, wt_sp500){
  
  starting_date <- as.Date(starting_date)
  
  total_years <- working_years + spending_years
  
  if(starting_date + years(total_years) > max_date){
    stop("Working Years + Spending Years goes beyond length of data!!")
  }
  
  df <- dfa_data %>%
          filter(date >= starting_date, date < (starting_date + years(total_years))) %>%
          select(cpi, ret_sp500, ret_treasury_5yr) %>%
          as.matrix()
  
  df <- cbind(df, 
             rep(NA, nrow(df)),
             rep(NA, nrow(df)),
             rep(NA, nrow(df)))
  
  start_date_string <- date_to_string(starting_date)
  
  wt_treasury <- 1 - wt_sp500 
  
  for(i in 1:nrow(df)){
    
    cpi <- df[i, "cpi"]
    ret_sp500 <- df[i, "ret_sp500"] 
    ret_treasury <- df[i, "ret_treasury_5yr"]
    
    if(i == 1){
      df[i, 4] <- starting_salary/12 * savings_rate
      df[i, 5] <- df[i, 4] * (1 + ret_sp500) * wt_sp500
      df[i, 6] <- df[i, 4] * (1 + ret_treasury) * wt_treasury
    } else{
      if (i != (working_years*12 + 1)){
        df[i, 4] <- df[(i-1), 4] * (1 + cpi)
      } else {
        df[i, 4] <- (df[(i-1), 4]/savings_rate)*(1 - savings_rate) * -1
      } 
      
      # Initiate a rebalance every 12 months
      if((i-1) %% 12 == 0){
        old_port_sp500 <- (df[(i-1), 5] +  df[(i-1), 6]) * wt_sp500
        old_port_treasury <- (df[(i-1), 5] +  df[(i-1), 6]) * wt_treasury
      } else{
        old_port_sp500 <- df[(i-1), 5]
        old_port_treasury <- df[(i-1), 6]
      }
      
      df[i, 5] <- (old_port_sp500 +  (df[i, 4] * wt_sp500)) * (1 + ret_sp500)
      df[i, 6] <- (old_port_treasury +  (df[i, 4] * wt_treasury)) * (1 + ret_treasury)
    }

    if(df[i, 5] < 0 | df[i, 6] < 0 | i == nrow(df)){
      return(df)
    }
  } 
}

run_all_life_cycles <- function(working_years, spending_years, 
                                savings_rate_low, savings_rate_high,
                                first_date){
  
  last_date <- max_date - years(working_years + spending_years + 1) + months(1)
  
  all_dates <- seq.Date(as.Date(first_date), as.Date(last_date), by = "year")
  assign("all_dates", all_dates, envir = .GlobalEnv)
  
  counter <- 1
  for(sr in seq(savings_rate_low, savings_rate_high, 0.01)){
    for(wt in sp500_weights){
      for(i in 1:length(all_dates)){
        dt <- all_dates[i]
        print(dt)
        
        m  <- run_life_cycle(working_years, spending_years, sr, dt, wt)
        mf <- na.omit(m[m[,4] < 0, ])
        mf_pos_savings <- na.omit(m[m[,4] > 0, 4])

        if(wt == 0){
          wt_name <- "100% Bonds"
        } else if(wt == 1){
          wt_name <- "100% Stocks"
        } else{
          wt_name <- paste0(100*wt, "/", 100*(1-wt), " Stock/Bond")
        }
        
        df <- data.frame(retirement_start = year(dt) + working_years,
               working_dates = paste0(year(dt),"-", year(dt) + (working_years-1)),
               working_years = working_years,
               savings_rate = sr,
               weight_sp500 = wt_name,
               n_years_survival = length(mf[, 4])/12,
               total_portfolio = mf[1, 5] + mf[1, 6],
               retirement_start_monthly_spend = mf[1, 4]*-1,
               final_year_income = mf_pos_savings[length(mf_pos_savings)]/sr*12,
               total_savings = sum(mf_pos_savings)
        )
        if(counter == 1){
          final_df <- df
        } else{
          final_df <- bind_rows(final_df, df)
        }
        counter <- counter + 1
      }
    }
  }
  
  date_string <- date_to_string(first_date)
  assign(paste0("all_results"), final_df, envir=.GlobalEnv)
}

# Set savings rates, working years, and spending years
min_sr       <- 0.05
max_sr       <- 0.2
working_yrs  <- 40
spending_yrs <- 25
sp500_weights <- c(0, 0.6, 1)

run_all_life_cycles(working_yrs, 
                    spending_yrs, 
                    min_sr, max_sr,
                    "1926-01-01")

for (sr in seq(min_sr, max_sr, 0.01)){
  print(sr)
  if(sr < 0.1){
    sr_string <- paste0("0", 100*sr)
  } else{
    sr_string <- paste0(100*sr)
  }
  
  file_path <- paste0(out_path, "/survival_yrs_", sr_string, ".jpeg")
  
  source_string <- paste0("Source:  DFA, 1926-2018 (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note: Assumes a starting salary of $", salary_string," that grows with inflation while saved money grows at the portfolio return level.   ",
                                 "Spending throughout retirement is indexed to inflation.  ",
                                 "'Stocks' are represented by the S&P 500 and 'Bonds' are represented by 5-Year U.S. Treasuries.  ",
                                 "The portfolio is rebalanced annually."), 
                          width = 80)
    
  to_plot <- all_results %>%
              filter(savings_rate == sr)
  
  plot <- ggplot(to_plot, aes(x=retirement_start, y = n_years_survival, col = as.factor(weight_sp500), shape = as.factor(weight_sp500))) +
            geom_point() +
            scale_y_continuous(limits = c(0, spending_yrs), breaks = seq(0, spending_yrs, 5)) +
            of_dollars_and_data_theme +
            theme(legend.position = "bottom",
                  legend.title = element_blank()) +
            ggtitle(paste0("Number of Years Until You Run Out of Money\nWith a ", 100*sr, "% Savings Rate")) +
            labs(x="Retirement Start Year", y="Number of Years",
                 caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(sr_string == "10"){
    # Plot portfolio value as well
    file_path <- paste0(out_path, "/retirement_start_p_value_", sr_string, ".jpeg")
    
    y_max <- round_to_nearest(max(to_plot$total_portfolio), "up", 10^6)
    
    assign("to_plot_10pct", to_plot, envir = .GlobalEnv)

    plot <- ggplot(to_plot, aes(x=retirement_start, y = total_portfolio, col = as.factor(weight_sp500), shape = as.factor(weight_sp500))) +
      geom_point() +
      scale_y_continuous(label = dollar, limits = c(0, y_max)) +
      of_dollars_and_data_theme +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste0("Retirement Start Portfolio Value\nWith a ", 100*sr, "% Savings Rate")) +
      labs(x="Retirement Start Year", y="Portfolio Value",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
    
    # Final income by retirement year
    file_path <- paste0(out_path, "/final_income.jpeg")
    
    y_max <- round_to_nearest(max(to_plot$final_year_income), "up", 10^5)
    
    plot <- ggplot(to_plot, aes(x=retirement_start, y = final_year_income)) +
      geom_point() +
      scale_y_continuous(label = dollar, limits = c(0, y_max)) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Final Annual Salary by Retirement Year")) +
      labs(x="Retirement Start Year", y="Final Annual Salary",
           caption = paste0(source_string, "\nNote:  Assumes a starting salary of $", salary_string, " that grows with inflation.")
      )
    
    # Save the plot
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

create_gif(out_path,
           paste0("survival_yrs_*.jpeg"),
           40,
           0,
           paste0("_gif_survival_yrs_retirement.gif"))

# Returns by year (for checking)
dfa_data_by_year <- dfa_data %>%
                      mutate(yr = year(date)) %>%
                      group_by(yr) %>%
                      summarize(ret_sp500 = prod(1+ret_sp500) - 1,
                                ret_treasury_5yr = prod(1+ret_treasury_5yr) - 1) %>%
                      ungroup()

# Create decade stock and bond plots
to_plot <- dfa_data %>%
                  filter(date >= "1930-01-01", date < "2000-01-01") %>%
                  mutate(decade = year(floor_date(date, years(10)))) %>%
                  group_by(decade) %>%
                  summarize(`Stocks` = prod(1+ret_sp500 - cpi)^(1/10) - 1,
                            `Bonds` = prod(1+ret_treasury_5yr - cpi)^(1/10) - 1) %>%
                  ungroup() %>%
                  gather(-decade, key=key, value=value)

file_path <- paste0(out_path, "/decade_plot_stocks_bonds.jpeg")

source_string <- paste0("Source:  DFA, 1926-2018 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  'Stocks' are represented by the S&P 500 and 'Bonds' are represented by 5-Year U.S. Treasuries."), 
                        width = 80)

plot <- ggplot(to_plot, aes(x=as.factor(decade), y=value, fill = key)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "dark green")) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Each Decade Brings Different Challenges") +
  labs(x = "Decade", y="Total Real Annualized Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #