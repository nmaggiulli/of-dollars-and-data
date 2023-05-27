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
library(tidyverse)

folder_name <- "0350_down_sideways_markets"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

dd <- sp500_ret_pe %>%
          select(date, price_plus_div) %>%
          drawdown_path()

first <- pull(sp500_ret_pe[1, "price_plus_div"])

df <- sp500_ret_pe %>%
              left_join(dd) %>%
              mutate(growth_of_dollar = price_plus_div/first,
                    down_sideways = case_when(
                      abs(growth_of_dollar/lag(growth_of_dollar, 9) - 1) < 0.02 & pct < -0.2 ~ 1,
                      TRUE ~ 0
                    ),
                    sideways = case_when(
                      abs(growth_of_dollar/lag(growth_of_dollar, 28) - 1) < 0.02 ~ 1,
                      TRUE ~ 0
                    ),
                    lead_ret_12m = lead(growth_of_dollar, 12)/growth_of_dollar - 1, 
                    lead_ret_60m = lead(growth_of_dollar, 60)/growth_of_dollar - 1, 
                     )

#Plot the down then sideways market
first <- df %>%
          filter(date == "2021-12-01") %>%
          pull(growth_of_dollar)

to_plot <- df %>%
            filter(date >= "2021-12-01") %>%
            mutate(growth_of_dollar = growth_of_dollar/first)

file_path <- paste0(out_path, "/down_sideways_2021_12_01.jpeg")
source_string <- str_wrap(paste0("Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns include dividends and are adjusted for inflation."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=date, y=growth_of_dollar)) +
  geom_line(col = "black") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("The Down & Sideways Market of\n2022 & 2023")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

all_down_sideways_markets <- df %>%
                        filter(down_sideways == 1)

to_plot <- df

file_path <- paste0(out_path, "/sp500_w_down_sideways_markets.jpeg")
source_string <- str_wrap(paste0("Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns include dividends and are adjusted for inflation. ",
                        "Dots show every case where the market was down at least 20% and flat (<2% change) over the prior 9 months."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=date, y=growth_of_dollar)) +
  geom_line(col = "black") +
  geom_point(data= all_down_sideways_markets, aes(x=date, y=growth_of_dollar), col = "red", alpha = 0.5) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Down & Sideways Market\nIn U.S. Stock History")) +
  labs(x = "Date" , y = "Growth of $1 (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do sideways only
all_sideways_markets <- df %>%
  filter(sideways == 1)

to_plot <- df

file_path <- paste0(out_path, "/sp500_w_sideways_markets.jpeg")
source_string <- str_wrap(paste0("Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Returns include dividends and are adjusted for inflation. ",
                               "Dots show every case where the market was flat (<2% change) over the prior 28 months."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=date, y=growth_of_dollar)) +
  geom_line(col = "black") +
  geom_point(data= all_sideways_markets, aes(x=date, y=growth_of_dollar), col = "red", alpha = 0.5) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Every Sideways Market\nIn U.S. Stock History")) +
  labs(x = "Date" , y = "Growth of $1 (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Down Sideways summary
summary <- df %>%
            filter(!is.na(lead_ret_60m)) %>%
            group_by(down_sideways) %>%
            summarise(n_periods = n(),
              median_12m_ret = quantile(lead_ret_12m, probs = 0.5),
              median_60m_ret = (1+quantile(lead_ret_60m, probs = 0.5))^(1/5) - 1) %>%
            ungroup()

non_na_sideways <- df %>%
  filter(!is.na(lead_ret_12m), down_sideways == 1)

plot_fwd_months <- function(n_months, title){
  
  for(i in 1:nrow(non_na_sideways)){
    dt <- pull(non_na_sideways[i, "date"])
    
    tmp <- df %>%
      filter(date >= dt, date <= dt + months(n_months)) 
    
    first <- pull(tmp[1, "growth_of_dollar"])
    
    tmp2 <- tmp %>%
      mutate(growth_of_dollar = growth_of_dollar/first,
             month = row_number(),
             start_date = as.character(dt))
    
    if(i == 1){
      stack <- tmp2
    } else{
      stack <- stack %>% bind_rows(tmp2)
    }
  }
  
  n_periods <- length(unique(stack$start_date))
  
  avg <- stack %>%
    group_by(month) %>%
    summarise(growth_of_dollar = mean(growth_of_dollar)) %>%
    ungroup() %>%
    mutate(start_date = "zAverage")
  
  to_plot <- stack %>%
    bind_rows(avg)
  
  # For plot titles
  if(title == "One"){
    year_years <- "Year" 
  } else{
    year_years <- "Years"
  }
  
  file_path <- paste0(out_path, "/growth_of_dollar_", n_months, "m_sideways_down_markets.jpeg")
  source_string <- str_wrap(paste0("Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"),
                            width = 85)
  note_string <- str_wrap(paste0("Note: Returns include dividends and are adjusted for inflation. ",
                                 "Down & sideways markets are defined as anytime where the market was down at least 20% and flat (<2% change) over the prior 9 months."),
                          width = 80)
  
  plot <- ggplot(to_plot, aes(x=month, y=growth_of_dollar, col = start_date)) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed", col = "black") +
    scale_y_continuous(label = dollar) +
    scale_color_manual(values = c(rep("gray", n_periods), "red"), guide = "none") +
    of_dollars_and_data_theme +
    ggtitle(paste0("How U.S. Stocks Perform ", title, " ", year_years, " After\nBeing Down & Sideways")) +
    labs(x = "Month" , y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  # Now do distribution
  to_plot <- df %>%
              filter(!is.na(lead_ret_60m)) %>%
              rename_(.dots = setNames(paste0("lead_ret_", n_months, "m"), "ret")) %>%
              mutate(group = ifelse(down_sideways == 1, "Down & Sideways", "All Other Times"))
  
  file_path <- paste0(out_path, "/future_dist_", n_months, "m_down_sideways_markets.jpeg")
  
  plot <- ggplot(to_plot, aes(ret, fill = group)) +
    geom_density(alpha = 0.7) +
    scale_x_continuous(label = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("grey", "red")) +
    of_dollars_and_data_theme +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle(paste0("U.S. Market Performance Over ", title, " ", year_years, "\nWhen Down & Sideways vs. All Other Times")) +
    labs(x = paste0(title, "-Year Future Return") , y = "Density",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_fwd_months(12, "One")
plot_fwd_months(60, "Five")

sideways_and_down_sideways <- df %>%
                              filter(sideways == 1, down_sideways == 1)

t.test(df %>% filter(down_sideways == 1) %>% pull(lead_ret_60m),
       df %>% filter(down_sideways == 0) %>% pull(lead_ret_60m))

# ############################  End  ################################## #