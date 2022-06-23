cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(tidyverse)

folder_name <- "0302_sp500_dca_simulator"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

monthly_payment <- 100

# Load data
raw <- read.csv(paste0(importdir, "/0302_stock_bond_cpi/DFA_GrowthOfWealth_20220613095224.csv"), skip = 7,
               col.names = c("date", "index_bond", "index_sp500_nom", "index_cpi")) %>%
  filter(!is.na(index_bond)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y") + days(1) - months(1),
         index_sp500_real = index_sp500_nom/index_cpi,
         index_bond_real = index_bond/index_cpi) %>%
  select(date, index_sp500_nom, index_sp500_real, index_bond_real, index_cpi) %>%
  mutate(ret_sp500_nom = index_sp500_nom/lag(index_sp500_nom, 1) - 1,
         ret_sp500_real = index_sp500_real/lag(index_sp500_real, 1) - 1,
         ret_bond_real = index_bond_real/lag(index_bond_real, 1) - 1,
         cpi_rate = index_cpi/lag(index_cpi, 1) - 1)

calculate_dca <- function(start_date, end_date){

  start_dt <- as.Date(start_date)
  end_dt <- as.Date(end_date)
  
  tmp <- raw %>%
          filter(date >= start_dt, date <= end_dt)
  
  tmp$payment <- monthly_payment
  for(i in 1:nrow(tmp)){
    if(i == 1){
      tmp[i, "port_real"] <- monthly_payment * (1 + tmp[i, "ret_sp500_real"])
      tmp[i, "port_nom"] <- monthly_payment * (1 + tmp[i, "ret_sp500_nom"])
      tmp[i, "port_bond"] <- monthly_payment * (1 + tmp[i, "ret_bond_real"])
      tmp[i, "cost_basis"] <- monthly_payment
    } else{
      tmp[i, "port_real"] <- (tmp[(i-1), "port_real"] +  tmp[i, "payment"]) * (1 + tmp[i, "ret_sp500_real"])
      tmp[i, "port_nom"] <- (tmp[(i-1), "port_nom"] +  tmp[i, "payment"]) * (1 + tmp[i, "ret_sp500_nom"])
      tmp[i, "port_bond"] <- (tmp[(i-1), "port_bond"] +  tmp[i, "payment"]) * (1 + tmp[i, "ret_bond_real"])
      tmp[i, "cost_basis"] <- tmp[(i-1), "cost_basis"] + monthly_payment
    }
  }
  
  last_real <- tmp[nrow(tmp), "index_sp500_real"]
  last_nom <- tmp[nrow(tmp), "index_sp500_nom"]
  last_cpi <- tmp[nrow(tmp), "index_cpi"]
  
  to_export <- tmp %>%
                  select(date, payment, cost_basis, port_real, port_nom, port_bond, index_sp500_real, index_sp500_nom, index_cpi) %>%
                  mutate(real_cumulative_ret = last_real/index_sp500_real,
                         nom_cumulative_ret = last_nom/index_sp500_nom,
                         real_payment = payment*1/(last_cpi/index_cpi))

  return(to_export)
}
     
port_66_82_real <- calculate_dca("1966-02-01", "1982-10-01")
            
first_nom <- port_66_82_real %>%
                head(1) %>%
                pull(index_sp500_nom)

first_real <- port_66_82_real %>%
                  head(1) %>%
                  pull(index_sp500_real)

to_plot <- port_66_82_real %>%
              mutate(index_sp500_nom = index_sp500_nom/first_nom,
                     index_sp500_real = index_sp500_real/first_real) %>%
              select(date, index_sp500_nom, index_sp500_real) %>%
              rename(`Nominal` = index_sp500_nom,
                     `Inflation-Adjusted` = index_sp500_real) %>%
              gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/index_sp500_nom_real_1966_1982.jpeg")
source_string <- paste0("Source: Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures include reinvested dividends."),
                        width = 85)

color_nom <- "#ca0020"
color_real <- "blue"
color_cpi <- "black"
color_bond <- color_nom

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1980-01-01")
text_labels[1, "value"] <- 1.20
text_labels[1, "key"] <- "Inflation-Adjusted"

text_labels[2, "date"] <- as.Date("1975-01-01")
text_labels[2, "value"] <- 1.80
text_labels[2, "key"] <- "Nominal"

plot <- ggplot(data = to_plot, aes(x=date, y=value, col = key)) +
  geom_text(data = text_labels, aes(x=date, y=value, col = key, label = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = "none", values = c(color_real, color_nom)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1 in the S&P 500\nFeb 1966 - Oct 1982")) +
  labs(x = "Date", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do DCA
to_plot <- port_66_82_real %>%
  select(date, port_nom, cost_basis) %>%
  rename(`Nominal` = port_nom,
         `Cost Basis` = cost_basis) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dca_sp500_nom_cost_1966_1982.jpeg")
source_string <- paste0("Source: Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures include reinvested dividends. Monthly payments are not adjusted for inflation."),
                        width = 85)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1981-01-01")
text_labels[1, "value"] <- 22000
text_labels[1, "key"] <- "Cost Basis"

text_labels[2, "date"] <- as.Date("1976-01-01")
text_labels[2, "value"] <- 21000
text_labels[2, "key"] <- "Nominal"

plot <- ggplot(data = to_plot, aes(x=date, y=value, col = key)) +
  geom_text(data = text_labels, aes(x=date, y=value, col = key, label = key)) +
  geom_line() +
  scale_y_continuous(label = dollar, limits = c(0, 38000)) +
  scale_color_manual(guide = "none", values = c(color_cpi, color_nom)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("$100 Per Month into the S&P 500\nFeb 1966 - Oct 1982")) +
  labs(x = "Date", y = "Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#DCA inflation adjust
to_plot <- port_66_82_real %>%
  select(date, cost_basis, port_real, port_nom) %>%
  rename(`Cost Basis` = cost_basis,
         `Inflation-Adjusted` = port_real,
         `Nominal` = port_nom) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dca_sp500_all_1966_1982.jpeg")
note_string <- str_wrap(paste0("Note: Figures include reinvested dividends. Monthly payments are not adjusted for inflation."),
                        width = 85)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1981-01-01")
text_labels[1, "value"] <- 22000
text_labels[1, "key"] <- "Cost Basis"

text_labels[2, "date"] <- as.Date("1976-01-01")
text_labels[2, "value"] <- 21000
text_labels[2, "key"] <- "Nominal"

text_labels[3, "date"] <- as.Date("1980-01-01")
text_labels[3, "value"] <- 8500
text_labels[3, "key"] <- "Inflation-Adjusted"

plot <- ggplot(data = to_plot, aes(x=date, y=value, col = key)) +
  geom_text(data = text_labels, aes(x=date, y=value, col = key, label = key)) +
  geom_line() +
  scale_y_continuous(label = dollar, limits = c(0, 38000)) +
  scale_color_manual(guide = "none", values = c(color_cpi, color_real, color_nom)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("$100 Per Month into the S&P 500\nFeb 1966 - Oct 1982")) +
  labs(x = "Date", y = "Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Add bonds
to_plot <- port_66_82_real %>%
  select(date, cost_basis, port_real, port_bond) %>%
  rename(`Cost Basis` = cost_basis,
         `S&P 500` = port_real,
         `5YR Treasuries` = port_bond) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dca_sp500_bond_1966_1982.jpeg")
note_string <- str_wrap(paste0("Note: Figures include reinvested dividends. Monthly payments are not adjusted for inflation."),
                        width = 85)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1981-01-01")
text_labels[1, "value"] <- 22000
text_labels[1, "key"] <- "Cost Basis"

text_labels[2, "date"] <- as.Date("1976-01-01")
text_labels[2, "value"] <- 17000
text_labels[2, "key"] <- "5YR Treasuries"

text_labels[3, "date"] <- as.Date("1980-01-01")
text_labels[3, "value"] <- 8500
text_labels[3, "key"] <- "S&P 500"

plot <- ggplot(data = to_plot, aes(x=date, y=value, col = key)) +
  geom_text(data = text_labels, aes(x=date, y=value, col = key, label = key)) +
  geom_line() +
  scale_y_continuous(label = dollar, limits = c(0, 38000)) +
  scale_color_manual(guide = "none", values = c(color_bond, color_cpi, color_real)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("$100 Per Month into the S&P 500 & Treasuries\nFeb 1966 - Oct 1982")) +
  labs(x = "Date", y = "Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do $100 in stock vs cash
last_real <- port_66_82_real %>%
  tail(1) %>%
  pull(index_sp500_real)

to_plot <- port_66_82_real %>%
  mutate(index_sp500_real = 100*(last_real/index_sp500_real)) %>%
  select(date, index_sp500_real, real_payment) %>%
  rename(`S&P 500` = index_sp500_real,
         `Cash` = real_payment) %>%
  gather(-date, key=key, value = value)

file_path <- paste0(out_path, "/sp500_real_1966_1982.jpeg")
note_string <- str_wrap(paste0("Note: Shows what a $100 payment would be worth by October 1982 if in the S&P 500 or cash (uninvested)."),
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=date, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "identity", width = 31) +
  scale_fill_manual(values = c(color_cpi, color_real)) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("What $100 is Worth in October 1982")) +
  labs(x = "Date", y = "Value in October 1982",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #