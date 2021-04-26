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

folder_name <- "0210_max_401k"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

annual_savings <- 10000
cap_gains <- 0.15
annual_dividend <- 0.02
annual_growth <- 0.05
inc_tax <- 0.24

# Plan fee of 73 bps equates Roth with Taxable account over 30 years (cap gains at 15%)
# Plan fee of 153 bps equates Roth with Taxable account over 30 years (cap gains at 30%)
plan_fee <- 0.0

final_results <- data.frame()
tax_rates <- seq(0, 0.34, 0.01)

# Pretax parameters
counter <- 1
for(tax_rate_retire in tax_rates){
  n_years <- 30
  
  df <- data.frame()
  
  for(i in 1:n_years){
    df[i, "year"] <- i
    if(i == 1){
      df[i, "value_roth401k"] <- annual_savings
      df[i, "value_trad401k"] <- annual_savings/(1-inc_tax)
      df[i, "post_tax_dividend"] <- 0
      df[i, "unrealized_gains"] <- 0
      df[i, "cum_unrealized_gains"] <- 0
      df[i, "value_taxable"] <- annual_savings
      df[i, "liquid_taxable"] <- annual_savings
    } else{
      df[i, "post_tax_dividend"] <- (df[(i-1), "value_taxable"] * annual_dividend) * (1 - cap_gains)
      df[i, "unrealized_gains"] <- df[(i-1), "value_taxable"] * annual_growth
      df[i, "cum_unrealized_gains"] <- df[(i-1), "cum_unrealized_gains"] + df[i, "unrealized_gains"]
      df[i, "value_taxable"] <- df[(i-1), "value_taxable"] + df[i, "unrealized_gains"] + df[i, "post_tax_dividend"] + annual_savings
      df[i, "liquid_taxable"] <- df[i, "value_taxable"] - (cap_gains*df[i, "cum_unrealized_gains"]) 
      
      df[i, "value_roth401k"] <- (df[(i-1), "value_roth401k"] * (1+annual_growth+annual_dividend-plan_fee)) + annual_savings
      df[i, "value_trad401k"] <- (df[(i-1), "value_trad401k"] * (1+annual_growth+annual_dividend)) + (annual_savings/(1-inc_tax))
    }
    df[i, "liquid_trad401k"] <- df[i, "value_trad401k"] * (1-tax_rate_retire)
    
    df[i, "roth401k_tot_premium"] <- df[i, "value_roth401k"]/df[i, "liquid_taxable"]
    df[i, "trad401k_tot_premium"] <- df[i, "liquid_trad401k"]/df[i, "liquid_taxable"]
    
    df[i, "roth401k_ann_premium"] <- df[i, "roth401k_tot_premium"]^(1/i) - 1
    df[i, "trad401k_ann_premium"] <- df[i, "trad401k_tot_premium"]^(1/i) - 1
  }
  final_results[counter, "tax_rate"] <- tax_rate_retire
  final_results[counter, "trad401k_ann_premium"] <- df[nrow(df), "trad401k_ann_premium"]
  final_results[counter, "trad401k_tot_premium"] <- df[nrow(df), "trad401k_tot_premium"]
  final_results[counter, "trad401k_value_premium"] <- df[nrow(df), "liquid_trad401k"] - df[nrow(df), "liquid_taxable"]
  
  counter <- counter + 1
}

file_path <- paste0(out_path, "/roth_vs_taxable_balance_40yr.jpeg")
source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Assumes a ", 100*(annual_growth+annual_dividend), 
                               "% annual growth rate, a ", 100*cap_gains, "% tax rate on capital gains, ",
                               format_as_dollar(annual_savings, 0), " in annual savings, and a ", n_years, 
                               "-year investment period."),
                        width = 85)

to_plot <- df %>%
            select(year, value_roth401k, liquid_taxable) %>%
            rename(`Roth 401k` = value_roth401k,
                   `Taxable Account` = liquid_taxable) %>%
            gather(-year, key=key, value=value)

text_labels <- to_plot %>%
                  filter(year == max(to_plot$year)) %>%
                  mutate(label = paste0(format_as_dollar(value/1000000, 1), "M"))

# Plot the results
plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=year, y=value, col = key, label = label), hjust=-0.1,
            show.legend = FALSE) +
  scale_color_manual(values = c("blue", "black")) +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(limits = c(0, 45)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Roth 401(k) vs. Taxable Account Value")) +
  labs(x = "Year" , y = paste0("Value After Taxes"),
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot vs tax rates
file_path <- paste0(out_path, "/trad401k_vs_taxable_ann_premium.jpeg")
note_string <- str_wrap(paste0("Note: Assumes a ", 100*(annual_growth+annual_dividend), 
                               "% annual growth rate, a ", 100*cap_gains, "% tax rate on capital gains, a ",
                               100*inc_tax, "% tax rate on income while working, and a ", n_years, 
                               "-year investment period."),
                        width = 85)

to_plot <- final_results

point <- to_plot %>% filter(tax_rate == inc_tax)

plot <- ggplot(to_plot, aes(x = tax_rate, y = trad401k_ann_premium)) +
  geom_line() +
  geom_vline(xintercept = inc_tax, linetype = "dashed") +
  geom_point(data=point, aes(x=tax_rate, y=trad401k_ann_premium), col = "red", size = 2) +
  scale_color_manual(values = c("black")) +
  scale_y_continuous(label = percent_format(accuracy = 0.01), limits = c(-0.0002, 0.0105), breaks = seq(0, 0.01, 0.0025)) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Annualized Traditional 401(k) Premium\nBased on Tax Rate in Retirement")) +
  labs(x = "Income Tax Rate in Retirement" , y = paste0("Annualized Traditional 401(k) Premium"),
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #