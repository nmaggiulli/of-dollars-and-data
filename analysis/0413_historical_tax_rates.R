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
library(quantmod)
library(tidyverse)

folder_name <- "0413_historical_tax_rates"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in tax rates
raw <- read_excel(paste0(importdir, "/", folder_name, "/Historical Income Tax Rates and Brackets, 1862-2021.xlsx"),
                  skip = 1) %>%
          clean_cols() %>%
          rename(single_rate = `single_filer_(rates_brackets)`,
                 single_bracket = `___10`) %>%
          select(year, single_bracket, single_rate) %>%
          drop_na() %>%
          filter(year >= 1913) %>%
          arrange(year, single_bracket)

#Do 2022 to 2024
brackets_2022 <- data.frame(year = rep(2022,7),
                            single_bracket = c(0, 10275, 41775, 89075,
                                               170050, 215950, 539900),
                            single_rate = c(0.1, 0.12, 0.22, 0.24,
                                            0.32, 0.35, 0.37))

brackets_2023 <- data.frame(year = rep(2023,7),
                            single_bracket = c(0, 11000, 44725, 95375,
                                               182100, 231250, 578125),
                            single_rate = c(0.1, 0.12, 0.22, 0.24,
                                            0.32, 0.35, 0.37))

brackets_2024 <- data.frame(year = rep(2024,7),
                            single_bracket = c(0, 11600, 47150, 100525,
                                               191950, 243725, 609350),
                            single_rate = c(0.1, 0.12, 0.22, 0.24,
                                            0.32, 0.35, 0.37))

#Now do standard deductions from 1944 onward
standard_deduction <- data.frame(year = seq(1944, 2024, 1),
                                 standard_deduction = c(
                                   rep(1000, 26),
                                   1100, 1050, 1300, 1300, 1300, 1600, 1700, 2200, 2200, 2300,
                                   rep(2300, 5), 2400, 2480, 2540, 3000, 3100,
                                   3250, 3400, 3600, 3700, 3800, 3900, 4000, 4150, 4250, 4300,
                                   4400, 4550, 4700, 4750, 4850, 5000, 5150, 5350, 5450, 5700,
                                   5700, 5800, 5950, 6100, 6200, 6300, 6300, 6350, 12000, 12200,
                                   12400, 12550, 12950, 13850, 14600
                                 ))

#Combine them all
all_historical <- raw %>%
                    bind_rows(brackets_2022, brackets_2023, brackets_2024) %>%
                    arrange(year, single_bracket)

#Get CPI
getSymbols('CPIAUCNS',src='FRED')

raw_cpi <- data.frame(date=index(get("CPIAUCNS")), coredata(get("CPIAUCNS"))) %>%
                rename(cpi = `CPIAUCNS`) %>%
                filter(month(date) == 1) %>%
                mutate(year = year(date)) %>%
                select(year, cpi)

last_cpi <- raw_cpi[nrow(raw_cpi), "cpi"]

final_cpi <- raw_cpi %>%
                mutate(cpi_deflator = cpi/last_cpi) %>%
                left_join(standard_deduction) %>%
                mutate(standard_deduction = case_when(
                  year < 1944 ~ 0,
                  TRUE ~ standard_deduction
                ))

all_years <- unique(all_historical$year)

calculate_historical <- function(income){
  counter <- 1
  final_df <- data.frame()
  for(y in all_years){
    
    tmp_yr <- all_historical %>%
                filter(year == y)
    
    cpi_deflator <- final_cpi %>%
                      filter(year == y) %>%
                      pull(cpi_deflator)
    
    tmp_standard_deduction <- final_cpi %>%
                                filter(year == y) %>%
                                pull(standard_deduction)
    
    cpi_taxable_income <- (income*cpi_deflator) - tmp_standard_deduction
    cpi_income <- (income*cpi_deflator)
    
    for(i in 1:nrow(tmp_yr)){
      if(i == nrow(tmp_yr)){
        current_bracket <- tmp_yr[i, "single_bracket"]
        
        if(cpi_taxable_income > current_bracket){
          tmp_yr[i, "tax"] <- (cpi_taxable_income - current_bracket) * tmp_yr[i, "single_rate"]
        } else{
          tmp_yr[i, "tax"] <- 0
        }
      } else{
        current_bracket <- tmp_yr[i, "single_bracket"]
        next_bracket <- tmp_yr[(i+1), "single_bracket"]
        
        if(cpi_taxable_income > next_bracket){
          tmp_yr[i, "tax"] <- (next_bracket - current_bracket) * tmp_yr[i, "single_rate"]
        } else if(cpi_taxable_income < current_bracket){
          tmp_yr[i, "tax"] <- 0
        } else{
          tmp_yr[i, "tax"] <- (cpi_taxable_income - current_bracket) * tmp_yr[i, "single_rate"]
        }
      }
    }
    
    final_df[counter, "year"] <- y
    final_df[counter, "income"] <- income
    final_df[counter, "cpi_taxable_income"] <- cpi_taxable_income
    final_df[counter, "total_tax"] <- sum(tmp_yr$tax)
    final_df[counter, "effective_rate"] <- final_df[counter, "total_tax"]/cpi_income
    
    counter <- counter + 1
  }
  
  return(final_df)
}

result_100k <- calculate_historical(100000)
result_50k <- calculate_historical(50000)
result_200k <- calculate_historical(200000)
result_1m <- calculate_historical(1000000)
result_100m <- calculate_historical(100000000)

to_plot <- result_50k %>%
              bind_rows(result_100k, result_200k, result_1m, result_100m) %>%
              rowwise() %>%
              mutate(income_label = format_as_dollar(income))

to_plot$income_label <- factor(to_plot$income_label, levels = c("$100,000,000", 
                                                                "$1,000,000", "$200,000",
                                                                "$100,000", "$50,000"))

first_year <- min(to_plot$year)
last_year <- max(to_plot$year)

file_path <- paste0(out_path, "/historical_effective_tax_rates_", last_year, ".jpeg")
source_string <- paste0("Source: Tax Foundation, Tax Policy Center (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Assumes a single filer with the standard deduction (which started in 1944) and no other credits/exemptions. Incomes are adjusted for inflation.")
                         , width = 80)

plot <- ggplot(to_plot, aes(x=year, y=effective_rate, col = income_label)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Effective U.S. Federal Tax Rate by Income\nAdjusted for Inflation\n", first_year, "-", last_year)) +
  labs(x="Year", y="Effective Tax Rate",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #