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
library(psidR)
library(data.table)
library(devtools)
library(quantmod)
library(Hmisc)
library(tidyverse)

folder_name <- "0408_psid_lifestyle_creep"

out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

psid_path <- paste0(importdir, "_twl/0001_psid/")
r <- system.file(package="psidR")
#install_github("floswald/psidR")

wealth_years <- c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007,
                  2009, 2011, 2013, 2015, 2017, 2019, 2021)

# Select vars we care about and the years we care about
f <- fread(file.path(r,"psid-lists","famvars.txt")) %>%
        filter(name %in% c("wealth", "hvalue", "faminc", "hours"),
               year %in% wealth_years)

i <- fread(file.path(r,"psid-lists","indvars.txt")) %>%
        filter(name %in% c("age", "weight"),
               year %in% wealth_years)

# Reformat the shape
i <- dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f <- dcast(f[,list(year,name,variable)],year~name, value.var = "variable")

#  Wealth =
# ER81778/S103 Business asset +
# ER81784/S105 Checking/MM +
# ER81788 CD/Bonds +
# ER81792/S109 Real Estate +
# ER81798/S111 Stocks +
# ER81800/S113 Vehicles +
# ER81804/S115 Other + 
# ER81808 Private annuity/IRA +
# Home Equity +
# DC Assets (pension value)
# minus all debts

#Add 2019 and 2021 variables
f <- f %>% bind_rows(data.frame(year = c(2019, 2021),
                    faminc = c("ER77448", "ER81775"),
                    hvalue = c("ER72031", "ER78032"),
                    hours = c("ER77255", "ER81582"),
                     wealth = c ("ER77511", "ER81838"))) %>%
          left_join(data.frame(
            year = wealth_years,
            value_pension = c("V10498", NA, NA,"ER15181", "ER19349", "ER22744", "ER26725", 
                              "ER37761", "ER43734", "ER49080", "ER54836", "ER61956", "ER68010", 
                              "ER74036", "ER80159"),
            total_expenditure = c(NA, NA, NA, "ER16515D7", "ER20456D7", "ER24138D7", "ER28037E4", 
                                  "ER41027E4", "ER46971E4", "ER52395E4", "ER58212E4", "ER65448B", "ER71527B", 
                                  "ER77587", "ER81914")
          ))

i <- i %>% bind_rows(data.frame(year = c(2019, 2021),
                                age = c ("ER34704", "ER34904"),
                                weight = c("ER34863", "ER35064")))

# Build the full panel data (we will add 1984-2007 after)
full_psid <- build.panel(datadir=psid_path,
                fam.vars=f,
                ind.vars=i,
                heads.only = TRUE,
                sample = "SRC",
                design="balanced") %>%
            mutate(value_pension = case_when(
              value_pension > 10000000 ~ 0,
              value_pension == 999998 ~ 0,
              is.na(value_pension) ~ 0,
              TRUE ~ value_pension
            ))

#Loop through years 1984-2007
supplemental_years <- wealth_years[wealth_years < 2009]

for(s in supplemental_years){
  print(s)
  # Specify the name of the text file within the ZIP directory
  zip_file <- paste0(psid_path, "wlth", s, ".zip")
  txt_file <- paste0("WLTH", s, ".txt")
  
  # Open a connection to the ZIP file
  zip_connection <- unz(zip_file, txt_file)
  
  psid_supp_cols <- read_excel(paste0(psid_path, "psid_supp_wealth_columns.xlsx")) %>%
                      filter(year == s) %>%
                      select(year, colnum, width, colname)
  
  widths <- psid_supp_cols %>%
              arrange(colnum) %>%
              select(width) %>%
              pull()
  
  colnames <- psid_supp_cols %>%
                  arrange(colnum) %>%
                  select(colname) %>%
                  pull()
  
  tmp2 <- read.fwf(zip_connection, 
                   widths,
                   col.names = colnames,
                  header = FALSE, 
                  skip = 0) %>%
            mutate(year = s) %>%
            select(year, interview, supp_wealth)
  
  if(s == min(supplemental_years)){
    supp_stack <- tmp2
  } else{
    supp_stack <- supp_stack %>% bind_rows(tmp2)
  }
}

#Get CPI data for inflation adjustment
getSymbols('CPIAUCNS',src='FRED')

cpi_all_years <- data.frame(date=index(get("CPIAUCNS")), coredata(get("CPIAUCNS"))) %>%
  rename(cpi = `CPIAUCNS`) %>%
  mutate(year = year(date)) %>%
  filter(month(date) == 1, year %in% wealth_years) %>%
  select(year, cpi)

cpi_inflator <- cpi_all_years %>%
                  mutate(cpi_inflator = cpi_all_years[nrow(cpi_all_years), "cpi"]/cpi) %>%
                  select(year, cpi_inflator)
                        
# Join all data sources and deflate by CPI
full_psid_supp <- full_psid %>%
                    left_join(supp_stack) %>%
                    left_join(cpi_inflator) %>%
                    mutate(networth = case_when(
                      year < 2009 ~ (supp_wealth+value_pension)*cpi_inflator,
                      TRUE ~ (wealth+value_pension)*cpi_inflator
                    )) %>%
                    select(year, interview, weight, pernum, networth,
                           hvalue, faminc, hours, value_pension, total_expenditure, cpi_inflator,
                           ID1968, age)

missing_nw_id_to_remove <- full_psid_supp %>%
  filter(is.na(networth)) %>%
  pull(ID1968)

# Now just keep one observation per HH
full_pre_89_94 <- full_psid_supp %>%
            filter(ID1968 != missing_nw_id_to_remove, 
                   weight > 0) %>%
            arrange(ID1968, year, pernum) %>%
            group_by(year, ID1968) %>%
            slice(1) %>%
            ungroup()

#Grab 1984 pension values for 89 and 94
pension_1984 <- full_pre_89_94 %>%
  filter(year == 1984) %>%
  rename(pension_1984 = value_pension) %>%
  select(ID1968, pension_1984)

full_data <- full_pre_89_94 %>%
                left_join(pension_1984) %>%
                mutate(networth = case_when(
                  year %in% c(1989, 1994) ~ networth + (cpi_inflator*pension_1984),
                  TRUE ~ networth
                ),
                total_expenditure = total_expenditure*cpi_inflator)

plot_inc_spend_change <- function(start_yr, end_yr){
  
  start_data <- full_data %>%
    filter(year == start_yr) %>%
    select(year, weight, ID1968, age, networth,
           hvalue, faminc, hours, total_expenditure) %>%
    rename(
           start_nw = networth,
           start_age = age,
           start_hvalue = hvalue,
           start_faminc = faminc,
           start_hours = hours,
           start_expenditure = total_expenditure)
  
  end_data <- full_data %>%
    filter(year == end_yr) %>%
    select(ID1968, age, networth, 
           hvalue, faminc, hours, total_expenditure) %>%
    rename(end_nw = networth,
           end_age = age,
           end_hvalue = hvalue,
           end_faminc = faminc,
           end_hours = hours,
           end_expenditure = total_expenditure)
  
  merged <- start_data %>%
              left_join(end_data) %>%
              mutate(nw_change = end_nw - start_nw,
                     income_change = case_when(
                       end_faminc - start_faminc > 100000 ~ 100000,
                       end_faminc - start_faminc < -100000 ~ -100000,
                       TRUE ~ end_faminc - start_faminc
                     ),
                     spending_change = case_when(
                       end_expenditure - start_expenditure > 100000 ~ 100000,
                       end_expenditure - start_expenditure < -100000 ~ -100000,
                       TRUE ~ end_expenditure - start_expenditure
                     ),
                     l_creep = case_when(
                       income_change - spending_change > 0 & spending_change < 0 ~ "No Lifestyle Creep",
                       income_change - spending_change > 0 & spending_change/income_change < 0.75 ~ "Some Lifestyle Creep",
                       TRUE ~ "Serious Lifestyle Creep"
                       )) %>%
                  filter(income_change > 1000, end_age < 65)
  
  assign(paste0("merged_", start_yr), merged, envir = .GlobalEnv)  
  
  to_plot <- merged 
  
  summary_stats <- to_plot %>%
                      mutate(increase_bin = case_when(
                        income_change < 10000 ~ "<$10k",
                        income_change < 25000 ~ "$10k-$25k",
                        income_change < 50000 ~ "$25k-$50k",
                        income_change < 100000 ~ "$50k-$100k",
                        TRUE ~ "$100k+"
                      )) %>%
                      group_by(increase_bin) %>%
                      summarise(
                        spend_change_25pct = quantile(spending_change, weight = weight, probs = 0.25),
                        spend_change_50pct = quantile(spending_change, weight = weight, probs = 0.5),
                        spend_change_75pct = quantile(spending_change, weight = weight, probs = 0.75)
                      ) %>%
                      ungroup() 
  
  summary_stats$increase_bin <- factor(summary_stats$increase_bin, 
                                       levels = c("<$10k", "$10k-$25k", "$25k-$50k", "$50k-$100k", "$100k+"))
  
  summary_stats <- summary_stats %>%
                    arrange(increase_bin)
                      
  assign(paste0("stats_", start_yr), summary_stats, envir = .GlobalEnv)                    
  
  to_plot$l_creep <- factor(to_plot$l_creep, levels = c("No Lifestyle Creep", "Some Lifestyle Creep", "Serious Lifestyle Creep"))
  
  assign(paste0("to_plot_", start_yr), to_plot, envir = .GlobalEnv)
  
  max_change <- round_to_nearest(max(to_plot$income_change), "up", 100000)
  
  file_path <- paste0(out_path, "/psid_income_change_vs_spending_change_", start_yr, "_", end_yr, ".jpeg")
  source_string <- paste0("Source: PSID Data (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note: All figures are adjusted for inflation in 2021 dollars. ",
                                  "Excludes households with income increases less than $1,000 and those 65 and older. ",
                                  "All income and spending changes have been capped at $100,000 for easier visualization.")
                           , width = 80)
  
  plot <- ggplot(to_plot, aes(x=income_change, y=spending_change, col = l_creep)) +
    geom_point() +
    scale_color_manual(values = c("green","black", "red")) +
    scale_x_continuous(label = dollar, limits = c(0, 100000)) + 
    scale_y_continuous(label = dollar, limits = c(-100000, 100000)) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Spending Change vs. Income Change\nU.S. Households\n", start_yr, "—", end_yr)) +
    labs(x="Income Change (over 10 Years)", y="Spending Change (over 10 Years)",
         caption = paste0(source_string, "\n", note_string))
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_inc_spend_change(1999, 2009)
plot_inc_spend_change(2001, 2011)
plot_inc_spend_change(2003, 2013)
plot_inc_spend_change(2005, 2015)
plot_inc_spend_change(2007, 2017)
plot_inc_spend_change(2009, 2019)
plot_inc_spend_change(2011, 2021)

all_merged <- merged_1999 %>%
                  bind_rows(merged_2001, merged_2003, merged_2005,
                            merged_2007, merged_2009, merged_2011)

summary_stats <- all_merged %>%
  mutate(increase_bin = case_when(
    income_change < 10000 ~ "<$10k",
    income_change < 25000 ~ "$10k-$25k",
    income_change < 50000 ~ "$25k-$50k",
    income_change < 100000 ~ "$50k-$100k",
    TRUE ~ "$100k+"
  )) %>%
  group_by(increase_bin) %>%
  summarise(
    spend_change_25pct = quantile(spending_change, weight = weight, probs = 0.25),
    spend_change_50pct = quantile(spending_change, weight = weight, probs = 0.5),
    spend_change_75pct = quantile(spending_change, weight = weight, probs = 0.75)
  ) %>%
  ungroup() 

summary_stats$increase_bin <- factor(summary_stats$increase_bin, 
                                     levels = c("<$10k", "$10k-$25k", "$25k-$50k", "$50k-$100k", "$100k+"))

summary_stats <- summary_stats %>%
  arrange(increase_bin)

# ############################  End  ################################## #