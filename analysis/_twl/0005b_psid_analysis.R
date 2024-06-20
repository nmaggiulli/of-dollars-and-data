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

folder_name <- "_twl/0005b_psid_analysis"

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
                              "ER74036", "ER80159")
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
            rename() %>%
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
                           hvalue, faminc, hours, value_pension, cpi_inflator,
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
                wealth_level = case_when(
                  networth < 10000 ~ 1,
                  floor(log10(networth)) == 4 ~ 2,
                  floor(log10(networth)) == 5 ~ 3,
                  floor(log10(networth)) == 6 ~ 4,  
                  floor(log10(networth)) == 7 ~ 5,  
                  floor(log10(networth)) > 7 ~ 6, 
                  TRUE ~ NA
                ))

#Loop through start and end years
years_df <- data.frame(
   start_year = c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007, 2009, 2011,
                  1984, 1989, 1994, 1999, 2001),
   end_year = c(1994, 1999, 2003, 2009, 2011, 2013, 2015, 2017, 2019, 2021,
                2003, 2009, 2013, 2019, 2021),
   n_years = c(rep(10, 10), rep(20, 5))
)

for(i in 1:nrow(years_df)){
  start_yr <- years_df[i, "start_year"]
  end_yr <- years_df[i, "end_year"]
  n_years <- years_df[i, "n_years"]
  
  start_data <- full_data %>%
    filter(year == start_yr) %>%
    select(year, weight, ID1968, wealth_level, age, networth,
           hvalue, faminc, hours) %>%
    rename(start_level = wealth_level,
           start_nw = networth,
           start_age = age,
           start_hvalue = hvalue,
           start_faminc = faminc,
           start_hours = hours)
  
  end_data <- full_data %>%
    filter(year == end_yr) %>%
    select(ID1968, wealth_level, age, networth, 
           hvalue, faminc, hours) %>%
    rename(end_level = wealth_level,
           end_nw = networth,
           end_age = age,
           end_hvalue = hvalue,
           end_faminc = faminc,
           end_hours = hours)
  
  merged_level_comparison <- start_data %>%
                              left_join(end_data) %>%
                              mutate(nw_change = end_nw - start_nw,
                                     level_change = end_level - start_level,
                                     n_years = n_years)
  
  tmp_level_weight <- merged_level_comparison %>%
                      group_by(start_level) %>%
                      summarise(level_weight = sum(weight)) %>%
                      ungroup()
  
  tmp_levels_summary <- merged_level_comparison %>%
                      group_by(start_level, end_level) %>%
                      summarise(n_hhs = n(),
                                total_weight = sum(weight)) %>%
                      ungroup() %>%
                      left_join(tmp_level_weight) %>%
                      mutate(level_pct = total_weight / level_weight,
                             all_pct = total_weight / sum(total_weight),
                             start_year = start_yr,
                             end_year = end_yr,
                             n_years = n_years)
  
  tmp_change_summary <- merged_level_comparison %>%
    group_by(level_change) %>%
    summarise(n_hhs = n(),
              total_weight = sum(weight)) %>%
    ungroup() %>%
    mutate(start_year = start_yr,
           end_year = end_yr,
           n_years = n_years)
  
  if(start_yr == min(years_df$start_year) & n_years == 10){
    levels_stack <- tmp_levels_summary
    change_stack <- tmp_change_summary
    merged_level_stack <- merged_level_comparison
  } else{
    levels_stack <- levels_stack %>% bind_rows(tmp_levels_summary)
    change_stack <- change_stack %>% bind_rows(tmp_change_summary)
    merged_level_stack <- merged_level_stack %>% bind_rows(merged_level_comparison)
  }
}

#Summarize diffs
summarize_diffs <- function(years_n){
  
  lv_tmp <- levels_stack %>%
                    filter(n_years == years_n) %>%
                    group_by(start_level) %>%
                    summarise(level_weight = sum(total_weight)) %>%
                    ungroup()
  
  lv_sum <- levels_stack %>%
                        filter(n_years == years_n) %>%
                        group_by(start_level, end_level) %>%
                        summarise(total_weight = sum(total_weight)) %>%
                        ungroup() %>%
                        left_join(lv_tmp) %>%
                        mutate(level_pct = total_weight / level_weight,
                               all_pct = total_weight / sum(total_weight))
  
  change_stack_n_years <- change_stack %>%
                             filter(n_years == years_n)
  
  all_change_sum <- change_stack_n_years %>%
                          group_by(level_change) %>%
                          summarise(total_weight = sum(total_weight)) %>%
                          ungroup() %>%
                          mutate(all_pct = total_weight/sum(change_stack_n_years$total_weight))
  
  nw_change_sum <- merged_level_stack %>%
                      filter(n_years == years_n) %>%
                      group_by(start_level, end_level) %>%
                      summarise(nw_change = wtd.mean(nw_change, weights = weight)) %>%
                      ungroup()
  
  assign(paste0("levels_summary_", years_n), lv_sum, envir = .GlobalEnv)
  assign(paste0("all_change_summary_", years_n), all_change_sum, envir = .GlobalEnv)
  assign(paste0("all_nw_change_summary_", years_n), nw_change_sum, envir = .GlobalEnv)
  
  all_levels_summary <- lv_sum %>%
                          filter(start_level != 6, end_level != 6) %>%
                          select(start_level, end_level, level_pct) %>%
                          spread(key=end_level, value = level_pct)
  
  assign(paste0("all_levels_summary_", years_n), all_levels_summary, envir = .GlobalEnv)
}

summarize_diffs(10)
summarize_diffs(20)

#Ages over time
for(w in wealth_years){
  tmp_one_year <- full_data %>%
    filter(year == w, weight > 0) %>%
    select(year, weight, ID1968, wealth_level, age)
  
  if(w == min(wealth_years)){
    merged_stack <- tmp_one_year
  } else{
    merged_stack <- merged_stack %>% bind_rows(tmp_one_year)
  }
}

age_analysis <- merged_stack %>%
                  mutate(
                    l1_dummy = ifelse(wealth_level == 1, 1, 0),
                    l2_dummy = ifelse(wealth_level == 2, 1, 0),
                    l3_dummy = ifelse(wealth_level == 3, 1, 0),
                    l4_dummy = ifelse(wealth_level == 4, 1, 0)) %>%
                  group_by(year) %>%
                  summarise(avg_age = wtd.mean(age, weights = weight),
                            avg_wealth_level = wtd.mean(wealth_level, weight),
                            `L1 (<$10k)` = wtd.mean(l1_dummy, weight),
                            `L2 ($10k)` = wtd.mean(l2_dummy, weight),
                            `L3 ($100k)` = wtd.mean(l3_dummy, weight),
                            `L4 ($1M)` = wtd.mean(l4_dummy, weight),
                            ) %>%
                  ungroup()

to_plot <- age_analysis %>%
              select(-avg_age, -avg_wealth_level) %>%
              gather(-year, key=key, value=value)

file_path <- paste0(out_path, "/wealth_levels_over_time.jpeg")
source_string <- paste0("Source: PSID, University of Michigan (1984-2021)")

bw_colors <- c("#cccccc", "#969696", "#525252", "#252525")

# Create plot 
plot <- ggplot(data = to_plot, aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = bw_colors) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1985, 2020, 5)) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank()) +
  ggtitle(paste0("Wealth Level Breakdown by Year\n", min(wealth_years), "-", max(wealth_years))) +
  labs(x = "Year" , y = "Percentage of All Households",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Level comp summary
l_set <- merged_level_stack %>%
            filter(n_years == 20,
                   year == 1984)

l_summary <- l_set %>%
                group_by(start_level, end_level) %>%
                summarise(n_hh = n(),
                          start_hvalue = wtd.quantile(start_hvalue, weights = weight, probs = 0.5),
                          end_hvalue = wtd.quantile(end_hvalue, weights = weight, probs = 0.5),
                          start_faminc = wtd.quantile(start_faminc, weights = weight, probs = 0.5),
                          end_faminc = wtd.quantile(end_faminc, weights = weight, probs = 0.5),
                          start_hours = wtd.quantile(start_hours, weights = weight, probs = 0.5),
                          end_hours = wtd.quantile(end_hours, weights = weight, probs = 0.5),
                          start_wealth = wtd.quantile(start_nw, weights = weight, probs = 0.5),
                          end_wealth = wtd.quantile(end_nw, weights = weight, probs = 0.5),
                          start_age = wtd.quantile(start_age, weights = weight, probs = 0.5),
                          ) %>%
                  ungroup()


# ############################  End  ################################## #