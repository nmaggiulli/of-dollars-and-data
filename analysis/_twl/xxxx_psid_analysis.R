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

folder_name <- "_twl/xxxx_psid_analysis"

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
        filter(name %in% c("wealth", "hvalue"),
               year %in% wealth_years)

i <- fread(file.path(r,"psid-lists","indvars.txt")) %>%
        filter(name %in% c("age", "weight"),
               year %in% wealth_years)

# Reformat the shape
i <- dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f <- dcast(f[,list(year,name,variable)],year~name, value.var = "variable")

#Add 2019 and 2021 variables
f <- f %>% bind_rows(data.frame(year = c(2019, 2021),
                     wealth = c ("ER77511", "ER81838")))

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
            rename()

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

cpi_deflator <- cpi_all_years %>%
                  mutate(cpi_deflator = cpi/cpi_all_years[1, "cpi"]) %>%
                  select(year, cpi_deflator)
                        
# Join all data sources and deflate by CPI
full_psid_supp <- full_psid %>%
                    left_join(supp_stack) %>%
                    left_join(cpi_deflator) %>%
                    mutate(networth = case_when(
                      year < 2009 ~ supp_wealth/cpi_deflator,
                      TRUE ~ wealth/cpi_deflator
                    ),
                    wealth_level = case_when(
                      networth < 10000 ~ 1,
                      floor(log10(networth)) == 4 ~ 2,
                      floor(log10(networth)) == 5 ~ 3,
                      floor(log10(networth)) == 6 ~ 4,  
                      floor(log10(networth)) == 7 ~ 5,  
                      floor(log10(networth)) > 7 ~ 6, 
                      TRUE ~ NA
                    )) %>%
                    select(year, interview, weight, pernum, networth, wealth_level,
                           ID1968, age)

missing_nw_id_to_remove <- full_psid_supp %>%
  filter(is.na(networth)) %>%
  pull(ID1968)

# Now just keep one observation per HH
full_data <- full_psid_supp %>%
            filter(ID1968 != missing_nw_id_to_remove) %>%
            arrange(ID1968, year, pernum) %>%
            group_by(year, ID1968) %>%
            slice(1) %>%
            ungroup()

#Loop through start and end years
years_df <- data.frame(
   start_year = c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007, 2009, 2011),
   end_year = c(1994, 1999, 2003, 2009, 2011, 2013, 2015, 2017, 2019, 2021),
   n_years = c(rep(10, 10))
)

for(i in 1:nrow(years_df)){
  start_yr <- years_df[i, "start_year"]
  end_yr <- years_df[i, "end_year"]
  n_years <- years_df[i, "n_years"]
  
  start_data <- full_data %>%
    filter(year == start_yr, weight > 0) %>%
    select(year, weight, ID1968, wealth_level, age) %>%
    rename(start_level = wealth_level)
  
  end_data <- full_data %>%
    filter(year == end_yr, weight > 0) %>%
    select(ID1968, wealth_level, age) %>%
    rename(end_level = wealth_level,
           end_age = age)
  
  merged_level_comparison <- start_data %>%
                              left_join(end_data)
  
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
    mutate(level_change = end_level - start_level) %>%
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
  } else{
    levels_stack <- levels_stack %>% bind_rows(tmp_levels_summary)
    change_stack <- change_stack %>% bind_rows(tmp_change_summary)
  }
}

#Summarize diffs
summarize_diffs <- function(years_n){
  
lv_tmp <- levels_stack %>%
                  filter(n_years == years_n) %>%
                  group_by(start_level) %>%
                  summarise(level_weight = sum(total_weight)) %>%
                  ungroup()

all_lv_sum <- levels_stack %>%
                      filter(n_years == years_n) %>%
                      group_by(start_level, end_level) %>%
                      summarise(total_weight = sum(total_weight)) %>%
                      ungroup() %>%
                      left_join(lv_tmp) %>%
                      mutate(level_pct = total_weight / level_weight,
                             all_pct = total_weight / sum(total_weight))

all_change_sum <- change_stack %>%
                        filter(n_years == years_n) %>%
                        group_by(level_change) %>%
                        summarise(total_weight = sum(total_weight)) %>%
                        ungroup() %>%
                        mutate(all_pct = total_weight/sum(change_stack$total_weight))

assign(paste0("all_levels_summary_", years_n), all_lv_sum, envir = .GlobalEnv)
assign(paste0("all_change_summary_", years_n), all_change_sum, envir = .GlobalEnv)

}

summarize_diffs(10)

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
                            `L4 ($1M)` = wtd.mean(l4_dummy, weight)
                            ) %>%
                  ungroup()

to_plot <- age_analysis %>%
              select(-avg_age, -avg_wealth_level) %>%
              gather(-year, key=key, value=value)

file_path <- paste0(out_path, "/wealth_levels_over_time.jpeg")
source_string <- paste0("Source: PSID, University of Michigan (1984-2017)")

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


# ############################  End  ################################## #