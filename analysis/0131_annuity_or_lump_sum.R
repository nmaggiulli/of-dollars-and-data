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
library(FinCal)
library(tidyverse)

folder_name <- "0131_annuity_or_lump_sum"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load in historical stock and bond data to calculate historical 60/40 portfolio return
hist_bond_stock <- readRDS(paste0(localdir, "0021_historical_returns_sp500_bond_damodaran.Rds")) %>%
                    mutate(ret_60_40 = 0.6*ret_sp500 + 0.4*ret_10yr_bond)

# Find average 60/40 return
mean(hist_bond_stock$ret_60_40)

# Bring in SSA life table
ssa_life <- read_excel(paste0(importdir, "/0131_lump_sum_annuity/2016_ssa_life_table.xlsx"))

# Calculate her survival probability with a function
calculate_survival_p <- function(start_age, end_age){
  p <- ssa_life %>%
    filter(age == start_age | age == end_age) %>%
    mutate(p_survive = female_survivors/lag(female_survivors)) %>%
    filter(!is.na(p_survive)) %>%
    pull(p_survive)
  return(p)
}

# Survivorship probability
1-calculate_survival_p(45, 62)
calculate_survival_p(45, 87)
calculate_survival_p(85, 87)

# Write a function
lump_sum_v_annuity <- function(client_staring_age,
                               n_years_work, 
                               n_years_retire, 
                               pmt_annuty, 
                               lump_sum,
                               ret_work,
                               ret_retire){
  
  # Calculate value of annuity at time of retirement
  pv_annuity <- pv(r = ret_retire,
                   n = n_years_retire,
                   fv = 0,
                   pmt = -pmt_annuity,
                   type = 1)
  
  fv_lump_sum <- fv(r = ret_work, n = n_years_work, pv = lump_sum, pmt = 0, type = 1)
  
  ret_eq <- discount.rate(n = n_years_work, 
                          pv = lump_sum,
                          fv = pv_annuity,
                          pmt = 0,
                          type=0
                          )
  
  df <- data.frame(age = seq(client_starting_age, client_starting_age+n_years_work+n_years_retire, 1))
  
  for(i in 1:nrow(df)){
    age <- df[i, "age"]
    
    if(age == client_starting_age){
      df[i, "value_lump_sum"] <- -lump_sum
      df[i, "value_annuity"] <- 0
    } else if (age == client_starting_age + n_years_work){
      df[i, "value_lump_sum"] <- df[(i-1), "value_lump_sum"] * (1 + ret_work) 
      df[i, "value_annuity"]  <- pv_annuity
    } else if (age > client_starting_age + n_years_work){
      df[i, "value_lump_sum"] <- (df[(i-1), "value_lump_sum"] - pmt_annuity) * (1 + ret_retire) 
      df[i, "value_annuity"] <- (df[(i-1), "value_annuity"] - pmt_annuity) * (1 + ret_retire) 
    } else{
      df[i, "value_lump_sum"] <- df[(i-1), "value_lump_sum"] * (1 + ret_work)
      df[i, "value_annuity"] <- 0
    }
    
    if(df[i, "value_annuity"] < 0){
      df[i, "value_annuity"] <- 0
    }
    if(df[i, "value_lump_sum"] < 0){
      df[i, "value_lump_sum"] <- 0
    }
  }
  
  to_plot <- df %>%
              gather(-age, key=key, value=value) 
  
  
  # Reset file path
  file_path <- paste0(out_path, "/ls_vs_annuity_", n_years_retire,"_years.jpeg")
  
  # Set source/note
  source_string <- paste0("Source:  SSA, Math (OfDollarsAndData.com)")
  note_string   <- str_wrap(paste0("Note:  Assumes a ", 
                                   100*ret_retire,
                                   "% nominal return during working years and retirement."), 
                            width = 85)
  
  text_labels <- data.frame(age = c(55, 65))
  
  text_labels[1, "key"] <- "value_lump_sum"
  text_labels[1, "value"] <- filter(to_plot, age == text_labels[1, "age"],
                                    key == text_labels[1, "key"]) %>% pull(value)
  text_labels[1, "label"] <- "Lump Sum"
  text_labels[2, "key"] <- "value_annuity"
  text_labels[2, "value"] <- filter(to_plot, age == text_labels[2, "age"],
                                    key == text_labels[2, "key"]) %>% pull(value)
  text_labels[2, "label"] <- "Annuity"
  
  if(df[nrow(df), "value_lump_sum"] > df[nrow(df), "value_annuity"]){
    title <- "The Lump Sum is Worth More Than\nthe Annuity at Retirement"
  } else{
    title <- "With a Longer Lifespan, the Annuity is\nWorth More Than the Lump Sum"
  }
  
  plot <- ggplot(to_plot, aes(x=age, y=value, fill = key)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_vline(xintercept = client_starting_age + n_years_work+0.5, linetype = "dashed") +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(label = dollar) +
    geom_text_repel(data=text_labels, aes(x=age, y=value, col = key),
                    label = text_labels$label,
                    size = 3.5,
                    family = "my_font",
                    max.iter = 1,
                    segment.colour = "transparent",
                    nudge_x = ifelse(text_labels$label == "Lump Sum", -5, 7)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(title)) +
    labs(x="Age", y="Present Value",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  
  print(paste0("Future Value of Lump Sum is: $", formatC(fv_lump_sum, digits = 0, big.mark = ",", format = "f")))
  print(paste0("Present Value of Annuity is: $", formatC(-pv_annuity, digits = 0, big.mark = ",", format = "f")))
}

# Define working and retirement parameters
n_years_work <- 17
client_starting_age <- 45
lump_sum <- -138000
pmt_annuity <- 1800*12
ret_work <- 0.06
ret_retire <- ret_work

# Run the simulations but play with the retiremnt years
lump_sum_v_annuity(client_staring_age,
                   n_years_work, 
                   23, 
                   pmt_annuty, 
                   lump_sum,
                   ret_work,
                   ret_retire)

lump_sum_v_annuity(client_staring_age,
                   n_years_work, 
                   25, 
                   pmt_annuty, 
                   lump_sum,
                   ret_work,
                   ret_retire)


# ############################  End  ################################## #