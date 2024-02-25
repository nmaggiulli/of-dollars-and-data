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

folder_name <- "0390_pe_vc_rets"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/0390_cambridge_vc_pe_rets/2002_2023_pe_vc_rets.xlsx")) %>%
          arrange(type, year)

df <- raw

for(i in 1:nrow(df)){
  ret <- df[i, "ret"]
  
  if(i == 1){
    
    df[i, "growth"] <- 1*(1+ret)
    if(ret > 0){
      df[i, "growth_fees"] <- 1*(1+ret - 0.02)-(0.2*ret*1)
    } else{
      df[i, "growth_fees"] <- 1*(1+ret - 0.02)
    }
    
  } else{
    type <- df[i, "type"]
    prior_type <- df[(i-1), "type"]
    
    if(type == prior_type){
      df[i, "growth"] <- df[(i-1), "growth"]*(1+ret)
      
      if(ret > 0){
        df[i, "growth_fees"] <- df[(i-1), "growth_fees"]*(1+ret - 0.02)-(0.2*ret*df[(i-1), "growth_fees"])
      } else{
        df[i, "growth_fees"] <- df[(i-1), "growth_fees"]*(1+ret - 0.02)
      }
    } else{
      df[i, "growth"] <- 1*(1+ret)
      
      if(ret > 0){
        df[i, "growth_fees"] <- 1*(1+ret - 0.02)-(0.2*1)
      } else{
        df[i, "growth_fees"] <- 1*(1+ret - 0.02)
      }
    }
  }
  if(df[i, "type"] == "sp500"){
    df[i, "growth_fees"] <- df[i, "growth"]
  }
}

df <- data.frame(year = rep(2001, 3),
                 type = c("pe", "vc", "sp500"),
                 ret = rep(0, 3),
                 growth = rep(1, 3),
                 growth_fees = rep(1, 3)) %>%
        bind_rows(df) %>%
        mutate(type = case_when(
          type == "pe" ~ "Private Equity",
          type == "sp500" ~ "S&P 500",
          TRUE ~ "Venture Capital"
        ))

to_plot <- df

min_year <- min(to_plot$year)
max_year <- max(to_plot$year)

file_path <- paste0(out_path, "/pe_vc_sp500_growth_of_dollar_2002_2023.jpeg")
source_string <- paste0("Source:  Cambridge Associates, Macrotrends.net (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures shown are not adjusted for inflation or fees."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "year"] <- 2018
text_labels[2, "year"] <- 2021
text_labels[3, "year"] <- 2021

text_labels[1, "growth"] <- 16
text_labels[2, "growth"] <- 1.5
text_labels[3, "growth"] <- 7

text_labels[1, "type"] <- "Private Equity"
text_labels[2, "type"] <- "S&P 500"
text_labels[3, "type"] <- "Venture Capital"

text_labels[1, "label"] <- text_labels[1, "type"]
text_labels[2, "label"] <- text_labels[2, "type"]
text_labels[3, "label"] <- text_labels[3, "type"]

# Create the plot object
plot <- ggplot(to_plot, aes(x = year, y = growth, col = type)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=year, y=growth, col = type, label = label),
            family = my_font) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("#1f78b4", "black", "#b2df8a"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1 (Gross of Fees)\n", 
                 min_year+1, 
                 " - ",
                max_year)) +
  labs(x = "Year", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do net of fees
file_path <- paste0(out_path, "/pe_vc_sp500_growth_fees_2002_2023.jpeg")
source_string <- paste0("Source:  Cambridge Associates, Macrotrends.net (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures shown assume a 2% management fee and 20% performance fee (for VC/PE returns only)."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "year"] <- 2018
text_labels[2, "year"] <- 2021
text_labels[3, "year"] <- 2021

text_labels[1, "growth_fees"] <- 6
text_labels[2, "growth_fees"] <- 2.75
text_labels[3, "growth_fees"] <- 0.75

text_labels[1, "type"] <- "Private Equity"
text_labels[2, "type"] <- "S&P 500"
text_labels[3, "type"] <- "Venture Capital"

text_labels[1, "label"] <- text_labels[1, "type"]
text_labels[2, "label"] <- text_labels[2, "type"]
text_labels[3, "label"] <- text_labels[3, "type"]

# Create the plot object
plot <- ggplot(to_plot, aes(x = year, y = growth_fees, col = type)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=year, y=growth_fees, col = type, label = label),
            family = my_font) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("#1f78b4", "black", "#b2df8a"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1 (Net of Fees)\n", 
                 min_year+1, 
                 " - ",
                 max_year)) +
  labs(x = "Year", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


#Now do blended
to_plot <- df %>%
            mutate(type = case_when(
              type == "S&P 500" ~ "S&P 500",
              TRUE ~ "PE/VC (50/50)"
              )) %>%
            group_by(year, type) %>%
            summarise(growth_fees = mean(growth_fees)) %>%
            ungroup()

file_path <- paste0(out_path, "/pe_vc_blend_sp500_growth_fees_2002_2023.jpeg")
source_string <- paste0("Source:  Cambridge Associates, Macrotrends.net (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures shown assume a 2% management fee and 20% performance fee (for VC/PE returns only)."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "year"] <- 2018
text_labels[2, "year"] <- 2021

text_labels[1, "growth_fees"] <- 4.5
text_labels[2, "growth_fees"] <- 2.25

text_labels[1, "type"] <- "PE/VC (50/50)"
text_labels[2, "type"] <- "S&P 500"

text_labels[1, "label"] <- text_labels[1, "type"]
text_labels[2, "label"] <- text_labels[2, "type"]

# Create the plot object
plot <- ggplot(to_plot, aes(x = year, y = growth_fees, col = type)) +
  geom_line() +
  geom_text(data = text_labels, aes(x=year, y=growth_fees, col = type, label = label),
            family = my_font) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("#1f78b4", "black"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $1 (Net of Fees)\n", 
                 min_year+1, 
                 " - ",
                 max_year)) +
  labs(x = "Year", y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #