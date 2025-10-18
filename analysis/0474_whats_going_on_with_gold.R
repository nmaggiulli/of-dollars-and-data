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
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(quantmod)
library(FinCal)
library(tidyverse)

folder_name <- "0474_whats_going_on_with_gold"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

yesterday <- Sys.Date() - 1 

raw_spxtr <- read.csv(paste0(importdir, "/", folder_name, "/SPXTR_data_2025_10_18.csv")) %>%
              mutate(day = as.Date(Period),
                     index_spxtr = `S.P.500.Total.Return...SPXTR..Level`,
                     date = as.Date(paste0(year(day), "-", month(day), "-01"))) %>%
              select(date, index_spxtr)

raw_gld <- read.csv(paste0(importdir, "/", folder_name, "/IGPUSPPC8_data_2025_10_18.csv")) %>%
  mutate(date = as.Date(Period) + days(1),
         index_gld = `Gold.Price.in.US.Dollars..I.GPUSPPC8.`) %>%
  select(date, index_gld)

getSymbols("GC%3DF", from = as.Date("2025-08-31"), to = yesterday, 
           src="yahoo", periodicity = "daily") 

gld_latest <- data.frame(day=index(get("GC%3DF")), coredata(get("GC%3DF"))) %>%
  rename(index_gld = `GC.3DF.Adjusted`) %>%
  mutate(date = as.Date(paste0(year(day), "-", month(day), "-01"))) %>%
  group_by(date) %>%
  summarise(index_gld_monthly = mean(index_gld)) %>%
  ungroup() %>%
  select(date, index_gld_monthly)
          
#Do monthly
monthly_spxtr <- raw_spxtr %>%
                    group_by(date) %>%
                    summarise(index_spxtr = mean(index_spxtr)) %>%
                    ungroup() %>%
                    select(date, index_spxtr)

df <- monthly_spxtr %>%
          left_join(raw_gld) %>%
          left_join(gld_latest) %>%
          mutate(index_gld = case_when(
            is.na(index_gld) ~ index_gld_monthly,
            TRUE ~ index_gld
          )) %>%
        select(-index_gld_monthly) %>%
        arrange(date)

plot_by_date <- function(start_date){
  tmp <- df %>%
              filter(date >= start_date)
  
  first_gld <- pull(tmp[1, "index_gld"])
  first_spxtr <- pull(tmp[1, "index_spxtr"])
  
  tmp_long <- tmp %>%
                mutate(`Gold` = index_gld/first_gld,
                       `S&P 500` = index_spxtr/first_spxtr) %>%
              select(date, `Gold`, `S&P 500`)
  
  to_plot <- tmp_long %>%
              gather(-date, key=key, value=value)
  
  start_year <- year(start_date)
  end_year <- max(year(to_plot$date))
  
  file_path <- paste0(out_path, "/spxtr_v_gld_", start_year, "_", end_year, ".jpeg")
  source_string <- "Source:  YCharts, YahooFinance (OfDollarsAndData.com)"
  note_string <- str_wrap(paste0("Note: Uses average monthly prices. S&P 500 return includes dividends but is not adjusted for inflation."),
                          width = 85)
  
  text_labels <- data.frame()
  x_nudge <- 24
  y_nudge <- 0.9
  
  text_labels[1, "date"] <- tmp_long[nrow(tmp_long) - x_nudge, "date"]
  text_labels[1, "value"] <- tmp_long[nrow(tmp_long), "Gold"]*y_nudge
  text_labels[1, "key"] <- "Gold"
  text_labels[1, "label"] <- paste0("Gold\n", format_as_dollar(text_labels[1, "value"], 2))
  
  text_labels[2, "date"] <- tmp_long[nrow(tmp_long) - x_nudge, "date"]
  text_labels[2, "value"] <- tmp_long[nrow(tmp_long), "S&P 500"]*y_nudge
  text_labels[2, "key"] <- "S&P 500"
  text_labels[2, "label"] <- paste0("S&P 500\n", format_as_dollar(text_labels[2, "value"], 2))
  
  plot <- ggplot(to_plot, aes(x=date, y = value, col = key)) +
    geom_line() +
    geom_text(data = text_labels, aes(x=date, y=value, label = label, col = key),
              family = "my_font") +
    scale_color_manual(values = c("gold", "black"), guide = "none") +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Growth of $1\nGold vs. S&P 500\n", start_year, "-", end_year)) +
    labs(x="Year", y=paste0("Growth of $1"),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_by_date("1990-01-01")
plot_by_date("2000-01-01")
plot_by_date("2012-01-01")

# ############################  End  ################################## #