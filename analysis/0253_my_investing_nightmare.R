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
library(tidyverse)

folder_name <- "0253_my_investing_nightmare"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #


raw_1970 <- read.csv(paste0(importdir, "/0249_world_returns/dfa_msci_1970.csv"), skip = 6,
                col.names = c("date", "US", "Spain",
                              "UK", "Canada", "Japan", "Italy"),
                ) %>%
        filter(row_number() != 1, `US` != "") %>%
        mutate(date = as.Date(date, "%m/%d/%y"))

df_1970 <- sapply(raw_1970[2:ncol(raw_1970)], as.numeric) %>%
                as.data.frame() %>%
                bind_cols(date = raw_1970[, "date"]) %>%
            select(date, everything())

raw_1999 <- read.csv(paste0(importdir, "/0249_world_returns/dfa_msci_1999.csv"), skip = 6,
                     col.names = c("date", "China", "Greece", "Russia"),
) %>%
  filter(row_number() != 1, `China` != "") %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

df_1999 <- sapply(raw_1999[2:ncol(raw_1999)], as.numeric) %>%
  as.data.frame() %>%
  bind_cols(date = raw_1999[, "date"]) %>%
  select(date, everything())

df <- df_1970 %>%
        left_join(df_1999)

df_long <- df %>%
            gather(-date, key=key, value=value) %>%
            mutate(start_date = date,
                   end_date = case_when(key == lead(key, 119) ~  lead(date, 119),
                                        TRUE ~ NaN),
              ret_10yr = case_when(
              key == lead(key, 119) ~ lead(value, 119)/value - 1,
              TRUE ~ NaN
            )) %>%
          select(start_date, end_date, key, ret_10yr) %>%
          drop_na

plot_country <- function(country, start_date, end_date){
  to_plot <- df %>%
              rename_(.dots = setNames(paste0(country), "index")) %>%
              filter(date >= start_date, date <= end_date)
  
  first_value <- to_plot[1, "index"]
  
  to_plot <- to_plot %>%
              mutate(value = index/first_value - 1)
  
  file_path <- paste0(out_path, "/", country, "_", date_to_string(start_date), "_", date_to_string(end_date),"_pct_change.jpeg")
  source_string <- "Source: Returns 2.0 (OfDollarsAndData.com)"
  
  plot <- ggplot(to_plot, aes(x= date, y=value)) +
    geom_line() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0(country, " Stock Performance")) +
    labs(x="Year", y="Percentage Change",
         caption = paste0(source_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_country("Spain", "1973-09-30", "1983-08-31")
plot_country("Greece", "2007-12-31", "2017-11-30")
plot_country("Italy", "2006-02-28", "2016-01-31")
plot_country("US", "1999-03-31", "2009-02-28")

# ############################  End  ################################## #