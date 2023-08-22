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
library(tidyverse)

folder_name <- "0296_how_to_not_panic"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0296_ycharts_spx/SPX_data.csv")) %>%
  mutate(date = as.Date(Period),
         index_sp500 = `S.P.500...SPX..Level`) %>%
  arrange(date) 

start_dates <- c("2015-07-20", "2015-12-01", "2018-10-03", "2020-02-19", "2022-01-03")
end_dates <- c("2015-11-03", "2016-04-19", "2019-04-23", "2020-08-12", "2023-08-22")
crash_years <- c(2015, 2016, 2018, 2020, 2022)

for(s in 1:length(start_dates)){
  
  start_dt <- as.Date(start_dates[s])
  end_dt <- as.Date(end_dates[s])
  crash <- crash_years[s]
  
  tmp <- raw %>%
            filter(date >= start_dt,
                   date <= end_dt)
  
  first_value <- tmp %>%
                  head(1) %>% pull(index_sp500)
  
  tmp2 <- tmp %>%
            mutate(pct_change = index_sp500/first_value - 1,
                   crash = crash,
                   day = row_number()) %>%
            select(day, pct_change, crash)
  
  if(s == 1){
    to_plot <- tmp2
  } else{
    to_plot <- to_plot %>% bind_rows(tmp2)
  }
}

file_path <- paste0(out_path, "/corrections_since_2012.jpeg")
source_string <- paste0("Source: YCharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Figures do not include dividends and are not adjusted for inflation."),
                        width = 85)

plot <- ggplot(data = to_plot, aes(x=day, y=pct_change, col = as.factor(crash))) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 Corrections\nSince 2012")) +
  labs(x = "Day", y = "Percentage Decline",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

  
  




# ############################  End  ################################## #