cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)
library(tidyverse)

folder_name <- "0330_rate_changes_v_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dgs10 <- read_excel(paste0(importdir, "/", folder_name, "/DGS10.xls"),
                    skip = 10) %>%
            mutate(date = as.Date(observation_date),
                   rate_10yr = DGS10) %>%
            select(date, rate_10yr)

spx <- read.csv(paste0(importdir, "/", folder_name, "/SPX_data.csv")) %>%
  clean_cols() %>%
  mutate(date = as.Date(period),
         index_sp500 = s_p_500___spx__level) %>%
  select(date, index_sp500) %>%
  arrange(date)

df <- dgs10 %>%
        inner_join(spx) %>%
        mutate(rate_change = (rate_10yr  - lag(rate_10yr, 250))/100,
               spx_change = lead(index_sp500, 250)/index_sp500 - 1,
               dummy_1980 = ifelse(date >= as.Date("1980-01-01"), 1, 0)
               )

to_plot <- df %>%
            filter(dummy_1980 == 0,
                   month(date) == 1,
                   day(date) < 5)

file_path <- paste0(out_path, "/rate_change_vs_spx_12m_pre_1980.jpg")
source_string <- paste0("Source: YCharts, FRED (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Shows nominal change in SPX price. "),
                               width = 80)

plot <- ggplot(data = to_plot, aes(x=rate_change, y=spx_change)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Change in S&P 500 Over Next Year vs.\nChange in 10-Year Treasury Rates Over Prior Year")) +
  labs(x = "Change in Rates (Over Prior Year)", y = "Change in S&P 500 (Over Next Year)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #