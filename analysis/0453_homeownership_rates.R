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
library(tidyverse)

folder_name <- "0450_homeownership_rates"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw_fred <- read_excel(paste0(importdir, "/xxxx_homeownership/fred_RHORUSQ156N.xlsx"),
                       sheet = "Quarterly")

raw_census <- read_excel(paste0(importdir, "/xxxx_homeownership/census_homeownership_rates_historical.xlsx"),
                       sheet = "data") %>%
                mutate(observation_date = as.Date(paste0(year, "-01-01"))) %>%
                select(observation_date, ownership_rate)

df <- raw_fred %>%
        filter(month(observation_date) == 1,
               year(observation_date) > 2000) %>%
        mutate(ownership_rate = RHORUSQ156N/100) %>%
        select(observation_date, ownership_rate) %>%
        bind_rows(raw_census) %>%
        arrange(observation_date)
        
to_plot <- df

min_year <- year(min(df$observation_date))
max_year <- year(max(df$observation_date))

## Add plot ##
file_path <- paste0(out_path, "/us_homeownership_rate_over_time.jpeg")
source_string <- paste0("Source: Census Bureau, FRED (OfDollarsAndData.com)")

plot <- ggplot(to_plot, aes(x= observation_date, y = ownership_rate)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("U.S. Homeownership Rate\n", min_year, "-", max_year)) +
  labs(x="Year", y="Homeownership Rate (%)",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #