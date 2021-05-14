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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0243_fed_wealth_dist"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Generation facts
# Boomers turned 35 in 1990 (on average)
# Gen X turned 35 in 2008 (on average)
# Millennials will turn 35 in 2023 (on average)

cpi <- readRDS(paste0(localdir, "0021_FRED_cpi.Rds")) %>%
        filter(year >= 1989) %>%
        arrange(-year)

for(i in 1:nrow(cpi)){
  if(i == 1){
    cpi[i, "inflator"] <- 1
  } else{
    cpi[i, "inflator"] <- cpi[(i-1), "inflator"]*(1+cpi[i, "rate_cpi"])
  }
}

cpi <- cpi %>%
        arrange(year) %>%
        select(year, inflator)

fed_wealth_data <- read.csv(paste0(importdir, "/0243_fed_distributional_financial_accounts/fed_dfa_data/dfa-generation-levels.csv")) %>%
                      clean_cols() %>%
                      filter(grepl("Q4", date)) %>%
                      mutate(year = as.numeric(substr(date, 1, 4)),
                             re = real_estate) %>%
                      select(year, category, net_worth, assets, re, liabilities)

df <- fed_wealth_data %>%
        inner_join(cpi) %>%
        mutate(real_nw = net_worth*inflator,
               real_assets = assets*inflator,
               real_re = re*inflator,
               real_liabilities = liabilities*inflator) %>%
      select(year, category, contains("real_")) %>%
      arrange(category, year) %>%
      mutate(growth_real_nw = case_when(
        category == lag(category, 1) ~ real_nw/lag(real_nw, 1) - 1,
        TRUE ~ NaN
      ))

to_plot <- df %>%
            select(year, category, growth_real_nw)


file_path <- paste0(out_path, "/growth_rates_of_nw.jpeg")
source_string <- "Source:  FRED, FED (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=year, y= growth_real_nw, col = category)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Annual Real Growth Rate in Wealth")) +
  labs(x="Year", y="Real Growth Rate",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #