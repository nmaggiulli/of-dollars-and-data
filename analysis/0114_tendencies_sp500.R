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

folder_name <- "0114_tendencies"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in all data
ret_yr <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  rename(index = price_plus_div) %>%
                  mutate(yr = year(date),
                         mt = month(date)) %>%
                  filter(mt == 1) %>%
                  mutate(ret = index/lag(index) - 1) %>%
                  select(date, ret, yr, index) %>%
                  filter(!is.na(ret), yr > 1940)

avg_ret <- ret_yr %>%
            summarise(mean_ret = mean(ret)) %>%
            pull()

print(avg_ret)

file_path <- paste0(out_path, "/sp500_returns_by_yr.jpeg")
source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)" 
note_string <- paste0("Note:  Real return includes reinvested dividends.") 
            
plot <- ggplot(ret_yr, aes(x=date, y=ret)) +
          geom_bar(stat="identity", fill = chart_standard_color) +
          geom_hline(yintercept = avg_ret, linetype = "dashed") +
          geom_hline(yintercept = 0, col = "black") +
          scale_y_continuous(label = percent) +
          of_dollars_and_data_theme +
          ggtitle("Market Returns Are Rarely Average") +
          labs(x="Date", y="Annual Return",
               caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #