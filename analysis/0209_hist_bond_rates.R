cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "0209_hist_bond_rates"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bond_ret <- read.csv(paste0(importdir, "/0209_bond_rets/treasury_5yr.csv"), skip = 7,
                     col.names = c("date", "index_bond")) %>%
            drop_na() %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
            mutate(date = as.Date(paste0(year(date), "-", month(date), "-01")))

shiller <- readRDS(paste0(localdir, "0009_sp500_ret_pe.RDS")) %>%
            select(date, long_irate)

n_months_ret <- 120

df <- bond_ret %>%
        left_join(shiller) %>%
        mutate(ret = (lead(index_bond, n_months_ret)/index_bond)^(1/(n_months_ret/12)) - 1)

file_path <- paste0(out_path, "/yield_v_return.jpeg")
source_string <- paste0("Source:  Returns 2.0, Shiller data (OfDollarsAndData.com)")

to_plot <- df 

# Plot the results
plot <- ggplot(to_plot, aes(x = long_irate, y = ret)) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Starting Yield vs. Future 10-Year Return")) +
  labs(x = "Starting Yield" , y = "Forward 10-Year Return",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #