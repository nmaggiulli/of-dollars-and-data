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

folder_name <- "xxxx_sp500_cape"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_month_ret <- 120

#Bring in all data
ret_yr <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                  rename(index = price_plus_div) %>%
                  mutate(fwd_ret = (lead(index, n_month_ret)/index)^(12/n_month_ret) - 1) %>%
                  select(date, fwd_ret, cape) %>%
                  filter(!is.na(fwd_ret), !is.na(cape), 
                         date >= "1980-01-01",
                         date < "1995-01-01") 

to_plot <- ret_yr

# Do stock beats by year
file_path <- paste0(out_path, "/cape_forward_ret.jpeg")

source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
note_string <- paste0("Note:  Real return includes reinvested dividends.")

plot <- ggplot(data = to_plot, aes(x=cape, y = fwd_ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("CAPE vs. ", n_month_ret, "-Month Annualized Forward Return")) +
  labs(x = paste0("CAPE"), y = paste0(n_month_ret, "-Month Annualized Forward Return (%)"),
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #