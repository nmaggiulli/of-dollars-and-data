cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "xxxx_expectation_vs_reality"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read_excel(paste0(importdir, "0097_spx_daily/spx_daily.xlsx")) %>%
  rename(date = Period,
         reality = `S&P 500 Level`) %>%
  filter(date >= "1978-01-01", date < "2018-01-01") %>%
  mutate(year = year(date),
         date = as.Date(date),
         ret = reality/lag(reality) - 1) %>%
  select(date, reality, ret) %>%
  filter(!is.na(ret))

first_index <- pull(spx[1, "reality"])
total_return <- as.numeric((spx[nrow(spx), "reality"]/first_index)^(1/nrow(spx)) - 1) - 0.00005

for (i in 1:nrow(spx)){
  if(i == 1){
    spx[i, "expectation"] <- 1
  } else{
    spx[i, "expectation"] <- spx[(i-1), "expectation"] * (1+total_return)
  }
}

to_plot <- spx %>%
            mutate(reality = reality/first_index) %>%
            select(-ret) %>%
            gather(-date, key=key, value=value)

ggplot(to_plot, aes(x=date, y=value, col=key)) +
  geom_line()

# ############################  End  ################################## #
