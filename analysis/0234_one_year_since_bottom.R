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

folder_name <- "0234_one_year_since_bottom"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "0233_dow_rolling_90_day_watermark/Dow daily 2020.xlsx"), 
                  col_names = c("date", "index_dow")) %>%
                mutate(ret_1yr = index_dow/lag(index_dow, 250) - 1) %>%
                drop_na 

to_plot <- raw

file_path <- paste0(out_path, "/dow_1yr_ret_dist.jpeg")
source_string <- "Source:  Bloomberg (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x=ret_1yr)) +
  geom_density(fill = chart_standard_color) +
  scale_x_continuous(label = percent_format(accuracy = 2)) +
  of_dollars_and_data_theme +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("Dow Jones Industrial Average\n1-Year Return Distribution Since 1915")) +
  labs(x="1-Year Return", y="Frequency",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #