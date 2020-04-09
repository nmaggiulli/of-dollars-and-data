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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0178_death_of_a_value_investor"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0178_ycharts_value_v_growth/RLGTR_RLVTR_annual_ret.csv"),
                col.names = c("date", "ret_growth", "ret_value")) %>%
          mutate(year = year(as.Date(date)),
                 ret_growth = as.numeric(ret_growth)/100,
                 ret_value = as.numeric(ret_value)/100) %>%
          select(year, contains("ret_"))

# Plot all rise dates
file_path <- paste0(out_path, "/value_v_growth_by_year.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note:  Return data includes dividends, but not adjusted for inflation."),
                  width = 85)

to_plot <- raw %>%
            mutate(net_ret = ret_value - ret_growth)

text_labels <- data.frame()

text_labels[1, "year"] <- 2010
text_labels[1, "net_ret"] <- 0.27
text_labels[1, "label"] <- "Value Outperforms"

text_labels[2, "year"] <- text_labels[1, "year"]
text_labels[2, "net_ret"] <- -1*text_labels[1, "net_ret"]
text_labels[2, "label"] <- "Growth Outperforms"

plot <- ggplot(to_plot, aes(x=year, y=net_ret)) + 
  geom_bar(stat = "identity", fill = chart_standard_color) +
  geom_text(data=text_labels, aes(x=year, y=net_ret, label=label), 
            size = 3.5, 
            family = "my_font", 
            col = "black") +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Value Minus Growth Annual Return")) +
  labs(x = "Year" , y = "Net Return",
       caption = paste0("\n", source_string, "\n", note_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #