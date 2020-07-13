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

folder_name <- "0194_what_risk_isnt"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/0194_what_risk_isnt/thorpe_returns.xlsx")) 

to_plot <- raw %>%
            gather(-year, key=key, value=value) %>%
            mutate(value = value/100)

file_path <- paste0(out_path, "/thorpe_returns_by_year.jpeg")
source_string <- paste0("Source:  A Man for All Markets Appendix D (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Thorpe's (Princeton Newport Partners) returns are gross of fees."))

plot <- ggplot(to_plot, aes(x=year, y=value, fill = key)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "green")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Thorpe's Returns vs. 3m TBills")) +
  labs(x="Year", y="Annual Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #