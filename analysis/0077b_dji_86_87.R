cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(stats)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

dj <- read.csv(paste0(importdir, "77b-dji-86-87/dji_daily_data_86_87.csv"))

colnames(dj) <- c("date", "index")

to_plot <- dj %>%
            mutate(date = as.Date(date, format = "%Y-%m-%d"))

file_path <- paste0(exportdir, "77b-dji-86-87/dji-86-87-crash.jpeg")

# Add a source and note string for the plots
source_string <- str_wrap(paste0("Source: YCharts.com (OfDollarsAndData.com)"),
                          width = 85)

note_string <- str_wrap(paste0("Note: Shows the Dow Jones Industrial Average through Black Monday (10/19/87)."),
                        width = 85)

plot <- ggplot(to_plot, aes(x= date, y = index)) +
  geom_line()  +
  scale_y_continuous() +
  scale_x_date(date_labels = "%m/%d/%y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("The Dow Jones Erased 18 Months of Gains\nIn a Matter of Hours")) +
  labs(x = "Date", y = "DJIA Index",
       caption = paste0("\n", source_string, "\n", note_string))

# Save plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #