cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(tidyverse)

folder_name <- "0411_coast_fire"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

retire_amount <- 2.5*10^6
expected_return <- 0.04

df <- data.frame(
  age = seq(20, 65)
)

for(i in 1:nrow(df)){
  age <- df[i, "age"]
  df[i, "coast_number"] <- retire_amount/((1+expected_return)^(65-age))
}

to_plot <- df

file_path <- paste0(out_path, "/coast_fire_2p5_million_final.jpeg")
source_string <- paste0("Source: Simulated data (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Assumes retirement occurs at age 65, your money compounds at 4% per year after inflation, and ",
                                format_as_dollar(retire_amount), " is needed for retirement.")
                         , width = 80)

plot <- ggplot(to_plot, aes(x=age, y=coast_number)) +
  geom_bar(fill = chart_standard_color, stat = "identity") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Amount Needed to Reach Coast FIRE\nBy Age")) +
  labs(x="Current Age", y="Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #