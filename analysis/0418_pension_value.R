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
library(FinCal)
library(tidyverse)

folder_name <- "0418_pension_value"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

rates <- seq(0.02, 0.1, 0.01)

final_results <- data.frame()
counter <- 1
for(i in 1:length(rates)){
  # Calculate value of annuity at time of retirement
  pv_annuity <- pv(r = rates[i],
                   n = 30,
                   fv = 0,
                   pmt = -40000,
                   type = 1)
  
  final_results[i, "rate"] <- rates[i]
  final_results[i, "pv"] <- pv_annuity
  
  counter <- counter + 1
}

to_plot <- final_results

file_path <- paste0(out_path, "/pv_pension_30yr_40k.jpeg")
source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
note_string <-  str_wrap(paste0("Note: Assumes an annual payment of $40,000 for 30 years.")
                         , width = 85)

plot <- ggplot(to_plot, aes(x=rate, y=pv)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  scale_x_continuous(label = percent_format(accuracy = 1), breaks = rates) +
  scale_y_continuous(label = dollar, breaks = seq(0, 10^6, 10^5)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Pension Value by Discount Rate")) +
  labs(x="Discount Rate", y="Pension Value",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #