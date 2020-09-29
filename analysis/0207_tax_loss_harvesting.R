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

folder_name <- "0207_tax_loss_harvesting"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0207_tax_loss_harvesting/IEMG_VWO_data.csv"),
                col.names = c("date", "vwo", "iemg")) %>%
            mutate(date = as.Date(date)) %>%
            arrange(date) %>%
            mutate(ret_vwo = vwo/lag(vwo) - 1,
                   ret_iemg = iemg/lag(iemg) -1)

df <- raw

for(i in 1:nrow(df)){
  dt <- df[i, "date"]
  
  if(dt <= as.Date("2020-03-22")){
    df[i, "final_index"] <- df[i, "vwo"]
    df[i, "group"] <- 1
  } else{
    df[i, "final_index"] <- df[(i-1), "final_index"] * (1 + df[i, "ret_iemg"])
    df[i, "group"] <- 2
  }
}

file_path <- paste0(out_path, "/tax_loss_vwo_iemg_anno.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

to_plot <- df %>%
          select(date, group, final_index)

text_labels <- data.frame()

text_labels[1, "date"] <- as.Date("2020-02-01")
text_labels[2, "date"] <- as.Date("2020-08-27")

text_labels[1, "group"] <- 1
text_labels[2, "group"] <- 2

text_labels[1, "final_index"] <- 8000
text_labels[2, "final_index"] <- 9000

real_loss <- 10000-min(to_plot$final_index)
unreal_gain <- to_plot[nrow(to_plot), "final_index"] - min(to_plot$final_index)

text_labels[1, "label"] <- paste0(format_as_dollar(real_loss, 0), "\nRealized Loss\nin VWO")
text_labels[2, "label"] <- paste0(format_as_dollar(unreal_gain, 0), "\nUnrealized Gain\nin IEMG")

# Plot the results
plot <- ggplot(to_plot, aes(x = date, y = final_index, col = as.factor(group))) +
  geom_line(aes(group = 1)) +
  geom_text(data = text_labels, aes(x=date, y=final_index, col = as.factor(group), label = label)) +
  scale_color_manual(values = c("red", "green"), guide = FALSE) +
  scale_y_continuous(label = dollar, limits = c(6000, 11000)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Tax Loss Harvesting\nRealized Loss vs. Unrealized Gains")) +
  labs(x = "Date" , y = "Portfolio Value",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #