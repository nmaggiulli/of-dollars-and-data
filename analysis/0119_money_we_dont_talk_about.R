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

folder_name <- "0119_quiet_money"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- data.frame(x=seq(1, 150))

for(i in 1:nrow(df)){
  x <- df[i, "x"]
  df[i, "y"] <- x*(x+1)/2
}

file_path <- paste0(out_path, "/sum_of_sequential_numbers.jpeg")

# Set source/note
source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Shows number of relationships as group size (x) increases."), width = 85)

plot <- ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle("Social Information Grows Exponentially") +
  labs(x = "Group Size" , y = "Number of Relationships",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #