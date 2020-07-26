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

folder_name <- "0196_you_dont_need_alpha"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

rate_return <- 0.04
alpha <- 0.01
n_years <- 30
raw <- data.frame(year = seq(1, n_years, 1))

for(i in 1:nrow(raw)){
  raw[i, "ret_alpha"] <- ((1+rate_return+alpha)/(1+rate_return))^i - 1
}

to_plot <- raw

file_path <- paste0(out_path, "/alpha_sim_1pct.jpeg")
source_string <- paste0("Source:  Simulated data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Assumes the market returns ", 100*rate_return, "% after inflation.  Returns shown net of fees."))

plot <- ggplot(to_plot, aes(x=year, y=ret_alpha)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Cumulative Excess Return with ", 100*alpha,"% of Alpha")) +
  labs(x="Number of Years", y="Cumulative Excess Return",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #