cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(ggrepel)
library(lubridate)
library(Quandl)
library(tidyverse)

folder_name <- "0095_change_in_perspective"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Create fake data for 3 investment options

df <- data.frame(period = seq(1, 20, 1),
                 `A` = c(1, 3.5, 3.7, 4.2, 5.6,
                         7, 7.5, 9, 9.4, 10,
                         9.2, 6.5, 7.5, 7.7, 6.4,
                         6, 5.2, 5.0, 4.5, 4),
                 `B` = c(1, 1.5, 1.4, 1.7, 2.2,
                               2.6, 3.2, 2.5, 2.6, 2.7,
                               3.4, 3.6, 3.7, 3.6, 3.4,
                               3.5, 3.8, 3.9, 3.9, 4),
                 `C` = c(1, 0.8, 0.7, 0.9, 0.65,
                               0.5, 0.4, 0.2, 0.3, 0.25,
                               0.7, 1.5, 1.4, 1.8, 2.2,
                               2.5, 3.2, 3.6, 3.8, 4)
)

to_plot <- df %>%
              gather(key= key, value = value, - period)

## Drawdown plot
file_path <- paste0(out_path, "/three_options.jpeg")

source_string <- "Source:  Simulated Data (OfDollarsAndData.com)"

# Create a filtered df for the text on the plot
filtered_period <- filter(to_plot, period == 9)

plot <- ggplot(to_plot, aes(x=period, y=value, col = key)) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=filter(filtered_period, key == "A"), aes(x=period, y=value, col = key),
                  label = filter(filtered_period, key == "A") %>% pull(key),
                  max.iter = 3000,
                  nudge_y = 0.5,
                  size = 6) +
  geom_text_repel(data=filter(filtered_period, key == "B"), aes(x=period, y=value, col = key),
                  label = filter(filtered_period, key == "B") %>% pull(key),
                  max.iter = 3000,
                  nudge_y = 0.5,
                  size = 6) +
  geom_text_repel(data=filter(filtered_period, key == "C"), aes(x=period, y=value, col = key),
                  label = filter(filtered_period, key == "C") %>% pull(key),
                  max.iter = 3000,
                  nudge_y = 0.3,
                  size = 6) +
  of_dollars_and_data_theme +
  ggtitle("The Same Result,\nBut Different Perspectives") +
  labs(x = "Period" , y = "Value",
       caption = paste0("\n", source_string))

# Turn plot into a gtable
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #