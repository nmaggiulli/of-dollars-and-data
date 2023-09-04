cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(dplyr)

folder_name <- "0362_road_not_taken"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

#Bring in Nike data
raw <- read.csv(paste0(importdir, "/0362_ycharts_nike/NKE_data.csv"), col.names = c("date", "price")) %>%
          mutate(date = as.Date(date)) %>%
          arrange(date)

start_stop <- raw %>%
                head(1) %>%
                bind_rows(raw %>% tail(1)) %>%
                mutate(type = "Imagined")

to_plot <- start_stop

file_path <- paste0(out_path, "/nike_imagined_1980.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x = date, y = price)) +
  geom_line(col = "green") +
  scale_y_continuous(label = dollar) +
  ggtitle("Nike Share Price (Imagined)\n1980-2023") +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Price",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot <- raw %>%
              mutate(type = "Actual") %>%
              bind_rows(start_stop)

file_path <- paste0(out_path, "/nike_actual_1980.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x = date, y = price, col = type)) +
  geom_line() +
  scale_color_manual(values = c("black", "green")) +
  scale_y_continuous(label = dollar) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Nike Share Price (Actual & Imagined)\n1980-2023") +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Price",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

dd <- drawdown_path(raw)

file_path <- paste0(out_path, "/nike_dd_1980.jpeg")
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

plot <- ggplot(data = dd, aes(x = date, y = pct)) +
  geom_area(fill = "red") +
  scale_y_continuous(label = percent_format(accuracy = 1), breaks = seq(0, -1, -0.1)) +
  ggtitle("Nike Drawdowns\n1980-2023") +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Percentage of Value Lost",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #