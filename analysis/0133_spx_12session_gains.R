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

folder_name <- "0133_dollar_losses"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read.csv(paste0(importdir, "/0132_spx/SPX_data.csv"),
                col.names = c("date", "index_sp500")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(ret_12sessions = index_sp500/lag(index_sp500, 12) - 1) %>%
  filter(!is.na(ret_12sessions))

# Plot dist
file_path <- paste0(out_path, "/returns_12sessions_spx.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Returns do not include dividends."), 
                          width = 85)

to_plot <- spx %>%
            select(date, ret_12sessions)

rets_over_6pct <- round(100*nrow(filter(to_plot, ret_12sessions > 0.06))/nrow(to_plot), digits=0)

print(mean(to_plot$ret_12sessions))

plot <- ggplot(to_plot, aes(x=ret_12sessions)) +
  geom_density(fill = chart_standard_color) +
  scale_x_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Distribution of Returns Over 12 Sessions\n1950-2019")) +
  labs(x="Return Over 12 Sessions", y="Frequency",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot again with annotations
file_path <- paste0(out_path, "/returns_12sessions_spx_annotated.jpeg")

text_labels <- data.frame()

text_labels[1, "ret_12sessions"] <- 0.15

plot <- ggplot(to_plot, aes(x=ret_12sessions)) +
  geom_density(fill = chart_standard_color) +
  geom_vline(xintercept = 0.06, linetype = "dashed", color = "red") +
  geom_text_repel(data=text_labels, aes(x=ret_12sessions, y=3),
                  label = "Just 3% of Periods\nHad A Return\nGreater Than 6%",
                  size = 3.5,
                  color = "red",
                  max.iter = 1,
                  family = "my_font") +
  scale_x_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("S&P 500 Distribution of Returns Over 12 Sessions\n1950-2019")) +
  labs(x="Return Over 12 Sessions", y="Frequency",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")




# ############################  End  ################################## #