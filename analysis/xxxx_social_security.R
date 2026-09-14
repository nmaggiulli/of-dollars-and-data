cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(gt)
library(tidyverse)

folder_name <- "xxxx_social_security"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)
########################## Start Program Here ######################### #

# 2026 bend points (for workers turning 62 in 2026)
bend1 <- 1286
bend2 <- 7749

calc_pia <- function(aime){
  0.90 * pmin(aime, bend1) +
    0.32 * pmax(pmin(aime, bend2) - bend1, 0) +
    0.15 * pmax(aime - bend2, 0)
}

ss_data <- tibble(aime = seq(0, 15000, by = 10)) %>%
  mutate(pia = calc_pia(aime),
         replacement_rate = ifelse(aime == 0, NA, pia / aime))

########################## Chart 1: The benefit formula ############### #

to_plot <- ss_data

file_path <- paste0(out_path, "/ss_bend_points_2026.jpeg")

source_string <- paste0("Source: Social Security Administration (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Bend points shown are for workers turning 62 in 2026. ",
                               "Benefit shown is the monthly amount at full retirement age (67)."),
                        width = 85)

plot <- ggplot(to_plot, aes(x = aime, y = pia)) +
  geom_vline(xintercept = c(bend1, bend2),
             linetype = "dashed", col = "gray60", linewidth = 0.3) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  scale_x_continuous(label = dollar) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Your Social Security Benefit Flattens\nAs Your Earnings Rise")) +
  labs(x = "Average Indexed Monthly Earnings (AIME)",
       y = "Monthly Benefit at Full Retirement Age",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

########################## Chart 2: Replacement rate ################## #

to_plot <- ss_data

file_path <- paste0(out_path, "/ss_replacement_rate_aime_2026.jpeg")
note_string <- str_wrap(paste0("Note: Bend points shown are for workers turning 62 in 2026. ",
                               "Benefit shown is the monthly amount at full retirement age (67), as a share of AIME."),
                        width = 85)

plot <- ggplot(to_plot, aes(x = aime, y = replacement_rate)) +
  geom_vline(xintercept = c(bend1, bend2),
             linetype = "dashed", col = "gray60", linewidth = 0.3) +
  geom_line(col = chart_standard_color, linewidth = 0.8) +
  scale_x_continuous(label = dollar) +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("The More You Earn,\nThe Less Social Security Replaces")) +
  labs(x = "Average Indexed Monthly Earnings (AIME)",
       y = "Benefit as a % of AIME",
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #