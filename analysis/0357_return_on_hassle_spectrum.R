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

folder_name <- "0357_return_on_hassle_spectrum"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

df <- data.frame()

df[1, "difficulty"] <- 1
df[1, "return"] <- 0.04
df[1, "name"] <- "T-Bills"

df[2, "difficulty"] <- 2
df[2, "return"] <- 0.045
df[2, "name"] <- "U.S. Bonds"

df[3, "difficulty"] <- 3
df[3, "return"] <- 0.055
df[3, "name"] <- "Diversified Portfolio (i.e. 60/40)"

df[4, "difficulty"] <- 5
df[4, "return"] <- 0.07
df[4, "name"] <- "Passive Stock Fund"

df[5, "difficulty"] <- 6
df[5, "return"] <- 0.075
df[5, "name"] <- "Active Stock Fund"

df[6, "difficulty"] <- 7
df[6, "return"] <- 0.08
df[6, "name"] <- "Individual Stock Picking"

df[7, "difficulty"] <- 8
df[7, "return"] <- 0.1
df[7, "name"] <- "Real Estate Rentals"

df[8, "difficulty"] <- 10
df[8, "return"] <- 0.12
df[8, "name"] <- "Staring Your\nOwn Business"

to_plot <- df

text_labels <- to_plot %>%
                  mutate(difficulty = case_when(
                    difficulty == 1 ~ 1.7,
                    difficulty == 2 ~ 2.75,
                    difficulty == 3 ~ 4.9,
                    difficulty == 5 ~ 6.3,
                    difficulty == 6 ~ 7.2,
                    difficulty == 7 ~ 8.5,
                    difficulty == 8 ~ 9.2,
                    difficulty == 10 ~ 8.8,
                    TRUE ~ difficulty
                  ))

file_path <- paste0(out_path, "/return_on_hassle_spectrum_2023.jpeg")
source_string <- paste0("Source: Simulated data (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x = difficulty, y = return)) +
  geom_line() +
  geom_point() +
  geom_text(data = text_labels, aes(x= difficulty, y=return, label = name),
            family = my_font,
            size = 2.7) +
  scale_x_continuous(label = comma, breaks = seq(1, 10, 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  ggtitle("The Return on Hassle Spectrum") +
  of_dollars_and_data_theme +
  labs(x = "Difficulty/Hassle" , y = "Expected Annualized Return",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #